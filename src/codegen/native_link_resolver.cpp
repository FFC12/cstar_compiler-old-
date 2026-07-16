#include <codegen/native_link_resolver.hpp>

#include <cstdlib>
#include <set>
#include <sstream>
#include <string_view>

NativeLinkResolver::NativeLinkResolver(std::string targetTriple)
    : m_TargetTriple(std::move(targetTriple)) {
  const auto triple = m_TargetTriple;
  if (triple.find("windows") != std::string::npos ||
      triple.find("mingw") != std::string::npos ||
      triple.find("msvc") != std::string::npos) {
    m_Platform = Platform::Windows;
  } else if (triple.find("darwin") != std::string::npos ||
             triple.find("apple") != std::string::npos) {
    m_Platform = Platform::Darwin;
  } else {
#ifdef _WIN32
    m_Platform = Platform::Windows;
#elif defined(__APPLE__)
    m_Platform = Platform::Darwin;
#else
    m_Platform = Platform::Unix;
#endif
  }
}

std::string NativeLinkResolver::normalizeLibraryName(std::string library) {
  constexpr std::string_view kFrameworkPrefix = "-framework ";
  if (library.rfind(std::string(kFrameworkPrefix), 0) == 0) {
    return library.substr(kFrameworkPrefix.size());
  }

  if (library.rfind("-l", 0) == 0 && library.size() > 2) {
    return library.substr(2);
  }

  return library;
}

std::vector<std::filesystem::path> NativeLinkResolver::parseSearchDirsEnv() {
  std::vector<std::filesystem::path> dirs;
  const char* raw = std::getenv("CSTAR_NATIVE_LIBRARY_DIRS");
  if (raw == nullptr || raw[0] == '\0') {
    return dirs;
  }

#ifdef _WIN32
  constexpr char separator = ';';
#else
  constexpr char separator = ':';
#endif

  std::string current;
  for (char ch : std::string(raw)) {
    if (ch == separator) {
      if (!current.empty()) {
        dirs.emplace_back(current);
        current.clear();
      }
    } else {
      current.push_back(ch);
    }
  }
  if (!current.empty()) {
    dirs.emplace_back(current);
  }
  return dirs;
}

std::vector<std::filesystem::path> NativeLinkResolver::searchDirs() const {
  auto dirs = parseSearchDirsEnv();

  std::vector<std::filesystem::path> defaults;
  switch (m_Platform) {
    case Platform::Windows:
      defaults = {"C:/msys64/ucrt64/lib",
                  "C:/msys64/mingw64/lib",
                  "C:/msys64/usr/lib",
                  "C:/vcpkg/installed/x64-mingw-dynamic/lib",
                  "C:/vcpkg/installed/x64-mingw-static/lib",
                  "C:/vcpkg/installed/x64-windows/lib",
                  "C:/vcpkg/installed/x64-windows-static/lib"};
      break;
    case Platform::Darwin:
      defaults = {"/opt/homebrew/lib", "/usr/local/lib",
                  "/opt/homebrew/opt/glfw/lib", "/usr/local/opt/glfw/lib"};
      break;
    case Platform::Unix:
      defaults = {"/usr/lib", "/usr/local/lib", "/usr/lib64",
                  "/usr/local/lib64"};
      break;
  }

  dirs.insert(dirs.end(), defaults.begin(), defaults.end());
  return dirs;
}

std::vector<std::string> NativeLinkResolver::defaultSearchArgs() const {
  std::vector<std::string> args;
  for (const auto& dir : searchDirs()) {
    if (!dir.empty() && std::filesystem::is_directory(dir)) {
      args.emplace_back("-L" + dir.string());
    }
  }
  return args;
}

bool NativeLinkResolver::isVirtualRuntimeLibrary(
    const std::string& normalized) const {
  return normalized.rfind("std:", 0) == 0;
}

bool NativeLinkResolver::isDarwinFrameworkLibrary(
    const std::string& normalized) const {
  if (m_Platform != Platform::Darwin) {
    return false;
  }

  static const std::set<std::string> kFrameworks = {
      "Accelerate", "AppKit",       "AudioToolbox", "AVFoundation",
      "Carbon",     "Cocoa",        "CoreAudio",    "CoreFoundation",
      "CoreGraphics", "CoreMedia",  "CoreVideo",    "Foundation",
      "IOKit",      "Metal",        "OpenAL",       "OpenCL",
      "OpenGL",     "QuartzCore",   "Security",     "SystemConfiguration"};
  return kFrameworks.count(normalized) > 0;
}

std::string NativeLinkResolver::canonicalLinkName(
    const std::string& normalized) const {
  if (normalized == "OpenGL" || normalized == "opengl" ||
      normalized == "opengl32") {
    return m_Platform == Platform::Windows ? "opengl32" : "OpenGL";
  }

  if (m_Platform == Platform::Windows &&
      (normalized == "glfw" || normalized == "glfw3")) {
    return "glfw3";
  }

  return normalized;
}

std::vector<std::string> NativeLinkResolver::candidatesFor(
    const std::string& normalized) const {
  const auto canonical = canonicalLinkName(normalized);

  switch (m_Platform) {
    case Platform::Windows:
      if (canonical == "opengl32") {
        return {"opengl32.lib", "libopengl32.a"};
      }
      if (canonical == "glfw3") {
        return {"libglfw3.dll.a", "glfw3dll.lib", "glfw3.lib",
                "libglfw3.a"};
      }
      return {canonical + ".lib", "lib" + canonical + ".a"};
    case Platform::Darwin:
      return {"lib" + canonical + ".dylib", "lib" + canonical + ".a",
              canonical + ".dylib", canonical + ".a"};
    case Platform::Unix:
      return {"lib" + canonical + ".so", "lib" + canonical + ".a",
              canonical + ".so", canonical + ".a"};
  }

  return {};
}

std::optional<std::filesystem::path> NativeLinkResolver::locateLibrary(
    const std::string& normalized) const {
  if (normalized.find('/') != std::string::npos ||
      normalized.find('\\') != std::string::npos) {
    std::filesystem::path direct(normalized);
    if (std::filesystem::exists(direct)) {
      return std::filesystem::absolute(direct).lexically_normal();
    }
  }

  if (std::filesystem::exists(normalized)) {
    return std::filesystem::absolute(normalized).lexically_normal();
  }

  for (const auto& dir : searchDirs()) {
    if (dir.empty() || !std::filesystem::is_directory(dir)) {
      continue;
    }
    for (const auto& candidate : candidatesFor(normalized)) {
      auto path = dir / candidate;
      if (std::filesystem::exists(path)) {
        return std::filesystem::absolute(path).lexically_normal();
      }
    }
  }

  return std::nullopt;
}

std::string NativeLinkResolver::nativeLinkArgument(
    const std::string& library) const {
  const auto normalized = normalizeLibraryName(library);
  if (normalized.empty() || isVirtualRuntimeLibrary(normalized)) {
    return {};
  }

  if (normalized.rfind("-L", 0) == 0 || normalized.rfind("-F", 0) == 0 ||
      normalized.rfind("-Wl,", 0) == 0) {
    return normalized;
  }

  const auto path = std::filesystem::path(normalized);
  const auto ext = path.extension().string();
  if (normalized.find('/') != std::string::npos ||
      normalized.find('\\') != std::string::npos || ext == ".a" ||
      ext == ".so" || ext == ".dylib" || ext == ".lib") {
    return normalized;
  }

  if (normalized.find(':') != std::string::npos) {
    return {};
  }

  if (isDarwinFrameworkLibrary(normalized)) {
    return "-framework " + normalized;
  }

  return "-l" + canonicalLinkName(normalized);
}

bool NativeLinkResolver::requiresWindowsOpenGLLoader(
    const std::string& library) const {
  if (m_Platform != Platform::Windows) {
    return false;
  }

  const auto normalized = normalizeLibraryName(library);
  return normalized == "OpenGL" || normalized == "opengl" ||
         normalized == "opengl32";
}

std::vector<std::string> NativeLinkResolver::transitiveLinkArgs(
    const std::string& normalized,
    const std::filesystem::path& resolvedPath) const {
  if (m_Platform != Platform::Windows) {
    return {};
  }

  const auto canonical = canonicalLinkName(normalized);
  const auto filename = resolvedPath.filename().string();
  if (canonical != "glfw3" || filename.find(".dll.") != std::string::npos ||
      filename.find("dll") != std::string::npos) {
    return {};
  }

  return {"-lopengl32", "-lgdi32", "-luser32", "-lshell32"};
}

std::vector<std::filesystem::path> NativeLinkResolver::runtimeFiles(
    const std::string& normalized,
    const std::filesystem::path& resolvedPath) const {
  if (m_Platform != Platform::Windows) {
    return {};
  }

  const auto canonical = canonicalLinkName(normalized);
  const auto filename = resolvedPath.filename().string();
  if (canonical != "glfw3" || filename.find("dll") == std::string::npos) {
    return {};
  }

  std::vector<std::filesystem::path> candidates = {
      resolvedPath.parent_path() / "glfw3.dll",
      resolvedPath.parent_path().parent_path() / "bin" / "glfw3.dll"};
  for (const auto& candidate : candidates) {
    if (std::filesystem::exists(candidate)) {
      return {std::filesystem::absolute(candidate).lexically_normal()};
    }
  }
  return {};
}

std::string NativeLinkResolver::missingDiagnostic(
    const std::string& library, const std::string& normalized) const {
  std::ostringstream message;
  message << "Missing native library for import `" << library << "`.";

  if (m_Platform == Platform::Windows &&
      (normalized == "glfw" || normalized == "glfw3")) {
    message << " Install MSYS2 package `mingw-w64-ucrt-x86_64-glfw` or set "
               "CSTAR_NATIVE_LIBRARY_DIRS to the directory containing "
               "libglfw3.a/glfw3.lib.";
  } else {
    message << " Set CSTAR_NATIVE_LIBRARY_DIRS or install the matching "
               "development package.";
  }

  return message.str();
}

NativeLinkResolution NativeLinkResolver::resolve(
    const std::string& library) const {
  NativeLinkResolution result;
  const auto normalized = normalizeLibraryName(library);
  if (normalized.empty() || isVirtualRuntimeLibrary(normalized)) {
    result.skipped = true;
    return result;
  }

  if (normalized.rfind("-L", 0) == 0 || normalized.rfind("-F", 0) == 0 ||
      normalized.rfind("-Wl,", 0) == 0) {
    result.args.push_back(normalized);
    return result;
  }

  if (isDarwinFrameworkLibrary(normalized)) {
    result.args.push_back("-framework");
    result.args.push_back(normalized);
    return result;
  }

  const auto path = std::filesystem::path(normalized);
  const auto ext = path.extension().string();
  if (normalized.find('/') != std::string::npos ||
      normalized.find('\\') != std::string::npos || ext == ".a" ||
      ext == ".so" || ext == ".dylib" || ext == ".lib") {
    if (auto located = locateLibrary(normalized)) {
      result.args.push_back(located->string());
      auto transitiveArgs = transitiveLinkArgs(normalized, *located);
      result.args.insert(result.args.end(), transitiveArgs.begin(),
                         transitiveArgs.end());
      result.runtimeFiles = runtimeFiles(normalized, *located);
      return result;
    }
    result.missing = true;
    result.diagnostic = missingDiagnostic(library, normalized);
    return result;
  }

  if (auto located = locateLibrary(normalized)) {
    result.args.push_back(located->string());
    auto transitiveArgs = transitiveLinkArgs(normalized, *located);
    result.args.insert(result.args.end(), transitiveArgs.begin(),
                       transitiveArgs.end());
    result.runtimeFiles = runtimeFiles(normalized, *located);
    return result;
  }

  if (m_Platform == Platform::Windows &&
      (normalized == "glfw" || normalized == "glfw3")) {
    result.missing = true;
    result.diagnostic = missingDiagnostic(library, normalized);
    return result;
  }

  const auto arg = nativeLinkArgument(normalized);
  if (!arg.empty()) {
    result.args.push_back(arg);
    return result;
  }

  result.missing = true;
  result.diagnostic = missingDiagnostic(library, normalized);
  return result;
}
