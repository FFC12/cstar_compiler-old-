#ifndef CSTAR_NATIVE_LINK_RESOLVER_HPP
#define CSTAR_NATIVE_LINK_RESOLVER_HPP

#include <filesystem>
#include <optional>
#include <string>
#include <vector>

struct NativeLinkResolution {
  std::vector<std::string> args;
  std::vector<std::filesystem::path> runtimeFiles;
  bool skipped = false;
  bool missing = false;
  std::string diagnostic;
};

class NativeLinkResolver {
 public:
  explicit NativeLinkResolver(std::string targetTriple = {});

  std::vector<std::string> defaultSearchArgs() const;
  NativeLinkResolution resolve(const std::string& library) const;
  bool requiresWindowsOpenGLLoader(const std::string& library) const;
  std::string nativeLinkArgument(const std::string& library) const;

 private:
  enum class Platform { Windows, Darwin, Unix };

  Platform m_Platform;
  std::string m_TargetTriple;

  static std::string normalizeLibraryName(std::string library);
  static std::vector<std::filesystem::path> parseSearchDirsEnv();

  std::vector<std::filesystem::path> searchDirs() const;
  std::vector<std::string> candidatesFor(const std::string& normalized) const;
  std::optional<std::filesystem::path> locateLibrary(
      const std::string& normalized) const;
  bool isVirtualRuntimeLibrary(const std::string& normalized) const;
  bool isDarwinFrameworkLibrary(const std::string& normalized) const;
  std::string canonicalLinkName(const std::string& normalized) const;
  std::vector<std::string> transitiveLinkArgs(
      const std::string& normalized,
      const std::filesystem::path& resolvedPath) const;
  std::vector<std::filesystem::path> runtimeFiles(
      const std::string& normalized,
      const std::filesystem::path& resolvedPath) const;
  std::string missingDiagnostic(const std::string& library,
                                const std::string& normalized) const;
};

#endif
