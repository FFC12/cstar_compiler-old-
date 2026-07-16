#include <codegen/codegen.hpp>
#include <cstar_config.hpp>

#include <cstring>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <llvm/Support/raw_ostream.h>
#include <llvm/TargetParser/Triple.h>
#include <memory>
#include <parser/parser.hpp>
#include <set>
#include <sstream>
#include <string>
#ifndef _WIN32
#include <sys/wait.h>
#endif

std::unique_ptr<llvm::IRBuilder<>> Visitor::Builder;
std::unique_ptr<llvm::Module> Visitor::Module;

std::string CStarCodegen::quoteCommandArg(const std::string& arg) {
  if (arg.empty()) {
    return "\"\"";
  }

  const bool needsQuotes =
      arg.find_first_of(" \t\n\"&()[]{}^=;!'+,`~") != std::string::npos;
  if (!needsQuotes) {
    return arg;
  }

  std::string quoted = "\"";
  for (char c : arg) {
    if (c == '"') {
      quoted += "\\\"";
    } else {
      quoted += c;
    }
  }
  quoted += "\"";
  return quoted;
}

std::string CStarCodegen::resolveBackendClangPath() {
  if (const char* envClang = std::getenv("CSTAR_CLANG")) {
    if (envClang[0] != '\0') {
      return envClang;
    }
  }

  std::string configuredClang = CSTAR_BACKEND_CLANG_PATH;
  if (!configuredClang.empty()) {
    return configuredClang;
  }

  return "clang";
}

std::string CStarCodegen::resolveTargetTriple() {
  if (const char* envTriple = std::getenv("CSTAR_TARGET_TRIPLE")) {
    if (envTriple[0] != '\0') {
      return envTriple;
    }
  }

  return CSTAR_TARGET_TRIPLE;
}

std::string CStarCodegen::resolveArchiveToolPath() {
  if (const char* envAr = std::getenv("CSTAR_AR")) {
    if (envAr[0] != '\0') {
      return envAr;
    }
  }
  return "ar";
}

std::vector<std::string> CStarCodegen::backendTargetArgs() const {
  std::string targetTriple = resolveTargetTriple();
  if (targetTriple.empty()) {
    return {};
  }

  return {"-target", targetTriple};
}

static void appendDefaultNativeLibrarySearchPaths(std::vector<std::string>& args) {
#ifdef __APPLE__
  for (const char* path : {"/opt/homebrew/lib", "/usr/local/lib"}) {
    if (std::filesystem::is_directory(path)) {
      args.emplace_back(std::string("-L") + path);
    }
  }
#endif
}

static void appendNativeLinkArgument(std::vector<std::string>& args,
                                     const std::string& arg) {
  constexpr std::string_view kFrameworkPrefix = "-framework ";
  if (arg.rfind(std::string(kFrameworkPrefix), 0) == 0) {
    args.emplace_back("-framework");
    args.emplace_back(arg.substr(kFrameworkPrefix.size()));
    return;
  }

  args.push_back(arg);
}

static std::string normalizeNativeLibraryName(std::string library) {
  constexpr std::string_view kFrameworkPrefix = "-framework ";
  if (library.rfind(std::string(kFrameworkPrefix), 0) == 0) {
    return library.substr(kFrameworkPrefix.size());
  }

  if (library.rfind("-l", 0) == 0 && library.size() > 2) {
    return library.substr(2);
  }

  return library;
}

static bool isDarwinFrameworkLibrary(const std::string& library) {
#ifdef __APPLE__
  static const std::set<std::string> kFrameworks = {
      "Accelerate", "AppKit",       "AudioToolbox", "AVFoundation",
      "Carbon",     "Cocoa",        "CoreAudio",    "CoreFoundation",
      "CoreGraphics", "CoreMedia",  "CoreVideo",    "Foundation",
      "IOKit",      "Metal",        "OpenAL",       "OpenCL",
      "OpenGL",     "QuartzCore",   "Security",     "SystemConfiguration"};
  return kFrameworks.count(library) > 0;
#else
  (void)library;
  return false;
#endif
}

std::string CStarCodegen::nativeLinkArgument(const std::string& library) {
  const auto normalized = normalizeNativeLibraryName(library);
  if (normalized.empty()) {
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

#ifdef _WIN32
  if (normalized == "OpenGL") {
    return "opengl32.lib";
  }
  return normalized + ".lib";
#else
  return "-l" + normalized;
#endif
}

int CStarCodegen::runCommand(const std::vector<std::string>& args,
                             bool reportBackendFailure) const {
  std::ostringstream command;
  for (size_t i = 0; i < args.size(); ++i) {
    if (i > 0) {
      command << ' ';
    }
    command << quoteCommandArg(args[i]);
  }

  auto commandString = command.str();
  if (m_Options.verbose) {
    std::cout << CYN << "$ " << commandString << RES << std::endl;
  }
  std::cout.flush();
  std::cerr.flush();
  int result = std::system(commandString.c_str());
  int exitCode = result;
#ifndef _WIN32
  if (result != -1) {
    if (WIFEXITED(result)) {
      exitCode = WEXITSTATUS(result);
    } else if (WIFSIGNALED(result)) {
      exitCode = 128 + WTERMSIG(result);
    }
  }
#endif
  if (exitCode != 0 && reportBackendFailure) {
    std::cerr << REDISH << "Backend command failed with code " << exitCode << RES
              << std::endl;
  }
  return exitCode;
}

void CStarCodegen::appendIncludedSource(
    const std::filesystem::path& includePath,
    std::set<std::filesystem::path>& seen) {
  auto resolved = includePath;
  if (resolved.is_relative()) {
    if (resolved.begin() != resolved.end() && *resolved.begin() == "std") {
      resolved = std::filesystem::current_path() / resolved;
    } else {
      resolved = m_InputPath.parent_path() / resolved;
    }
  }
  resolved = std::filesystem::absolute(resolved).lexically_normal();

  if (seen.count(resolved) > 0) {
    return;
  }
  seen.insert(resolved);

  std::ifstream file(resolved);
  if (!file.is_open()) {
    std::cerr << REDISH << "Cannot open included C* file: "
              << resolved.string() << RES << std::endl;
    exit(1);
  }

  std::stringstream ss;
  ss << file.rdbuf();

  auto realPathStr = resolved.string();
  auto realPath = static_cast<char*>(std::malloc(realPathStr.size() + 1));
  std::memcpy(realPath, realPathStr.c_str(), realPathStr.size() + 1);
  const std::shared_ptr<char> realPathPtr(realPath, std::free);

  CStarLexer lexer(ss.str(), realPathPtr);
  CStarParser parser(std::move(lexer), false);
  parser.parse();

  for (const auto& library : parser.nativeLinkLibraries()) {
    if (std::find(m_NativeLinkLibraries.begin(), m_NativeLinkLibraries.end(),
                  library) == m_NativeLinkLibraries.end()) {
      m_NativeLinkLibraries.push_back(library);
    }
  }
  for (const auto& alias : parser.moduleAliases()) {
    if (std::find(m_ModuleAliases.begin(), m_ModuleAliases.end(), alias) ==
        m_ModuleAliases.end()) {
      m_ModuleAliases.push_back(alias);
    }
  }

  for (const auto& nestedInclude : parser.sourceIncludes()) {
    appendIncludedSource(resolved.parent_path() / nestedInclude, seen);
  }

  std::vector<ASTNode> includedAST;
  parser.ownedAST(includedAST);
  for (auto& node : includedAST) {
    if (node == nullptr || node->getASTKind() != ASTKind::Decl) {
      continue;
    }

    const auto declKind = node->getDeclKind();
    if (node->isPublicDecl() || declKind == DeclKind::ImportFuncDecl ||
        declKind == DeclKind::ImportVarDecl) {
      node->markFromIncludedSource();
      m_AST.push_back(std::move(node));
    }
  }
}

CStarCodegen::CStarCodegen(CStarParser&& parser, std::string& filepath,
                           CStarCodegenOptions options)
    : m_Parser(std::move(parser)), m_Options(std::move(options)) {
  time_t startTime = time(nullptr);

  m_MainContext = std::make_unique<llvm::LLVMContext>();
  m_InputPath = std::filesystem::absolute(filepath);
  m_Filename = ExtractFilenameFromPath(filepath, "/");
  if (!m_Options.outputDir.empty()) {
    m_OutputDir = m_Options.outputDir;
  } else {
    if (const char* outputDir = std::getenv("CSTAR_OUTPUT_DIR")) {
      if (outputDir[0] != '\0') {
        m_OutputDir = outputDir;
      }
    }
  }
  if (m_OutputDir.empty()) {
    m_OutputDir = std::filesystem::current_path() / ".cstar-out";
  }
  std::filesystem::create_directories(m_OutputDir);

  m_MainModule = std::make_unique<llvm::Module>(m_Filename, *m_MainContext);
  if (std::string targetTriple = resolveTargetTriple(); !targetTriple.empty()) {
    m_MainModule->setTargetTriple(llvm::Triple(targetTriple));
  }
  if (std::string dataLayout = CSTAR_TARGET_DATA_LAYOUT; !dataLayout.empty()) {
    m_MainModule->setDataLayout(dataLayout);
  }
  m_IRBuilder = std::make_unique<llvm::IRBuilder<>>(*m_MainContext);

  showStats(startTime, "LLVM Init");
}

CStarCodegen::~CStarCodegen() {
  auto moduleRef = Visitor::Module.release();
  if (moduleRef != nullptr) {
    moduleRef->dropAllReferences();
  }
}

void CStarCodegen::build() {
  this->m_Parser.parse();
  m_NativeLinkLibraries = m_Parser.nativeLinkLibraries();
  m_ModuleAliases = m_Parser.moduleAliases();
  auto sourceIncludes = m_Parser.sourceIncludes();
  m_Parser.ownedAST(m_AST);
  const auto primaryASTCount = m_AST.size();
  std::set<std::filesystem::path> seenIncludes;
  seenIncludes.insert(m_InputPath);
  for (const auto& includePath : sourceIncludes) {
    appendIncludedSource(includePath, seenIncludes);
  }
  Visitor::ModuleAliases.clear();
  Visitor::ModuleAliases.insert(m_ModuleAliases.begin(), m_ModuleAliases.end());

  bool hasExportedDecl = false;
  SemanticLoc exportLoc(0, 0, 0);
  for (size_t i = 0; i < primaryASTCount; ++i) {
    const auto& ast = m_AST[i];
    if (ast->getASTKind() == ASTKind::Decl &&
        (ast->getDeclKind() == DeclKind::ExportFuncDecl ||
         ast->getDeclKind() == DeclKind::ExportVarDecl)) {
      hasExportedDecl = true;
      exportLoc = ast->getSemLoc();
      break;
    }
  }
  if (hasExportedDecl && m_Options.emitKind == CStarEmitKind::Executable) {
    m_Parser.ParserWarning(
        "`export` marks library ABI visibility, but this build emits an "
        "executable. Use --emit=staticlib or --emit=dynamiclib when exporting "
        "symbols for another module.",
        exportLoc.begin, exportLoc.end, exportLoc.line);
    m_WarningCount += 1;
  }

  auto exitOnSemanticFailure = [&]() {
    if (!m_SemAnalysisFailure) {
      return;
    }

    std::cout << YEL << std::to_string(this->m_WarningCount)
              << " warning(s) generated\n" << RES;
    std::cout << REDISH << "Compilation failed. " << m_ErrorCount
              << " error(s) generated.\n" << RES;
    exit(1);
  };

  time_t startTime = time(nullptr);
  pass0();
  showStats(startTime, "Pass 0 (Symbol Analysis)");
  exitOnSemanticFailure();

  startTime = time(nullptr);
  pass1();
  showStats(startTime, "Pass 1 (Type Checking)");
  exitOnSemanticFailure();

  Visitor::Builder = std::move(this->m_IRBuilder);
  Visitor::Module = std::move(this->m_MainModule);

#ifdef ENABLE_CODEGEN
  codegen();

  std::string outstr;
  llvm::raw_string_ostream oss(outstr);
  Visitor::Module->print(oss, nullptr);
  const auto irPath = m_OutputDir / (m_Filename + ".ll");
  const auto asmPath = m_OutputDir / (m_Filename + ".s");
#ifdef _WIN32
  const auto objPath = m_OutputDir / (m_Filename + ".obj");
  const auto exePath = m_OutputDir / (m_Filename + ".exe");
  const auto staticLibPath = m_OutputDir / ("lib" + m_Filename + ".lib");
  const auto dynamicLibPath = m_OutputDir / (m_Filename + ".dll");
#else
  const auto objPath = m_OutputDir / (m_Filename + ".o");
  const auto exePath = m_OutputDir / (m_Filename + ".out");
  const auto staticLibPath = m_OutputDir / ("lib" + m_Filename + ".a");
#ifdef __APPLE__
  const auto dynamicLibPath = m_OutputDir / ("lib" + m_Filename + ".dylib");
#else
  const auto dynamicLibPath = m_OutputDir / ("lib" + m_Filename + ".so");
#endif
#endif

  std::ofstream outfile(irPath);
  outfile << outstr << std::endl;
  outfile.close();

  const std::string clangPath = resolveBackendClangPath();
  const auto irFilename = irPath.string();
  const auto asmFilename = asmPath.string();
  const auto objFilename = objPath.string();
  const auto exeFilename = exePath.string();
  const auto staticLibFilename = staticLibPath.string();
  const auto dynamicLibFilename = dynamicLibPath.string();
  auto targetArgs = backendTargetArgs();

  if (m_Options.emitKind == CStarEmitKind::IR) {
    if (m_Options.stats || !m_Options.runAfterBuild) {
      std::cout << GRN << "Output: " << irFilename << RES << std::endl;
    }
    showStats(startTime, "Pass 2 (Codegen)");
    return;
  }

  std::vector<std::string> emitAssemblyArgs{clangPath};
  emitAssemblyArgs.insert(emitAssemblyArgs.end(), targetArgs.begin(),
                          targetArgs.end());
  emitAssemblyArgs.insert(emitAssemblyArgs.end(),
                          {"-S", "-x", "ir", irFilename, "-o", asmFilename});
  if (runCommand(emitAssemblyArgs) != 0) {
    exit(1);
  }

  if (m_Options.emitKind == CStarEmitKind::Assembly) {
    if (m_Options.stats || !m_Options.runAfterBuild) {
      std::cout << GRN << "Output: " << asmFilename << RES << std::endl;
    }
    showStats(startTime, "Pass 2 (Codegen)");
    return;
  }

  auto emitObject = [&]() {
    std::vector<std::string> emitObjectArgs{clangPath};
    emitObjectArgs.insert(emitObjectArgs.end(), targetArgs.begin(),
                          targetArgs.end());
    emitObjectArgs.insert(emitObjectArgs.end(),
                          {"-c", "-x", "ir", irFilename, "-o", objFilename});
    if (runCommand(emitObjectArgs) != 0) {
      exit(1);
    }
  };

  if (m_Options.emitKind == CStarEmitKind::Object) {
    emitObject();
    if (m_Options.stats || !m_Options.runAfterBuild) {
      std::cout << GRN << "Output: " << objFilename << RES << std::endl;
    }
    showStats(startTime, "Pass 2 (Codegen)");
    return;
  }

  if (m_Options.emitKind == CStarEmitKind::StaticLibrary) {
    emitObject();
    if (runCommand({resolveArchiveToolPath(), "rcs", staticLibFilename,
                    objFilename}) != 0) {
      exit(1);
    }
    if (m_Options.stats || !m_Options.runAfterBuild) {
      std::cout << GRN << "Output: " << staticLibFilename << RES << std::endl;
    }
    showStats(startTime, "Pass 2 (Codegen)");
    return;
  }

  if (m_Options.emitKind == CStarEmitKind::DynamicLibrary) {
    std::vector<std::string> dynamicArgs{clangPath};
    dynamicArgs.insert(dynamicArgs.end(), targetArgs.begin(), targetArgs.end());
    dynamicArgs.insert(dynamicArgs.end(),
                       {"-shared", irFilename, "-o", dynamicLibFilename});
    appendDefaultNativeLibrarySearchPaths(dynamicArgs);
    for (const auto& library : m_NativeLinkLibraries) {
      auto arg = nativeLinkArgument(library);
      if (!arg.empty()) {
        appendNativeLinkArgument(dynamicArgs, arg);
      }
    }
    if (runCommand(dynamicArgs) != 0) {
      exit(1);
    }
    if (m_Options.stats || !m_Options.runAfterBuild) {
      std::cout << GRN << "Output: " << dynamicLibFilename << RES << std::endl;
    }
    showStats(startTime, "Pass 2 (Codegen)");
    return;
  }

  std::vector<std::string> linkArgs{clangPath};
  linkArgs.insert(linkArgs.end(), targetArgs.begin(), targetArgs.end());
  linkArgs.insert(linkArgs.end(), {irFilename, "-o", exeFilename});
  appendDefaultNativeLibrarySearchPaths(linkArgs);
  for (const auto& library : m_NativeLinkLibraries) {
    auto arg = nativeLinkArgument(library);
    if (!arg.empty()) {
      appendNativeLinkArgument(linkArgs, arg);
    }
  }
  if (runCommand(linkArgs) != 0) {
    exit(1);
  }

  if (m_Options.stats || !m_Options.runAfterBuild) {
    std::cout << GRN << "Output: " << exeFilename << RES << std::endl;
  }
  if (m_Options.runAfterBuild) {
    int programExitCode =
        runCommand({std::filesystem::absolute(exePath).string()}, false);
    if (m_Options.stats) {
      std::cout << YEL << "Generated program exited with code "
                << programExitCode << RES << std::endl;
    }
  }
  showStats(startTime, "Pass 2 (Codegen)");
#endif
}

void CStarCodegen::printAST() const noexcept {
  for (auto& node : m_AST) {
    node->debugNode();
  }
}

void CStarCodegen::showStats(time_t startTime, const char* str) {
  if (!m_Options.stats) {
    return;
  }

  time_t endTime = time(nullptr);
  std::cout << GRN << "======= " << str << " =======" << RES << std::endl;
  double dif = difftime(endTime, startTime);
  printf("-  Elapsed time : %.2lf seconds\n\n", dif);
}

void CStarCodegen::codegen() {
  Visitor visitor(this->m_DefinedTypes);

  for (auto& ast : m_AST) {
    if (ast->getASTKind() == ASTKind::Decl &&
        ast->getDeclKind() == DeclKind::StructDecl) {
      ast->accept(visitor);
    }
  }

  for (auto& ast : m_AST) {
    if (ast->getASTKind() == ASTKind::Decl &&
        (ast->getDeclKind() == DeclKind::FuncDecl ||
         ast->getDeclKind() == DeclKind::ImportFuncDecl ||
         ast->getDeclKind() == DeclKind::ExportFuncDecl)) {
      auto* funcAst = dynamic_cast<FuncAST*>(ast.get());
      if (funcAst != nullptr) {
        visitor.declareFunction(*funcAst);
      }
    }
  }

  for (auto& ast : m_AST) {
    if (ast->getASTKind() == ASTKind::Decl) {
      if (ast->getDeclKind() == DeclKind::FuncDecl ||
          ast->getDeclKind() == DeclKind::ExportFuncDecl) {
        auto* function =
            reinterpret_cast<llvm::Function*>(ast->accept(visitor));
        llvm::verifyFunction(*function);

      } else if (ast->getDeclKind() == DeclKind::VarDecl ||
                 ast->getDeclKind() == DeclKind::ImportVarDecl ||
                 ast->getDeclKind() == DeclKind::GlobVarDecl ||
                 ast->getDeclKind() == DeclKind::ExportVarDecl) {
        auto variable = ast->accept(visitor);
      }
    }
  }
  visitor.finalizeCodegen();
}
