#include <codegen/codegen.hpp>
#include <cstar_config.hpp>

#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <llvm/Support/raw_ostream.h>
#include <llvm/TargetParser/Triple.h>
#include <sstream>
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

std::vector<std::string> CStarCodegen::backendTargetArgs() const {
  std::string targetTriple = resolveTargetTriple();
  if (targetTriple.empty()) {
    return {};
  }

  return {"-target", targetTriple};
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

CStarCodegen::CStarCodegen(CStarParser&& parser, std::string& filepath,
                           CStarCodegenOptions options)
    : m_Parser(std::move(parser)), m_Options(std::move(options)) {
  time_t startTime = time(nullptr);

  m_MainContext = std::make_unique<llvm::LLVMContext>();
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
  m_Parser.ownedAST(m_AST);

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
#else
  const auto objPath = m_OutputDir / (m_Filename + ".o");
  const auto exePath = m_OutputDir / (m_Filename + ".out");
#endif

  std::ofstream outfile(irPath);
  outfile << outstr << std::endl;
  outfile.close();

  const std::string clangPath = resolveBackendClangPath();
  const auto irFilename = irPath.string();
  const auto asmFilename = asmPath.string();
  const auto objFilename = objPath.string();
  const auto exeFilename = exePath.string();
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

  if (m_Options.emitKind == CStarEmitKind::Object) {
    std::vector<std::string> emitObjectArgs{clangPath};
    emitObjectArgs.insert(emitObjectArgs.end(), targetArgs.begin(),
                          targetArgs.end());
    emitObjectArgs.insert(emitObjectArgs.end(),
                          {"-c", "-x", "ir", irFilename, "-o", objFilename});
    if (runCommand(emitObjectArgs) != 0) {
      exit(1);
    }

    if (m_Options.stats || !m_Options.runAfterBuild) {
      std::cout << GRN << "Output: " << objFilename << RES << std::endl;
    }
    showStats(startTime, "Pass 2 (Codegen)");
    return;
  }

  std::vector<std::string> linkArgs{clangPath};
  linkArgs.insert(linkArgs.end(), targetArgs.begin(), targetArgs.end());
  linkArgs.insert(linkArgs.end(), {irFilename, "-o", exeFilename});
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
