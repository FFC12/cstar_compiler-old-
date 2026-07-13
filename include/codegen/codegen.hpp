#ifndef CODEGEN_HPP
#define CODEGEN_HPP
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Target/TargetMachine.h>

#include <filesystem>
#include <map>
#include <parser/parser.hpp>
#include <string>
#include <visitor/visitor.hpp>

#define ENABLE_CODEGEN

enum class CStarEmitKind {
  IR,
  Assembly,
  Object,
  Executable,
};

struct CStarCodegenOptions {
  CStarEmitKind emitKind = CStarEmitKind::Executable;
  bool runAfterBuild = false;
  bool verbose = false;
  bool stats = false;
  std::filesystem::path outputDir;
};

class CStarCodegen {
  GlobalSymbolInfoList m_GlobalSymbols;
  LocalSymbolInfoList m_LocalSymbols;

  // instead of size_t more appropriate type
  // can be used for TypeInfo or something like that...
  // TODO: Will be done after soft-object oriented has impelemented.
  using DefinedTypeTable = std::map<std::string, size_t>;
  DefinedTypeTable m_DefinedTypes;

  CStarParser m_Parser;
  std::vector<ASTNode> m_AST;
  //  std::vector<llvm::Module> m_Modules;
  std::unique_ptr<llvm::Module> m_MainModule;
  std::unique_ptr<llvm::LLVMContext> m_MainContext;
  std::unique_ptr<llvm::IRBuilder<>> m_IRBuilder;
  std::unique_ptr<llvm::DataLayout> m_DataLayout;
  bool m_SemAnalysisFailure = false;
  size_t m_ErrorCount = 0;
  size_t m_WarningCount = 0;

  std::string m_Filename;
  std::filesystem::path m_OutputDir;
  CStarCodegenOptions m_Options;

  static std::string quoteCommandArg(const std::string& arg);
  static std::string resolveBackendClangPath();
  static std::string resolveTargetTriple();
  std::vector<std::string> backendTargetArgs() const;
  int runCommand(const std::vector<std::string>& args,
                 bool reportBackendFailure = true) const;

  // pass0 is for detecting and booking all symbols (gathering preinfo)
  // pass0.cpp
  void pass0();
  bool redefinitionCheck(SymbolInfoList& symbols, SymbolInfo& symbol);
  bool redefinitionCheck(SymbolInfoList& symbols, SymbolInfo& symbol,
                         size_t arr[3]);
  bool redefinitionCheck(SymbolInfo& symbol);
  void SemanticError(
      std::string message, SymbolInfo& symbolInfo,
      cstar::diagnostics::DiagnosticCode code =
          cstar::diagnostics::DiagnosticCode::SemanticError);
  void SemanticHint(std::string message, SymbolInfo& symbolInfo);

  // pass1 is for type checking
  void pass1();

  // codegen
  void codegen();

 public:
  explicit CStarCodegen(CStarParser&& parser, std::string& filepath,
                        CStarCodegenOptions options = {});
  ~CStarCodegen();

  void build();
  void printAST() const noexcept;
  void showStats(time_t startTime, const char* str);
};

#endif
