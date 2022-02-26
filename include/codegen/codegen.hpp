#ifndef CODEGEN_HPP
#define CODEGEN_HPP
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>

#include <map>
#include <parser/parser.hpp>
#include <visitor/visitor.hpp>

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
  bool m_SemAnalysisFailure = false;
  size_t m_ErrorCount = 0;
  size_t m_WarningCount = 0;

  // pass0 is for detecting and booking all symbols (gathering preinfo)
  // pass0.cpp
  void pass0();
  bool redefinitionCheck(SymbolInfoList& symbols, SymbolInfo& symbol);
  bool redefinitionCheck(SymbolInfoList& symbols, SymbolInfo& symbol,
                         size_t arr[3]);
  bool redefinitionCheck(SymbolInfo& symbol);
  void SemanticError(std::string message, SymbolInfo& symbolInfo);
  void SemanticHint(std::string message, SymbolInfo& symbolInfo);

  // pass1 is for type checking
  void pass1();

 public:
  explicit CStarCodegen(CStarParser&& parser, std::string& filepath)
      : m_Parser(std::move(parser)) {
    time_t startTime = time(nullptr);

    m_MainContext = std::make_unique<llvm::LLVMContext>();
    auto filename = ExtractFilenameFromPath(filepath, "/");
    m_MainModule = std::make_unique<llvm::Module>(filename, *m_MainContext);
    m_IRBuilder = std::make_unique<llvm::IRBuilder<>>(*m_MainContext);

    showStats(startTime, "LLVM Init");
  }

  void build() {
    this->m_Parser.parse();
    m_Parser.ownedAST(m_AST);

    time_t startTime = time(nullptr);
    pass0();
    showStats(startTime, "Pass 0 (Symbol Analysis)");

    /* if (m_SemAnalysisFailure) {
       std::cout << REDISH "Compilation failed. " << m_ErrorCount
                 << " error(s) generated.\n" RES;
       exit(1);
     }
     */

    startTime = time(nullptr);
    pass1();
    showStats(startTime, "Pass 1 (Type Checking)");
    if (m_SemAnalysisFailure) {
      std::cout << YEL
                << std::to_string(this->m_WarningCount) +
                       " warning(s) generated\n" RES;
      std::cout << REDISH "Compilation failed. " << m_ErrorCount
                << " error(s) generated.\n" RES;
      exit(1);
    }

    m_MainModule->print(llvm::errs(), nullptr);
  }

  void printAST() const noexcept {
    for (auto& node : m_AST) {
      node->debugNode();
    }
  }

  ~CStarCodegen() {
    // Need to release and delete it manually
    auto moduleRef = m_MainModule.release();
    moduleRef->dropAllReferences();
  }

  void showStats(time_t startTime, const char* str) {
    time_t endTime = time(nullptr);
    std::cout << GRN "======= " << str << " =======" RES << std::endl;
    double dif = difftime(endTime, startTime);
    printf("-  Elapsed time : %.2lf seconds\n\n", dif);
  }
};

#endif