#ifndef CODEGEN_HPP
#define CODEGEN_HPP
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>

#include <map>
#include <parser/parser.hpp>
#include <visitor/visitor.hpp>

class CStarCodegen {
  using SymbolInfoList = std::multimap<std::string, SymbolInfo>;
  using LocalSymbolInfoList = std::multimap<std::string, SymbolInfoList>;
  using GlobalSymbolInfoList = SymbolInfoList;

  GlobalSymbolInfoList m_GlobalSymbols;
  LocalSymbolInfoList m_LocalSymbols;

  CStarParser m_Parser;
  std::vector<ASTNode> m_AST;
  //  std::vector<llvm::Module> m_Modules;
  std::unique_ptr<llvm::Module> m_MainModule;
  std::unique_ptr<llvm::LLVMContext> m_MainContext;
  std::unique_ptr<llvm::IRBuilder<>> m_IRBuilder;
  bool m_SemAnalysisFailure = false;
  size_t m_ErrorCount = 0;

 public:
  explicit CStarCodegen(CStarParser&& parser, std::string& filepath)
      : m_Parser(std::move(parser)) {
    time_t startTime = time(nullptr);

    m_MainContext = std::make_unique<llvm::LLVMContext>();
    auto filename = ExtractFilenameFromPath(filepath, "/");
    m_MainModule = std::make_unique<llvm::Module>(filename, *m_MainContext);
    m_IRBuilder = std::make_unique<llvm::IRBuilder<>>(*m_MainContext);

    time_t endTime = time(nullptr);
    std::cout << GRN "======= LLVM Init =======" RES << std::endl;
    double dif = difftime(endTime, startTime);
    printf("-  Elapsed time : %.2lf seconds\n\n", dif);
  }

  void build() {
    this->m_Parser.parse();
    m_Parser.ownedAST(m_AST);

    time_t startTime = time(nullptr);
    pass0();
    time_t endTime = time(nullptr);
    std::cout << GRN "======= Pass 0 (Symbol Analysis) =======" RES << std::endl;
    double dif = difftime(endTime, startTime);
    printf("-  Elapsed time : %.2lf seconds\n\n", dif);

    if(m_SemAnalysisFailure) {
      std::cout << REDISH "Compilation failed. "<< m_ErrorCount << " error(s) generated.\n" RES;
      exit(1);
    }

    m_MainModule->print(llvm::errs(), nullptr);
  }

  void printAST() const noexcept {
    for (auto& node : m_AST) {
      node->debugNode();
    }
  }

  // pass0 is for detecting and booking all symbols (gathering preinfo)
  // pass0.cpp
  void pass0();
  bool redefinitionCheck(SymbolInfoList & funcName, SymbolInfo& symbol);
  bool redefinitionCheck(SymbolInfo& symbol);
  void RedefinitionError(std::string message,SymbolInfo& symbolInfo);

  ~CStarCodegen() {
    // Need to release and delete it manually
    auto moduleRef = m_MainModule.release();
    moduleRef->dropAllReferences();
  }
};

#endif