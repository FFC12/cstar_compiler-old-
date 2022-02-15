#ifndef CODEGEN_HPP
#define CODEGEN_HPP
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>

#include <map>
#include <parser/parser.hpp>
#include <visitor/visitor.hpp>

struct SymbolInfo {
  bool isGlob;
  size_t scopeLevel;
  size_t priorityOrder;           // top to down
  size_t secondaryPriorityOrder;  // left to right
  Type type;
  std::string value;
};

class CStarCodegen {
  std::multimap<std::string, SymbolInfo> m_Symbols;
  CStarParser m_Parser;
  std::vector<ASTNode> m_AST;
  //  std::vector<llvm::Module> m_Modules;
  std::unique_ptr<llvm::Module> m_MainModule;
  std::unique_ptr<llvm::LLVMContext> m_MainContext;
  std::unique_ptr<llvm::IRBuilder<>> m_IRBuilder;

 public:
  explicit CStarCodegen(CStarParser&& parser, std::string& filepath)
      : m_Parser(std::move(parser)) {
    m_MainContext = std::make_unique<llvm::LLVMContext>();

    auto filename = ExtractFilenameFromPath(filepath, "/");

    m_MainModule = std::make_unique<llvm::Module>(filename, *m_MainContext);
    m_IRBuilder = std::make_unique<llvm::IRBuilder<>>(*m_MainContext);
  }

  void build() {
    this->m_Parser.parse();
    m_Parser.ownedAST(m_AST);
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

  ~CStarCodegen() {
    // Need to release and delete it by ownself
    auto moduleRef = m_MainModule.release();
    moduleRef->dropAllReferences();
  }
};

#endif