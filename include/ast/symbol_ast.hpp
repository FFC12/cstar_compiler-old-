#ifndef SYMBOL_AST_HPP
#define SYMBOL_AST_HPP
#include <ast/ast.hpp>
#include <string>
#include <utility>

class SymbolAST : public IAST {
  // it can be whether a type name or variable name.
  // it'll be resolved when we performed semantics analysis.
  std::string m_SymbolName;

 public:
  // Maybe we need to use that token infos so preventing to move it...
  explicit SymbolAST(std::string symbolName) : m_SymbolName(symbolName) {
    this->m_ASTKind = ASTKind::Expr;
    this->m_ExprKind = ExprKind::SymbolExpr;
  }
  void debugNode() override { std::cout << m_SymbolName; }
};

#endif