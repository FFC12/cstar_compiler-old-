#ifndef SYMBOL_AST_HPP
#define SYMBOL_AST_HPP
#include <ast/ast.hpp>
#include <string>

class SymbolAST : public IAST {
  // it can be whether a type name or variable name.
  // it'll be resolved when we performed semantics analysis.
  std::string m_SymbolName;

 public:
  SymbolAST(std::string symbolName) : m_SymbolName(symbolName) {}
  void debugNode() override {}
};

#endif