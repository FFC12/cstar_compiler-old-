#ifndef FIX_AST_HPP
#define FIX_AST_HPP
#include <ast/ast.hpp>

class FixAST : public IAST {
  friend Visitor;

  std::string m_Name;
  bool m_IsPrefix;
  bool m_IsPostfix;
  bool m_IsIncrement;
  bool m_IsDecrement;

 public:
  FixAST(std::string name, bool isPrefix, bool isPostfix, bool isIncremant,
         bool isDecrement, SemanticLoc& semanticLoc)
      : IAST(semanticLoc),
        m_Name(std::move(name)),
        m_IsPrefix(isPrefix),
        m_IsPostfix(isPostfix),
        m_IsIncrement(isPrefix),
        m_IsDecrement(isDecrement) {}

  SymbolInfo acceptBefore(Visitor& visitor) override {
    return visitor.preVisit(*this);
  }

  ValuePtr accept(Visitor& visitor) override { return visitor.visit(*this); }
};

#endif