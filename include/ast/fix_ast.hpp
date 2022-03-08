#ifndef FIX_AST_HPP
#define FIX_AST_HPP
#include <ast/ast.hpp>

class FixAST : public IAST {
  friend Visitor;

  ASTNode m_Symbol;
  bool m_IsPrefix;
  bool m_IsPostfix;
  bool m_IsIncrement;
  bool m_IsDecrement;

 public:
  FixAST() = default;
  FixAST(ASTNode symbol, bool isPrefix, bool isPostfix, bool isIncremant,
         bool isDecrement, SemanticLoc& semanticLoc)
      : IAST(semanticLoc),
        m_Symbol(std::move(symbol)),
        m_IsPrefix(isPrefix),
        m_IsPostfix(isPostfix),
        m_IsIncrement(isPrefix),
        m_IsDecrement(isDecrement) {
    this->m_ASTKind = ASTKind::Expr;
    this->m_ExprKind = ExprKind::FixExpr;
  }

  void debugNode() override {}

  SymbolInfo acceptBefore(Visitor& visitor) override {
    return visitor.preVisit(*this);
  }

  ValuePtr accept(Visitor& visitor) override { return visitor.visit(*this); }
};

#endif