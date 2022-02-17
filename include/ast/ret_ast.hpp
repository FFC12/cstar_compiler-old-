#ifndef RET_AST_HPP
#define RET_AST_HPP
#include <ast/ast.hpp>

class RetAST : public IAST {
  friend Visitor;
  ASTNode m_RetExpr;
  bool m_NoReturn;

 public:
  RetAST(ASTNode retExpr, bool noReturn, SemanticLoc& semanticLoc)
      : IAST(semanticLoc), m_RetExpr(std::move(retExpr)), m_NoReturn(noReturn) {
    this->m_ASTKind = ASTKind::Expr;
    this->m_ExprKind = ExprKind::RetExpr;
  }

  void debugNode() override {
    std::cout << "ret";

    if (!m_NoReturn) {
      std::cout << " ";
      this->m_RetExpr->debugNode();
    }
  }

  SymbolInfo acceptBefore(Visitor& visitor) override {
    return visitor.previsit(*this);
  }

  ValuePtr accept(Visitor& visitor) override { return visitor.visit(*this); }
};

#endif