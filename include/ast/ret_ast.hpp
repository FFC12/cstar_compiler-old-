#ifndef RET_AST_HPP
#define RET_AST_HPP
#include <ast/ast.hpp>

class RetAST : public IAST {
  ASTNode m_RetExpr;
  bool m_NoReturn;

 public:
  RetAST(ASTNode retExpr, bool noReturn, SemanticLoc& semanticLoc)
      : IAST(semanticLoc), m_RetExpr(std::move(retExpr)), m_NoReturn(noReturn) {
    this->m_ASTKind = ASTKind::Expr;
    this->m_ExprKind = ExprKind::RetExpr;
  }
};

#endif