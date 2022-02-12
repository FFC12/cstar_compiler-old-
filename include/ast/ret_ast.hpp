#ifndef RET_AST_HPP
#define RET_AST_HPP
#include <ast/ast.hpp>

class RetAST : public IAST {
  ASTNode m_RetExpr;

 public:
  RetAST(ASTNode retExpr, SemanticLoc& semanticLoc)
      : IAST(semanticLoc), m_RetExpr(std::move(retExpr)) {}
};

#endif