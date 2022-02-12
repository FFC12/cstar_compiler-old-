#ifndef PARAM_AST_HPP
#define PARAM_AST_HPP
#include <ast/ast.hpp>
#include <parser/type_qualifier.hpp>

class ParamAST : public IAST {
  ASTNode m_Symbol0, m_Symbol1;
  ASTNode m_TypeNode;
  TypeQualifier m_TypeQualifier;
  bool m_IsCastable;
  bool m_IsNotClear;

 public:
  ParamAST(ASTNode symbol0, ASTNode symbol1, ASTNode typeNode,
                    bool isCastable, bool isNotClear,
                    TypeQualifier typeQualifier, SemanticLoc& semanticLoc)
      : IAST(semanticLoc),
        m_Symbol0(std::move(symbol0)),
        m_Symbol1(std::move(symbol1)),
        m_IsCastable(isCastable),
        m_IsNotClear(isNotClear),
        m_TypeQualifier(typeQualifier),
        m_TypeNode(std::move(typeNode)) {
    this->m_ASTKind = ASTKind::Expr;
    this->m_ExprKind = ExprKind::ParamExpr;
  }

  void debugNode() override { m_Symbol0->debugNode(); }
};

#endif