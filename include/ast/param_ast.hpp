#ifndef PARAM_AST_HPP
#define PARAM_AST_HPP
#include <ast/ast.hpp>
#include <parser/type_qualifier.hpp>

class ParamAST : public IAST {
  friend Visitor;
  ASTNode m_Symbol0, m_Symbol1;
  ASTNode m_TypeNode;
  TypeQualifier m_TypeQualifier;
  std::vector<ASTNode> m_ArrDim;
  bool m_IsSubscriptable;
  bool m_IsPrimitive;
  bool m_IsCastable;
  bool m_TypeAmbiguous;
  bool m_IsNotClear;

 public:
  ParamAST(ASTNode symbol0, ASTNode symbol1, ASTNode typeNode,
           std::vector<ASTNode> arrDim, bool isSubscriptable, bool isCastable,
           bool isNotClear, bool isAmbiguous, bool isPrimitive, TypeQualifier typeQualifier,
           SemanticLoc& semanticLoc)
      : IAST(semanticLoc),
        m_Symbol0(std::move(symbol0)),
        m_Symbol1(std::move(symbol1)),
        m_ArrDim(std::move(arrDim)),
        m_IsSubscriptable(isSubscriptable),
        m_IsPrimitive(isPrimitive),
        m_IsCastable(isCastable),
        m_IsNotClear(isNotClear),
        m_TypeAmbiguous(isAmbiguous),
        m_TypeQualifier(typeQualifier),
        m_TypeNode(std::move(typeNode)) {
    this->m_ASTKind = ASTKind::Expr;
    this->m_ExprKind = ExprKind::ParamExpr;
  }

  void debugNode() override { m_Symbol0->debugNode(); }

  SymbolInfo acceptBefore(Visitor& visitor) override {
    return visitor.previsit(*this);
  }

  ValuePtr accept(Visitor& visitor) override { return visitor.visit(*this); }
};

#endif