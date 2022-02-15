#ifndef SCALAR_AST_HPP
#define SCALAR_AST_HPP
#include <stdint.h>

#include <ast/ast.hpp>
#include <string>

// It's just a scalar or constant primitive value like 10, 1.04f, true
class ScalarAST : public IAST {
  friend Visitor;
  std::string m_Value;
  bool m_IsFloat;
  bool m_IsIntegral;
  bool m_IsBoolean;

 public:
  ScalarAST() = delete;

  ScalarAST(std::string value, bool isIntegral, bool isFloat, bool isBoolean,
            SemanticLoc& semanticLoc)
      : IAST(semanticLoc) {
    this->m_Value = value;
    this->m_IsFloat = isFloat;
    this->m_IsIntegral = isIntegral;
    this->m_IsBoolean = isBoolean;
    this->m_ASTKind = ASTKind::Expr;
    this->m_ExprKind = ExprKind::ScalarExpr;
  }

  void debugNode() override { std::cout << this->m_Value; }

  std::string getValue() const { return m_Value; }

  bool isFloat() const { return m_IsFloat; }
};

#endif