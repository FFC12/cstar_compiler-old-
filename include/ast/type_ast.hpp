#ifndef TYPE_AST_HPP
#define TYPE_AST_HPP
#include <ast/ast.hpp>
#include <parser/types.hpp>
#include <parser/type_specifiers.hpp>

class TypeAST : public IAST {
  friend Visitor;
  TypeSpecifier m_TypeSpec;
  ASTNode m_Symbol;
  size_t m_IndirectLevel;
  bool m_IsUniquePtr;
  bool m_IsPrimitiveType;
  bool m_IsRef;

 public:
  TypeAST(TypeSpecifier typeSpec, ASTNode symbol, bool isUniquePtr, bool isPrimitive,
          bool isRef, size_t indirectLevel, SemanticLoc& semanticLoc)
      : IAST(semanticLoc),
        m_TypeSpec(typeSpec),
        m_Symbol(std::move(symbol)),
        m_IsUniquePtr(isUniquePtr),
        m_IsRef(isRef),
        m_IsPrimitiveType(isPrimitive),
        m_IndirectLevel(indirectLevel) {
    this->m_ASTKind = ASTKind::Expr;
    this->m_ExprKind = ExprKind::TypeExpr;
  }

  void debugNode() override {
    std::cout << this->m_TypeSpec << " as type";
    for (size_t i = 0; i < this->m_IndirectLevel; i++)
      std::cout << (this->m_IsUniquePtr ? "^" : "*");
  }

  SymbolInfo acceptBefore(Visitor& visitor) override {
    return visitor.previsit(*this);
  }

  ValuePtr accept(Visitor& visitor) override { return visitor.visit(*this); }
};

#endif