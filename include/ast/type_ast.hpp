#ifndef TYPE_AST_HPP
#define TYPE_AST_HPP
#include <ast/ast.hpp>
#include <parser/type_specifiers.hpp>
#include <parser/types.hpp>

class TypeAST : public IAST {
  friend Visitor;
  TypeSpecifier m_TypeSpec;
  ASTNode m_Symbol;
  size_t m_IndirectLevel;
  bool m_IsUniquePtr;
  bool m_IsPrimitiveType;
  bool m_IsRef;
  bool m_IsNullable;
  bool m_IsDynamicTraitObject;

 public:
  TypeAST(TypeSpecifier typeSpec, ASTNode symbol, bool isUniquePtr,
          bool isPrimitive, bool isRef, size_t indirectLevel,
          SemanticLoc& semanticLoc, bool isNullable = false,
          bool isDynamicTraitObject = false)
      : IAST(semanticLoc),
        m_TypeSpec(typeSpec),
        m_Symbol(std::move(symbol)),
        m_IsUniquePtr(isUniquePtr),
        m_IsRef(isRef),
        m_IsNullable(isNullable),
        m_IsDynamicTraitObject(isDynamicTraitObject),
        m_IsPrimitiveType(isPrimitive),
        m_IndirectLevel(indirectLevel) {
    this->m_ASTKind = ASTKind::Expr;
    this->m_ExprKind = ExprKind::TypeExpr;
  }

  void setIsRef(bool v) { this->m_IsRef = true; }
  void setIsNullable(bool v) { this->m_IsNullable = v; }

  [[nodiscard]] TypeSpecifier typeSpec() const { return m_TypeSpec; }
  [[nodiscard]] const ASTNode& symbol() const { return m_Symbol; }
  [[nodiscard]] size_t indirectLevel() const { return m_IndirectLevel; }
  [[nodiscard]] bool isUniquePtr() const { return m_IsUniquePtr; }
  [[nodiscard]] bool isRef() const { return m_IsRef; }
  [[nodiscard]] bool isNullable() const { return m_IsNullable; }
  [[nodiscard]] bool isDynamicTraitObject() const {
    return m_IsDynamicTraitObject;
  }

  void debugNode() override {
    std::cout << this->m_TypeSpec << " as type";
    for (size_t i = 0; i < this->m_IndirectLevel; i++)
      std::cout << (this->m_IsUniquePtr ? "^" : "*");
  }

  SymbolInfo acceptBefore(Visitor& visitor) override {
    return visitor.preVisit(*this);
  }

  ValuePtr accept(Visitor& visitor) override { return visitor.visit(*this); }
};

#endif
