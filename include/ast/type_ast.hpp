#ifndef TYPE_AST_HPP
#define TYPE_AST_HPP
#include <ast/ast.hpp>

enum Type {
  T_I8,
  T_I16,
  T_I32,
  T_I64,
  T_INT,
  T_U8,
  T_U16,
  T_U32,
  T_U64,
  T_U128,
  T_UINT,
  T_ISIZE,
  T_USIZE,
  T_F32,
  T_F64,
  T_FLOAT,
  T_CHAR,
  T_UCHAR,
  T_BOOL,
  T_VEC2,
  T_VEC3,
  T_VEC4,
  // MATXxX - MAT

  // User defined types
  T_DEFINED,  // This will be checked in next phase (type checker)
};

class TypeAST : public IAST {
  Type m_TypeSpec;
  size_t m_IndirectLevel;
  bool m_IsUniquePtr;

 public:
  TypeAST(Type typeSpec, bool isUniquePtr, size_t indirectLevel)
      : m_TypeSpec(typeSpec), m_IsUniquePtr(isUniquePtr), m_IndirectLevel(indirectLevel) {
    this->m_ASTKind = ASTKind::Expr;
    this->m_ExprKind = ExprKind::TypeExpr;
  }

  void debugNode() override {
    std::cout << this->m_TypeSpec << " as type";
    for(size_t i = 0; i < this->m_IndirectLevel; i++)
      std::cout << (this->m_IsUniquePtr ? "^" : "*");
  }
};

#endif