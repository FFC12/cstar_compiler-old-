#ifndef NEW_AST_HPP
#define NEW_AST_HPP

#include <ast/ast.hpp>
#include <parser/type_specifiers.hpp>
#include <string>
#include <utility>

class NewAST : public IAST {
  friend Visitor;
  TypeSpecifier m_TypeSpec;
  std::string m_TypeName;
  ASTNode m_Allocator;
  ASTNode m_Args;
  ASTNode m_ArrayLength;
  bool m_IsShared;
  bool m_IsFallible;
  bool m_IsArrayAllocation;

 public:
  NewAST(TypeSpecifier typeSpec, std::string typeName, ASTNode allocator,
         ASTNode args, ASTNode arrayLength, bool isShared, bool isFallible,
         bool isArrayAllocation, SemanticLoc semLoc)
      : IAST(semLoc),
        m_TypeSpec(typeSpec),
        m_TypeName(std::move(typeName)),
        m_Allocator(std::move(allocator)),
        m_Args(std::move(args)),
        m_ArrayLength(std::move(arrayLength)),
        m_IsShared(isShared),
        m_IsFallible(isFallible),
        m_IsArrayAllocation(isArrayAllocation) {
    this->m_ASTKind = ASTKind::Expr;
    this->m_ExprKind = ExprKind::NewExpr;
  }

  void debugNode() override {
    if (m_IsShared) std::cout << "shared ";
    std::cout << "new ";
    if (m_IsFallible) std::cout << "? ";
    if (m_Allocator) {
      std::cout << "(";
      m_Allocator->debugNode();
      std::cout << ") ";
    }
    std::cout << m_TypeName;
    if (m_IsArrayAllocation) {
      std::cout << "[";
      if (m_ArrayLength) m_ArrayLength->debugNode();
      std::cout << "]";
      return;
    }
    std::cout << "(";
    if (m_Args) m_Args->debugNode();
    std::cout << ")";
  }

  SymbolInfo acceptBefore(Visitor& visitor) override {
    return visitor.preVisit(*this);
  }

  ValuePtr accept(Visitor& visitor) override { return visitor.visit(*this); }
};

#endif
