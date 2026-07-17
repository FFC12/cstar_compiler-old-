#ifndef NEW_AST_HPP
#define NEW_AST_HPP

#include <ast/ast.hpp>
#include <string>
#include <utility>

class NewAST : public IAST {
  friend Visitor;
  std::string m_TypeName;
  ASTNode m_Allocator;
  ASTNode m_Args;
  bool m_IsShared;
  bool m_IsFallible;

 public:
  NewAST(std::string typeName, ASTNode allocator, ASTNode args, bool isShared,
         bool isFallible, SemanticLoc semLoc)
      : IAST(semLoc),
        m_TypeName(std::move(typeName)),
        m_Allocator(std::move(allocator)),
        m_Args(std::move(args)),
        m_IsShared(isShared),
        m_IsFallible(isFallible) {
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
    std::cout << m_TypeName << "(";
    if (m_Args) m_Args->debugNode();
    std::cout << ")";
  }

  SymbolInfo acceptBefore(Visitor& visitor) override {
    return visitor.preVisit(*this);
  }

  ValuePtr accept(Visitor& visitor) override { return visitor.visit(*this); }
};

#endif
