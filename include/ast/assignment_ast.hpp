#ifndef ASSIGNMENT_AST_HPP
#define ASSIGNMENT_AST_HPP
#include <ast/ast.hpp>
#include <memory>
#include <vector>

enum ShortcutOp {
  S_PLUS,
  S_MIN,
  S_STA,
  S_DIV,
  S_SHR,
  S_SHL,
  S_XOR,
  S_OR,
  S_AND,
  S_MOD,
  S_MOV,
  S_NONE
};

class AssignmentAST : public IAST {
  friend Visitor;
  ASTNode m_LHS;  // SymbolAST
  ASTNode m_RHS;
  bool m_Subscriptable;
  bool m_IsDereferenced;
  size_t m_DerefLevel;
  ShortcutOp m_ShortcutOp;
  std::string m_Op;
  std::vector<ASTNode> m_SubscriptIndexes;

 public:
  AssignmentAST(ASTNode lhs, ASTNode rhs, bool isDereferenced, bool derefLevel,
                std::vector<ASTNode> subscriptIndexes, ShortcutOp shortcutOp,
                std::string& op, SemanticLoc& semanticLoc)
      : IAST(semanticLoc),
        m_Op(op),
        m_LHS(std::move(lhs)),
        m_RHS(std::move(rhs)),
        m_IsDereferenced(isDereferenced),
        m_DerefLevel(derefLevel),
        m_SubscriptIndexes(std::move(subscriptIndexes)),
        m_ShortcutOp(shortcutOp) {
    this->m_ASTKind = ASTKind::Expr;
    this->m_ExprKind = ExprKind::AssignmentExpr;
    this->m_Subscriptable = true;
  }

  AssignmentAST(ASTNode lhs, ASTNode rhs, bool isDereferenced,
                size_t derefLevel, ShortcutOp shortcutOp, std::string& op,
                SemanticLoc& semanticLoc)
      : IAST(semanticLoc),
        m_Op(op),
        m_LHS(std::move(lhs)),
        m_RHS(std::move(rhs)),
        m_IsDereferenced(isDereferenced),
        m_DerefLevel(derefLevel),
        m_ShortcutOp(shortcutOp) {
    this->m_ASTKind = ASTKind::Expr;
    this->m_ExprKind = ExprKind::AssignmentExpr;
    this->m_Subscriptable = false;
  }

  void debugNode() override {
    this->m_LHS->debugNode();
    if (this->m_Subscriptable) {
      std::cout << "[";
      for (int i = 0; i < this->m_SubscriptIndexes.size(); i++) {
        this->m_SubscriptIndexes[i]->debugNode();
        if (i != this->m_SubscriptIndexes.size() - 1) {
          std::cout << ":";
        }
      }
      std::cout << "]";
    }
    std::cout << m_Op;
    this->m_RHS->debugNode();
  }

  SymbolInfo acceptBefore(Visitor& visitor) override {
    return visitor.preVisit(*this);
  }

  ValuePtr accept(Visitor& visitor) override { return visitor.visit(*this); }
};

#endif
