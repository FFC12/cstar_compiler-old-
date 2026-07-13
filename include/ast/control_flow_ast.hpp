#ifndef CONTROL_FLOW_AST_HPP
#define CONTROL_FLOW_AST_HPP

#include <ast/ast.hpp>

class BreakStmtAST : public IAST {
 public:
  explicit BreakStmtAST(SemanticLoc semLoc) : IAST(semLoc) {
    this->m_ASTKind = ASTKind::Stmt;
    this->m_StmtKind = StmtKind::BreakStmt;
  }

  void debugNode() override { std::cout << "break"; }

  SymbolInfo acceptBefore(Visitor& visitor) override {
    return visitor.preVisit(*this);
  }

  ValuePtr accept(Visitor& visitor) override { return visitor.visit(*this); }
};

class ContinueStmtAST : public IAST {
 public:
  explicit ContinueStmtAST(SemanticLoc semLoc) : IAST(semLoc) {
    this->m_ASTKind = ASTKind::Stmt;
    this->m_StmtKind = StmtKind::ContinueStmt;
  }

  void debugNode() override { std::cout << "continue"; }

  SymbolInfo acceptBefore(Visitor& visitor) override {
    return visitor.preVisit(*this);
  }

  ValuePtr accept(Visitor& visitor) override { return visitor.visit(*this); }
};

#endif
