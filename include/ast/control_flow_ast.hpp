#ifndef CONTROL_FLOW_AST_HPP
#define CONTROL_FLOW_AST_HPP

#include <ast/ast.hpp>
#include <string>
#include <utility>

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

class DropStmtAST : public IAST {
  friend Visitor;
  std::string m_SymbolName;

 public:
  DropStmtAST(std::string symbolName, SemanticLoc semLoc)
      : IAST(semLoc), m_SymbolName(std::move(symbolName)) {
    this->m_ASTKind = ASTKind::Stmt;
    this->m_StmtKind = StmtKind::DropStmt;
  }

  void debugNode() override { std::cout << "drop " << m_SymbolName; }

  SymbolInfo acceptBefore(Visitor& visitor) override {
    return visitor.preVisit(*this);
  }

  ValuePtr accept(Visitor& visitor) override { return visitor.visit(*this); }
};

class ThrowStmtAST : public IAST {
  friend Visitor;
  ASTNode m_ErrorExpr;

 public:
  ThrowStmtAST(ASTNode errorExpr, SemanticLoc semLoc)
      : IAST(semLoc), m_ErrorExpr(std::move(errorExpr)) {
    this->m_ASTKind = ASTKind::Stmt;
    this->m_StmtKind = StmtKind::ThrowStmt;
  }

  void debugNode() override {
    std::cout << "throw";
    if (m_ErrorExpr) {
      std::cout << " ";
      m_ErrorExpr->debugNode();
    }
  }

  SymbolInfo acceptBefore(Visitor& visitor) override {
    return visitor.preVisit(*this);
  }

  ValuePtr accept(Visitor& visitor) override { return visitor.visit(*this); }
};

class DeferStmtAST : public IAST {
  friend Visitor;
  std::vector<ASTNode> m_Scope;

 public:
  DeferStmtAST(std::vector<ASTNode>&& scope, SemanticLoc semLoc)
      : IAST(semLoc), m_Scope(std::move(scope)) {
    this->m_ASTKind = ASTKind::Stmt;
    this->m_StmtKind = StmtKind::DeferStmt;
  }

  void debugNode() override {
    std::cout << "defer {\n";
    for (auto& node : m_Scope) {
      node->debugNode();
      std::cout << ";\n";
    }
    std::cout << "}";
  }

  SymbolInfo acceptBefore(Visitor& visitor) override {
    return visitor.preVisit(*this);
  }

  ValuePtr accept(Visitor& visitor) override { return visitor.visit(*this); }
};

#endif
