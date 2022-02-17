#ifndef IF_STMT_HPP
#define IF_STMT_HPP
#include <ast/ast.hpp>
#include <map>
#include <vector>

using Scope = std::vector<ASTNode>;
using ConditionBlock = std::map<ASTNode, Scope>;

class IfStmtAST : public IAST {
  friend Visitor;
  ConditionBlock m_Cond, m_ElseIfs;
  Scope m_Else;
  bool m_HasElif = false;
  bool m_HasElse = false;

 public:
  IfStmtAST(ConditionBlock&& cond, SemanticLoc& semLoc)
      : IAST(semLoc), m_Cond(std::move(cond)) {
    this->m_ASTKind = ASTKind::Stmt;
    this->m_StmtKind = StmtKind::IfStmt;
  }

  IfStmtAST(ConditionBlock&& cond, Scope&& elsePart, SemanticLoc& semLoc)
      : IAST(semLoc), m_Cond(std::move(cond)), m_Else(std::move(elsePart)) {
    this->m_HasElse = true;

    this->m_ASTKind = ASTKind::Stmt;
    this->m_StmtKind = StmtKind::IfStmt;
  }

  IfStmtAST(ConditionBlock&& cond, ConditionBlock&& elseIfs,
            SemanticLoc& semLoc)
      : IAST(semLoc), m_Cond(std::move(cond)), m_ElseIfs(std::move(elseIfs)) {
    this->m_HasElif = true;

    this->m_ASTKind = ASTKind::Stmt;
    this->m_StmtKind = StmtKind::IfStmt;
  }

  IfStmtAST(ConditionBlock&& cond, ConditionBlock&& elseIfs, Scope&& elsePart,
            SemanticLoc& semLoc)
      : IAST(semLoc),
        m_Cond(std::move(cond)),
        m_ElseIfs(std::move(elseIfs)),
        m_Else(std::move(elsePart)) {
    this->m_HasElif = this->m_HasElse = true;

    this->m_ASTKind = ASTKind::Stmt;
    this->m_StmtKind = StmtKind::IfStmt;
  }

  void debugNode() override {
    std::cout << "if(";
    if(!this->m_Cond.empty()) {
      for (auto& c : this->m_Cond) {
        c.first->debugNode();
        std::cout << "){\n";
        for (auto& s : c.second) {
          s->debugNode();
          if(s->getASTKind() != ASTKind::Stmt)
            std::cout << ";\n";
        }
        std::cout << "\n}\n";
      }
    }

    if(m_HasElif) {
      for (auto& c : this->m_Cond) {
        std::cout << "elif(";
        c.first->debugNode();
        std::cout << "){\n";
        for (auto& s : c.second) {
          s->debugNode();
        }
        std::cout << "\n}\n";
      }
    }

    if(m_HasElse) {
      std::cout << "else{\n";
      for(auto& s: this->m_Else){
        s->debugNode();
        if(s->getASTKind() != ASTKind::Stmt)
          std::cout << ";\n";
      }
      std::cout << "\n}\n";
    }
  }

  SymbolInfo acceptBefore(Visitor& visitor) override {
    return visitor.previsit(*this);
  }

  ValuePtr accept(Visitor& visitor) override { return visitor.visit(*this); }
};
#endif