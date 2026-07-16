#include <visitor/semantic/semantic_private.hpp>

void Visitor::scopeHandler(std::unique_ptr<IAST> &node,
                           SymbolScope symbolScope) {
  if (node->m_ASTKind == ASTKind::Decl) {
    if (node->m_DeclKind == DeclKind::VarDecl) {
      auto temp = node->acceptBefore(*this);
      this->m_SymbolInfos.push_back(temp);
    }
  } else if (node->m_ASTKind == ASTKind::Stmt) {
    if (node->m_StmtKind == StmtKind::LoopStmt) {
      node->acceptBefore(*this);
    } else if (node->m_StmtKind == StmtKind::IfStmt) {
      node->acceptBefore(*this);
    } else if (node->m_StmtKind == StmtKind::OptionStmt) {
      node->acceptBefore(*this);
    } else if (node->m_StmtKind == StmtKind::BreakStmt ||
               node->m_StmtKind == StmtKind::ContinueStmt ||
               node->m_StmtKind == StmtKind::DropStmt) {
      node->acceptBefore(*this);
    }
  } else if (node->m_ASTKind == ASTKind::Expr &&
             node->m_ExprKind == ExprKind::ParamExpr) {
    auto temp = node->acceptBefore(*this);
    this->m_SymbolInfos.push_back(temp);
  } else if (node->m_ASTKind == ASTKind::Expr &&
             node->m_ExprKind == ExprKind::AssignmentExpr) {
    auto temp = node->acceptBefore(*this);
  } else if (node->m_ASTKind == ASTKind::Expr &&
             node->m_ExprKind == ExprKind::RetExpr) {
    node->acceptBefore(*this);
  } else if (node->m_ASTKind == ASTKind::Expr &&
             node->m_ExprKind == ExprKind::FixExpr) {
    node->acceptBefore(*this);
  } else if (node->m_ASTKind == ASTKind::Expr &&
             node->m_ExprKind == ExprKind::FuncCallExpr) {
    node->acceptBefore(*this);
  } else if (node->m_ASTKind == ASTKind::Expr &&
             node->m_ExprKind == ExprKind::CastExpr) {
    node->acceptBefore(*this);
  }
}

void Visitor::typeCheckerScopeHandler(std::unique_ptr<IAST> &node) {
  if (node->m_ASTKind == ASTKind::Decl) {
    if (node->m_DeclKind == DeclKind::VarDecl) {
      auto temp = node->acceptBefore(*this);
    }
  } else if (node->m_ASTKind == ASTKind::Stmt) {
    if (node->m_StmtKind == StmtKind::LoopStmt) {
      node->acceptBefore(*this);
    } else if (node->m_StmtKind == StmtKind::IfStmt) {
      node->acceptBefore(*this);
    } else if (node->m_StmtKind == StmtKind::OptionStmt) {
      node->acceptBefore(*this);
    } else if (node->m_StmtKind == StmtKind::BreakStmt ||
               node->m_StmtKind == StmtKind::ContinueStmt ||
               node->m_StmtKind == StmtKind::DropStmt) {
      node->acceptBefore(*this);
    }
  } else if (node->m_ASTKind == ASTKind::Expr &&
             node->m_ExprKind == ExprKind::ParamExpr) {
    auto temp = node->acceptBefore(*this);
  } else if (node->m_ASTKind == ASTKind::Expr &&
             node->m_ExprKind == ExprKind::AssignmentExpr) {
    auto temp = node->acceptBefore(*this);
  } else if (node->m_ASTKind == ASTKind::Expr &&
             node->m_ExprKind == ExprKind::RetExpr) {
    node->acceptBefore(*this);
  } else if (node->m_ASTKind == ASTKind::Expr &&
             node->m_ExprKind == ExprKind::FixExpr) {
    node->acceptBefore(*this);
  } else if (node->m_ASTKind == ASTKind::Expr &&
             node->m_ExprKind == ExprKind::FuncCallExpr) {
    node->acceptBefore(*this);
  } else if (node->m_ASTKind == ASTKind::Expr &&
             node->m_ExprKind == ExprKind::CastExpr) {
    node->acceptBefore(*this);
  }
}
