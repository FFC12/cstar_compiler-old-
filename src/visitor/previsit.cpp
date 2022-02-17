#include <ast/assignment_ast.hpp>
#include <ast/ast.hpp>
#include <ast/binary_op_ast.hpp>
#include <ast/cast_op_ast.hpp>
#include <ast/func_ast.hpp>
#include <ast/func_call_ast.hpp>
#include <ast/if_stmt.hpp>
#include <ast/loop_stmt.hpp>
#include <ast/param_ast.hpp>
#include <ast/ret_ast.hpp>
#include <ast/scalar_ast.hpp>
#include <ast/symbol_ast.hpp>
#include <ast/type_ast.hpp>
#include <ast/unary_op_ast.hpp>
#include <ast/var_ast.hpp>
#include <visitor/visitor.hpp>

SymbolInfo Visitor::previsit(VarAST &varAst) {
  SymbolInfo symbolInfo;
  symbolInfo.symbolName = varAst.m_Name;
  symbolInfo.begin = varAst.m_SemLoc.begin;
  symbolInfo.end = varAst.m_SemLoc.end;
  symbolInfo.line = varAst.m_SemLoc.line;

  if (varAst.m_IsLocal) {
    // will be evualated later
    // symbolInfo = varAst.m_RHS->acceptBefore(*this);
    symbolInfo.isGlob = false;
  } else {
    symbolInfo.isGlob = true;
    symbolInfo.scopeLevel = 0;
    symbolInfo.scopeId = 0;
  }

  symbolInfo.type = varAst.m_TypeSpec;
  symbolInfo.indirectionLevel = varAst.m_IndirectLevel;
  symbolInfo.isConstRef = varAst.m_IsRef;
  symbolInfo.isConstPtr = varAst.m_TypeQualifier == Q_CONSTPTR;
  symbolInfo.isConstVal = varAst.m_TypeQualifier == Q_CONST;

  return symbolInfo;
}

SymbolInfo Visitor::previsit(AssignmentAST &assignmentAst) {}

SymbolInfo Visitor::previsit(SymbolAST &symbolAst) {
  auto symbolName = symbolAst.m_SymbolName;

  SymbolInfo symbolInfo;
  symbolInfo.value = symbolName;
  symbolInfo.begin = symbolAst.m_SemLoc.begin;
  symbolInfo.end = symbolAst.m_SemLoc.end;
  symbolInfo.line = symbolAst.m_SemLoc.line;

  return symbolInfo;
}

SymbolInfo Visitor::previsit(ScalarOrLiteralAST &scalarAst) {}
SymbolInfo Visitor::previsit(BinaryOpAST &binaryOpAst) {}
SymbolInfo Visitor::previsit(CastOpAST &castOpAst) {}

SymbolInfo Visitor::previsit(FuncAST &funcAst) {
  SymbolInfo symbolInfo;
  symbolInfo.assocFuncName = funcAst.m_FuncName;

  enterScope(false);

  auto scopeLevel = this->m_ScopeLevel;
  auto scopeId = this->m_ScopeId;

  for (auto &node : funcAst.m_Scope) {
    if (node->m_ASTKind == ASTKind::Decl) {
      if (node->m_DeclKind == DeclKind::VarDecl) {
        auto temp = node->acceptBefore(*this);

        temp.symbolScope = SymbolScope::Func;
        temp.scopeLevel = scopeLevel;
        temp.scopeId = scopeId;

        this->m_SymbolInfos.push_back(temp);
      }
    } else if (node->m_ASTKind == ASTKind::Stmt) {
      if (node->m_StmtKind == StmtKind::LoopStmt) {
        node->acceptBefore(*this);
      } else if (node->m_StmtKind == StmtKind::IfStmt) {
      }
    }
  }

  exitScope(false);
  return symbolInfo;
}


SymbolInfo Visitor::previsit(IfStmtAST &ifStmtAst) {
  ifStmtAst.
}

SymbolInfo Visitor::previsit(LoopStmtAST &loopStmtAst) {
  SymbolInfo symbolInfo;
  enterScope(false);

  auto scopeLevel = this->m_ScopeLevel;
  auto scopeId = this->m_ScopeId;

  for (auto &node : loopStmtAst.m_Scope) {
    if (node->m_ASTKind == ASTKind::Decl) {
      if (node->m_DeclKind == DeclKind::VarDecl) {
        Visitor visitor{};

        auto temp = node->acceptBefore(*this);

        temp.symbolScope = SymbolScope::LoopSt;
        temp.scopeLevel = scopeLevel;
        temp.scopeId = scopeId;

        this->m_SymbolInfos.push_back(temp);
      }
    } else if (node->m_ASTKind == ASTKind::Stmt) {
      if (node->m_StmtKind == StmtKind::LoopStmt) {
        node->acceptBefore(*this);
      } else if (node->m_StmtKind == StmtKind::IfStmt) {
      }
    }
  }

  exitScope(false);
  return symbolInfo;
}

SymbolInfo Visitor::previsit(FuncCallAST &funcCallAst) {}
SymbolInfo Visitor::previsit(ParamAST &paramAst) {}
SymbolInfo Visitor::previsit(RetAST &retAst) {}
SymbolInfo Visitor::previsit(UnaryOpAST &unaryOpAst) {}
SymbolInfo Visitor::previsit(TypeAST &typeAst) {}
