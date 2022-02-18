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
  symbolInfo.symbolName = symbolName;
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

  size_t i = 0;
  for (auto &param : funcAst.m_Params) {
    auto symbol = param->acceptBefore(*this);
    if (symbol.isNeededTypeCheck) {
      m_AmbiguousSymbols[funcAst.m_FuncName] = i;
    } else {
      scopeHandler(param, SymbolScope::Func, scopeLevel, scopeId);
    }
    i++;
  }

  for (auto &node : funcAst.m_Scope) {
    scopeHandler(node, SymbolScope::Func, scopeLevel, scopeId);
  }

  exitScope(false);
  return symbolInfo;
}

SymbolInfo Visitor::previsit(IfStmtAST &ifStmtAst) {
  SymbolInfo symbolInfo;
  enterScope(false);

  auto scopeLevel = this->m_ScopeLevel;
  auto scopeId = this->m_ScopeId;

  for (auto &block : ifStmtAst.m_Cond) {
    for (auto &node : block.second.second)
      scopeHandler(node, SymbolScope::IfSt, scopeLevel, scopeId);
  }

  if (ifStmtAst.m_HasElif) {
    for (auto &entry : ifStmtAst.m_ElseIfs) {
      auto &elseIfBlock = entry.second;
      // manually increasing
      this->m_ScopeId++;
      for (auto &node : elseIfBlock.second) {
        scopeHandler(node, SymbolScope::IfSt, scopeLevel, scopeId);
      }
    }
  }

  if (ifStmtAst.m_HasElse) {
    // manually increasing
    this->m_ScopeId++;
    for (auto &node : ifStmtAst.m_Else) {
      scopeHandler(node, SymbolScope::IfSt, scopeLevel, scopeId);
    }
  }

  exitScope(false);
  return symbolInfo;
}

SymbolInfo Visitor::previsit(LoopStmtAST &loopStmtAst) {
  SymbolInfo symbolInfo;
  enterScope(false);

  auto scopeLevel = this->m_ScopeLevel;
  auto scopeId = this->m_ScopeId;

  for (auto &node : loopStmtAst.m_Scope) {
    scopeHandler(node, SymbolScope::LoopSt, scopeLevel, scopeId);
  }

  exitScope(false);
  return symbolInfo;
}

SymbolInfo Visitor::previsit(FuncCallAST &funcCallAst) {}
SymbolInfo Visitor::previsit(ParamAST &paramAst) {
  SymbolInfo symbolInfo;

  if (paramAst.m_IsNotClear) {
    symbolInfo.isNeededTypeCheck = true;
  } else {
    symbolInfo = paramAst.m_Symbol0->acceptBefore(*this);
  }

  return symbolInfo;
}
SymbolInfo Visitor::previsit(RetAST &retAst) {}
SymbolInfo Visitor::previsit(UnaryOpAST &unaryOpAst) {}
SymbolInfo Visitor::previsit(TypeAST &typeAst) {}

void Visitor::scopeHandler(std::unique_ptr<IAST> &node, SymbolScope symbolScope,
                           size_t scopeLevel, size_t scopeId) {
  if (node->m_ASTKind == ASTKind::Decl) {
    if (node->m_DeclKind == DeclKind::VarDecl) {
      Visitor visitor{};

      auto temp = node->acceptBefore(*this);

      temp.symbolScope = symbolScope;
      temp.scopeLevel = scopeLevel;
      temp.scopeId = scopeId;

      this->m_SymbolInfos.push_back(temp);
    }
  } else if (node->m_ASTKind == ASTKind::Stmt) {
    if (node->m_StmtKind == StmtKind::LoopStmt) {
      node->acceptBefore(*this);
    } else if (node->m_StmtKind == StmtKind::IfStmt) {
      node->acceptBefore(*this);
    }
  } else if (node->m_ASTKind == ASTKind::Expr &&
             node->m_ExprKind == ExprKind::ParamExpr) {
    auto temp = node->acceptBefore(*this);
    temp.symbolScope = symbolScope;
    temp.scopeLevel = scopeLevel;
    temp.scopeId = scopeId;

    this->m_SymbolInfos.push_back(temp);
  }
}