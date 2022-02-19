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

  if (m_TypeChecking || true) {
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
    symbolInfo.isSubscriptable = varAst.m_IsInitializerList;
    symbolInfo.indirectionLevel = varAst.m_IndirectLevel;
    symbolInfo.isConstRef = varAst.m_TypeQualifier == Q_CONSTREF;
    symbolInfo.isConstPtr = varAst.m_TypeQualifier == Q_CONSTPTR;
    symbolInfo.isConstVal = varAst.m_TypeQualifier == Q_CONST;
    symbolInfo.isRef = varAst.m_IsRef;
    symbolInfo.isNeededEval = true;

    if(varAst.m_RHS) {
      if (varAst.m_RHS->m_ASTKind == ASTKind::Expr) {
        if (varAst.m_RHS->m_ExprKind == ExprKind::ScalarExpr ||
            varAst.m_RHS->m_ExprKind == ExprKind::SymbolExpr) {
          symbolInfo.isNeededEval = false;
        }
      }
    }
  }

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
SymbolInfo Visitor::previsit(BinaryOpAST &binaryOpAst) {
  if (this->m_TypeChecking) {
    SymbolInfo symbolInfo;

    ASTNode &lhs = binaryOpAst.m_LHS, &rhs = binaryOpAst.m_RHS;
    //   while()

    return symbolInfo;
  } else {
  }
}
SymbolInfo Visitor::previsit(CastOpAST &castOpAst) {}

SymbolInfo Visitor::previsit(FuncAST &funcAst) {
  SymbolInfo symbolInfo;
  symbolInfo.assocFuncName = funcAst.m_FuncName;

  enterScope(false);

  auto scopeLevel = this->m_ScopeLevel;
  auto scopeId = this->m_ScopeId;

  for (auto &param : funcAst.m_Params) {
    auto symbol = param->acceptBefore(*this);
    if (symbol.isNeededTypeCheck) {
      //      if(m_TypeTable.count(symbol.ty))
      bool isLeftOne = false, isRightOne = false;
      for (auto &type : this->m_TypeTable) {
        if (type.first == symbol.definedTypenamePair.first) {
          isLeftOne = true;
        }

        if (type.first == symbol.definedTypenamePair.second) {
          isRightOne = true;
        }
      }

      if (!isLeftOne && !isRightOne) {
        this->m_UnknownTypeErrorMessages.emplace_back(
            "Unknown type '" + symbol.definedTypenamePair.first + "' or '" +
                symbol.definedTypenamePair.second + "'",
            symbol);
      } else {
        symbol.scopeLevel = scopeLevel;
        symbol.scopeId = scopeId;

        if (isRightOne && isLeftOne) {
          auto firstSymbol = symbol.definedTypenamePair.first;
          this->m_UnknownTypeErrorMessages.emplace_back(
              "Unexpected token '" + firstSymbol + "' after '" + firstSymbol +
                  "'",
              symbol);
        } else if (isLeftOne) {
          symbol.symbolName = symbol.definedTypenamePair.second;
          this->m_SymbolInfos.push_back(symbol);
        } else {
          symbol.symbolName = symbol.definedTypenamePair.first;
          this->m_SymbolInfos.push_back(symbol);
        }
      }
    } else {
      scopeHandler(param, SymbolScope::Func, scopeLevel, scopeId);
    }
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
      scopeId++;
      for (auto &node : elseIfBlock.second) {
        scopeHandler(node, SymbolScope::IfSt, scopeLevel, scopeId);
      }
    }
  }

  if (ifStmtAst.m_HasElse) {
    // manually increasing
    scopeId++;
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
    auto temp0 = paramAst.m_Symbol0->acceptBefore(*this);
    symbolInfo.begin = temp0.begin;
    auto leftOne = temp0.symbolName;

    auto temp1 = paramAst.m_Symbol1->acceptBefore(*this);
    symbolInfo.end = temp1.end;
    symbolInfo.line = temp1.line;
    auto rightOne = temp1.symbolName;

    symbolInfo.definedTypenamePair = std::make_pair(leftOne, rightOne);
  } else {
    symbolInfo = paramAst.m_Symbol0->acceptBefore(*this);
    if (paramAst.m_IsPrimitive) {
      auto typeInfo = paramAst.m_TypeNode->acceptBefore(*this);
      symbolInfo.type = typeInfo.type;
    } else {
      symbolInfo.type = TypeSpecifier::SPEC_DEFINED;
    }
  }

  symbolInfo.isSubscriptable = paramAst.m_IsSubscriptable;

  return symbolInfo;
}
SymbolInfo Visitor::previsit(RetAST &retAst) {}
SymbolInfo Visitor::previsit(UnaryOpAST &unaryOpAst) {}
SymbolInfo Visitor::previsit(TypeAST &typeAst) {
  SymbolInfo symbolInfo;

  symbolInfo.type = typeAst.m_TypeSpec;
  symbolInfo.isPrimitive = typeAst.m_IsPrimitiveType;

  return symbolInfo;
}

void Visitor::scopeHandler(std::unique_ptr<IAST> &node, SymbolScope symbolScope,
                           size_t scopeLevel, size_t scopeId) {
  if (node->m_ASTKind == ASTKind::Decl) {
    if (node->m_DeclKind == DeclKind::VarDecl) {
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