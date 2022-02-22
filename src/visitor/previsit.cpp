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

static size_t GetBitSize(TypeSpecifier typeSpecifier) {
  switch (typeSpecifier) {
    case SPEC_VOID:
      return 1;
    case SPEC_I8:
      return 8;
    case SPEC_I16:
      return 16;
    case SPEC_I32:
      return 32;
    case SPEC_I64:
      return 64;
    case SPEC_INT:
#if defined(_M_X64) || defined(__amd64__)
      return 64;
#else
      return 32;
#endif
    case SPEC_U8:
      return 8;
    case SPEC_U16:
      return 16;
    case SPEC_U32:
      return 32;
    case SPEC_U64:
      return 64;
    case SPEC_U128:
      return 128;
    case SPEC_UINT:
#if defined(_M_X64) || defined(__amd64__)
      return 128;
#else
      return 64;
#endif
    case SPEC_ISIZE:
#if defined(_M_X64) || defined(__amd64__)
      return 64;
#else
      return 32;
#endif
    case SPEC_USIZE:
#if defined(_M_X64) || defined(__amd64__)
      return 128;
#else
      return 64;
#endif
    case SPEC_F32:
      return 32;
    case SPEC_F64:
      return 64;
    case SPEC_FLOAT:
#if defined(_M_X64) || defined(__amd64__)
      return 128;
#else
      return 64;
#endif
    case SPEC_CHAR:
      return 8;
    case SPEC_UCHAR:
      return 8;
    case SPEC_BOOL:
      return 8;
    case SPEC_VEC2:
    case SPEC_VEC3:
    case SPEC_VEC4:
      assert(false && "Not implemented yet!");
    case SPEC_NIL:
      return 8;
    case SPEC_DEFINED:
      assert(false && "Not implemented yet!");
    default:
      assert(false && "Unreacheable");
  }
}
static std::string GetTypeStr(TypeSpecifier typeSpecifier) {
  switch (typeSpecifier) {
    case SPEC_VOID:
      return "void";
    case SPEC_I8:
      return "int8";
    case SPEC_I16:
      return "int16";
    case SPEC_I32:
      return "int32";
    case SPEC_I64:
      return "int64";
    case SPEC_INT:
      return "int";
    case SPEC_U8:
      return "uint8";
    case SPEC_U16:
      return "uint16";
    case SPEC_U32:
      return "uint32";
    case SPEC_U64:
      return "uint64";
    case SPEC_U128:
      return "uint128";
    case SPEC_UINT:
      return "uint";
    case SPEC_ISIZE:
      return "isize";
    case SPEC_USIZE:
      return "usize";
    case SPEC_F32:
      return "float32";
    case SPEC_F64:
      return "float64";
    case SPEC_FLOAT:
      return "float";
    case SPEC_CHAR:
      return "char";
    case SPEC_UCHAR:
      return "uchar";
    case SPEC_BOOL:
      return "bool";
    case SPEC_VEC2:
    case SPEC_VEC4:
    case SPEC_VEC3:
      assert(false && "Not implemented yet!");

    case SPEC_NIL:
      assert(false && "!");
    case SPEC_DEFINED:
    default:
      assert(false && "Unreacheable");
  }
}

SymbolInfo Visitor::preVisit(VarAST &varAst) {
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
    symbolInfo.isReadOnly = varAst.m_TypeQualifier == Q_READONLY;
    symbolInfo.isConstVal = varAst.m_TypeQualifier == Q_CONST;
    symbolInfo.isRef = varAst.m_IsRef;
    symbolInfo.isNeededEval = true;

    if (!varAst.m_RHS) {
      // Well,the symbol it's not initialized.
      symbolInfo.isNeededEval = false;
    }
  }

  // Do not need to look up the symbol table
  // for the declared name. since we can directly
  // take the information from decl.
  if (this->m_TypeChecking && symbolInfo.isNeededEval) {
    this->m_ExpectedType = varAst.m_TypeSpec;

    // reset states
    this->m_DefinedTypeName.clear();
    this->m_DefinedTypeFlag = false;
    this->m_LastSymbolName.clear();
    this->m_LastSymbolIndirectionLevel = 0;
    this->m_LastSymbolIsRef = false;
    this->m_LastSymbolIsUniqPtr = false;
    this->m_LastSymbolConstPtr = false;
    this->m_LastSymbolConstRef = false;
    this->m_LastSymbolRO = false;
    this->m_LastSymbolConst = false;

    if (varAst.m_TypeSpec == TypeSpecifier::SPEC_DEFINED) {
      if (varAst.m_Typename->m_ExprKind == ExprKind::SymbolExpr) {
        auto typeName =
            dynamic_cast<SymbolAST *>(varAst.m_Typename.get())->m_SymbolName;
        this->m_DefinedTypeName = typeName;
        this->m_DefinedTypeFlag = true;
      }
    }

    this->m_LastSymbolName = varAst.m_Name;
    this->m_LastSymbolIndirectionLevel = varAst.m_IndirectLevel;
    this->m_LastSymbolIsRef = varAst.m_IsRef;
    this->m_LastSymbolIsUniqPtr = varAst.m_IsUniquePtr;
    this->m_LastSymbolConstPtr = symbolInfo.isConstPtr;
    this->m_LastSymbolConstRef = symbolInfo.isConstRef;
    this->m_LastSymbolRO = symbolInfo.isReadOnly;
    this->m_LastSymbolConst = symbolInfo.isConstVal;

    auto tempSymbolInfo = varAst.m_RHS->acceptBefore(*this);
  }

  return symbolInfo;
}

SymbolInfo Visitor::preVisit(AssignmentAST &assignmentAst) {}

SymbolInfo Visitor::preVisit(SymbolAST &symbolAst) {
  auto symbolName = symbolAst.m_SymbolName;

  SymbolInfo symbolInfo;
  symbolInfo.symbolName = symbolName;
  symbolInfo.begin = symbolAst.m_SemLoc.begin;
  symbolInfo.end = symbolAst.m_SemLoc.end;
  symbolInfo.line = symbolAst.m_SemLoc.line;

  return symbolInfo;
}

SymbolInfo Visitor::preVisit(ScalarOrLiteralAST &scalarAst) {
  SymbolInfo symbolInfo;

  if (this->m_TypeChecking) {
    if (!this->m_DefinedTypeFlag) {
      if (this->m_ExpectedType == TypeSpecifier::SPEC_BOOL &&
          scalarAst.m_IsBoolean) {
      } else if ((this->m_ExpectedType == TypeSpecifier::SPEC_FLOAT ||
                  this->m_ExpectedType == TypeSpecifier::SPEC_F32 ||
                  this->m_ExpectedType == TypeSpecifier::SPEC_F64) &&
                 scalarAst.m_IsIntegral && scalarAst.m_IsFloat) {
      } else if (this->m_ExpectedType == TypeSpecifier::SPEC_VOID &&
                 this->m_LastSymbolIndirectionLevel) {
        symbolInfo.begin = scalarAst.m_SemLoc.begin;
        symbolInfo.end = scalarAst.m_SemLoc.end;
        symbolInfo.line = scalarAst.m_SemLoc.line;

        this->m_TypeErrorMessages.emplace_back(
            "'void' is an incomplete type and cannot be used as a "
            "declaration type",
            symbolInfo);
      } else if ((this->m_ExpectedType == TypeSpecifier::SPEC_CHAR ||
                  this->m_ExpectedType == TypeSpecifier::SPEC_UCHAR) &&
                 scalarAst.m_IsLetter &&
                 this->m_LastSymbolIndirectionLevel == 0) {
      } else if ((this->m_ExpectedType == TypeSpecifier::SPEC_CHAR ||
                  this->m_ExpectedType == TypeSpecifier::SPEC_UCHAR) &&
                 scalarAst.m_IsLiteral &&
                 this->m_LastSymbolIndirectionLevel > 0 &&
                 (this->m_LastSymbolConstPtr || this->m_LastSymbolRO)) {
      } else if ((this->m_ExpectedType == TypeSpecifier::SPEC_U8 ||
                  this->m_ExpectedType == TypeSpecifier::SPEC_U16 ||
                  this->m_ExpectedType == TypeSpecifier::SPEC_U32 ||
                  this->m_ExpectedType == TypeSpecifier::SPEC_U64 ||
                  this->m_ExpectedType == TypeSpecifier::SPEC_U128 ||
                  this->m_ExpectedType == TypeSpecifier::SPEC_UINT ||
                  this->m_ExpectedType == TypeSpecifier::SPEC_USIZE) &&
                 scalarAst.m_IsIntegral &&
                 this->m_LastSymbolIndirectionLevel == 0) {
      } else if ((this->m_ExpectedType == TypeSpecifier::SPEC_I8 ||
                  this->m_ExpectedType == TypeSpecifier::SPEC_I16 ||
                  this->m_ExpectedType == TypeSpecifier::SPEC_I32 ||
                  this->m_ExpectedType == TypeSpecifier::SPEC_I64 ||
                  this->m_ExpectedType == TypeSpecifier::SPEC_INT ||
                  this->m_ExpectedType == TypeSpecifier::SPEC_ISIZE) &&
                 scalarAst.m_IsIntegral &&
                 this->m_LastSymbolIndirectionLevel == 0) {
      } else {
        symbolInfo.begin = scalarAst.m_SemLoc.begin;
        symbolInfo.end = scalarAst.m_SemLoc.end;
        symbolInfo.line = scalarAst.m_SemLoc.line;

        std::string typeQualifier;
        if (this->m_LastSymbolRO) typeQualifier = "readonly";
        if (this->m_LastSymbolConstRef) typeQualifier = "constref";
        if (this->m_LastSymbolConstPtr) typeQualifier = "constptr";
        if(this->m_LastSymbolConst) typeQualifier = "const";

        std::string indirection;
        if (this->m_LastSymbolIsRef) indirection += "&";

        if (!this->m_LastSymbolIsRef) {
          for (int i = 0; i < this->m_LastSymbolIndirectionLevel; i++) {
            if (this->m_LastSymbolIsUniqPtr) {
              indirection += "^";
            } else {
              indirection += "*";
            }
          }
        }

        this->m_TypeErrorMessages.emplace_back(
            "Incompatible type. Expected a suitable value with '" +
                (typeQualifier.empty() ? "" : (typeQualifier + " ")) +
                GetTypeStr(this->m_ExpectedType) + indirection + "'",
            symbolInfo);
      }
    }
  }

  return symbolInfo;
}

SymbolInfo Visitor::preVisit(BinaryOpAST &binaryOpAst) {
  if (this->m_TypeChecking) {
    SymbolInfo symbolInfo;

    ASTNode &lhs = binaryOpAst.m_LHS, &rhs = binaryOpAst.m_RHS;
    //   while()

    return symbolInfo;
  } else {
  }
}
SymbolInfo Visitor::preVisit(CastOpAST &castOpAst) {}

SymbolInfo Visitor::preVisit(FuncAST &funcAst) {
  SymbolInfo symbolInfo;

  symbolInfo.assocFuncName = funcAst.m_FuncName;
  symbolInfo.begin = funcAst.m_SemLoc.begin;
  symbolInfo.end = funcAst.m_SemLoc.end;
  symbolInfo.line = funcAst.m_SemLoc.line;

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
        this->m_TypeErrorMessages.emplace_back(
            "Unknown type '" + symbol.definedTypenamePair.first + "' or '" +
                symbol.definedTypenamePair.second + "'",
            symbol);
      } else {
        symbol.scopeLevel = scopeLevel;
        symbol.scopeId = scopeId;

        if (isRightOne && isLeftOne) {
          auto firstSymbol = symbol.definedTypenamePair.first;
          this->m_TypeErrorMessages.emplace_back("Unexpected token '" +
                                                     firstSymbol + "' after '" +
                                                     firstSymbol + "'",
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

SymbolInfo Visitor::preVisit(IfStmtAST &ifStmtAst) {
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

SymbolInfo Visitor::preVisit(LoopStmtAST &loopStmtAst) {
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

SymbolInfo Visitor::preVisit(FuncCallAST &funcCallAst) {}
SymbolInfo Visitor::preVisit(ParamAST &paramAst) {
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
SymbolInfo Visitor::preVisit(RetAST &retAst) {}
SymbolInfo Visitor::preVisit(UnaryOpAST &unaryOpAst) {}
SymbolInfo Visitor::preVisit(TypeAST &typeAst) {
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
