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
static bool IsPrimitiveType(TypeSpecifier typeSpecifier) {
  switch (typeSpecifier) {
    case SPEC_I8:
    case SPEC_I16:
    case SPEC_I32:
    case SPEC_I64:
    case SPEC_INT:
    case SPEC_U8:
    case SPEC_U16:
    case SPEC_U32:
    case SPEC_U64:
    case SPEC_U128:
    case SPEC_UINT:
    case SPEC_ISIZE:
    case SPEC_USIZE:
    case SPEC_F32:
    case SPEC_F64:
    case SPEC_FLOAT:
    case SPEC_CHAR:
    case SPEC_UCHAR:
    case SPEC_BOOL:
      return true;
    default:
      return false;
  }
}

static bool LosslessCasting(TypeSpecifier target, TypeSpecifier source) {
  return GetBitSize(target) < GetBitSize(source);
}

void Visitor::accumulateIncompatiblePtrErrMesg(const SymbolInfo &symbolInfo,
                                               const std::string &s = "") {
  std::string typeQualifier;
  if (this->m_LastSymbolInfo.isReadOnly) typeQualifier = "readonly";
  if (this->m_LastSymbolInfo.isConstRef) typeQualifier = "constref";
  if (this->m_LastSymbolInfo.isConstPtr) typeQualifier = "constptr";
  if (this->m_LastSymbolInfo.isConstVal) typeQualifier = "const";

  std::string indirection;
  if (this->m_LastSymbolInfo.isConstRef) indirection += "&";

  if (!this->m_LastSymbolInfo.isConstRef) {
    for (int i = 0; i < this->m_LastSymbolInfo.indirectionLevel; i++) {
      if (this->m_LastSymbolInfo.isUnique) {
        indirection += "^";
      } else {
        indirection += "*";
      }
    }
  }

  if (s.empty()) {
    this->m_TypeErrorMessages.emplace_back(
        "Incompatible type. Expected a suitable value with '" +
            (typeQualifier.empty() ? "" : (typeQualifier + " ")) +
            GetTypeStr(this->m_ExpectedType) + indirection + "'",
        symbolInfo);
  } else {
    this->m_TypeErrorMessages.emplace_back(
        s + " '" + (typeQualifier.empty() ? "" : (typeQualifier + " ")) +
            GetTypeStr(this->m_ExpectedType) + indirection + "'",
        symbolInfo);
  }
}

SymbolInfo Visitor::preVisit(VarAST &varAst) {
  SymbolInfo symbolInfo;

  if (m_TypeChecking || true) {
    symbolInfo.symbolName = varAst.m_Name;
    symbolInfo.begin = varAst.m_SemLoc.begin;
    symbolInfo.end = varAst.m_SemLoc.end;
    symbolInfo.line = varAst.m_SemLoc.line;

    // symbolInfo.symbolId = Visitor::SymbolId;
    // Visitor::SymbolIdList[symbolInfo.symbolName] = symbolInfo.symbolId;

    if (varAst.m_IsLocal) {
      // will be evualated later
      // symbolInfo = varAst.m_RHS->acceptBefore(*this);
      symbolInfo.isGlob = false;

      symbolInfo.symbolId = Visitor::SymbolId++ + 1;
    } else {
      symbolInfo.isGlob = true;
      symbolInfo.scopeLevel = 0;
      symbolInfo.scopeId = 0;

      symbolInfo.symbolId = Visitor::SymbolId;
    }

    symbolInfo.type = varAst.m_TypeSpec;
    symbolInfo.isSubscriptable = varAst.m_IsInitializerList;
    symbolInfo.indirectionLevel = varAst.m_IndirectLevel;
    symbolInfo.isConstRef = varAst.m_TypeQualifier == Q_CONSTREF;
    symbolInfo.isConstPtr = varAst.m_TypeQualifier == Q_CONSTPTR;
    symbolInfo.isReadOnly = varAst.m_TypeQualifier == Q_READONLY;
    symbolInfo.isConstVal = varAst.m_TypeQualifier == Q_CONST;
    symbolInfo.isRef = varAst.m_IsRef;
    symbolInfo.isUnique = varAst.m_IsUniquePtr;

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

    if (varAst.m_TypeSpec == TypeSpecifier::SPEC_DEFINED) {
      if (varAst.m_Typename->m_ExprKind == ExprKind::SymbolExpr) {
        auto typeName =
            dynamic_cast<SymbolAST *>(varAst.m_Typename.get())->m_SymbolName;
        this->m_DefinedTypeName = typeName;
        this->m_DefinedTypeFlag = true;
      }
    }

    // reset all state flags.
    this->m_LastBinOpHasAtLeastOnePtr = false;

    this->m_LastSymbolInfo = symbolInfo;
    auto tempSymbolInfo = varAst.m_RHS->acceptBefore(*this);
  }

  return symbolInfo;
}

SymbolInfo Visitor::preVisit(AssignmentAST &assignmentAst) {
  SymbolInfo symbolInfo;

  if (m_TypeChecking) {
    if (assignmentAst.m_LHS->m_ASTKind != ASTKind::Expr &&
        assignmentAst.m_ExprKind != ExprKind::SymbolExpr) {
      // problem..
      assert(false && "Will be implemented!");
    } else {
      SymbolInfo matchedSymbol;
      auto lhs = (SymbolAST *)assignmentAst.m_LHS.get();

      symbolInfo.symbolName = lhs->m_SymbolName;
      symbolInfo.begin = lhs->m_SemLoc.begin;
      symbolInfo.end = lhs->m_SemLoc.end;
      symbolInfo.line = lhs->m_SemLoc.line;

      if (symbolValidation(lhs->m_SymbolName, symbolInfo, matchedSymbol)) {
        this->m_ExpectedType = matchedSymbol.type;

        if (matchedSymbol.type == TypeSpecifier::SPEC_DEFINED) {
          // definedTypeName...
          this->m_DefinedTypeFlag = true;
        }

        this->m_LastBinOpHasAtLeastOnePtr = false;
        //        this->m_LastSymbolInfo = matchedSymbol;

        //      this->m_LastIde
        auto rhs = assignmentAst.m_RHS->acceptBefore(*this);
      }
    }
  }

  return symbolInfo;
}

SymbolInfo Visitor::preVisit(SymbolAST &symbolAst) {
  auto symbolName = symbolAst.m_SymbolName;

  SymbolInfo symbolInfo;

  symbolInfo.symbolName = symbolName;
  symbolInfo.begin = symbolAst.m_SemLoc.begin;
  symbolInfo.end = symbolAst.m_SemLoc.end;
  symbolInfo.line = symbolAst.m_SemLoc.line;

  SymbolInfo matchedSymbol;
  if (this->m_TypeChecking) {
    if (!this->m_LastLoopDataSymbol && !this->m_LastLoopIndexSymbol) {
      if (symbolValidation(symbolName, symbolInfo, matchedSymbol)) {
        this->m_MatchedSymbolType = matchedSymbol.type;

        if (matchedSymbol.type == this->m_LastSymbolInfo.type) {
          if (matchedSymbol.indirectionLevel +
                      (this->m_LastReferenced ? 1 : 0) !=
                  this->m_LastSymbolInfo.indirectionLevel &&
              this->m_LastBinOp) {
            symbolInfo.typeCheckerInfo.isCompatiblePtr = false;
          } else if (matchedSymbol.indirectionLevel +
                             (this->m_LastReferenced ? 1 : 0) !=
                         this->m_LastSymbolInfo.indirectionLevel &&
                     this->m_LastSymbolInfo.indirectionLevel &&
                     !this->m_LastBinOp) {
            accumulateIncompatiblePtrErrMesg(symbolInfo);
          }

          if ((this->m_LastBinOp &&
               matchedSymbol.indirectionLevel ==
                   this->m_LastSymbolInfo.indirectionLevel)) {
            this->m_LastBinOpHasAtLeastOnePtr = true;
          }
        } else {
          if (IsPrimitiveType(this->m_ExpectedType) &&
              IsPrimitiveType(this->m_MatchedSymbolType) &&
              !this->m_LastReferenced) {
            // Plain type without ptr-level
            if (LosslessCasting(this->m_ExpectedType,
                                this->m_MatchedSymbolType) &&
                this->m_LastSymbolInfo.indirectionLevel == 0) {
              this->m_TypeWarningMessages.emplace_back(
                  "A '" + GetTypeStr(this->m_MatchedSymbolType) +
                      "' type is casting to '" +
                      GetTypeStr(this->m_ExpectedType) +
                      "'. Potential data loss might be occured!",
                  symbolInfo);
            }
          } else {
            accumulateIncompatiblePtrErrMesg(symbolInfo);
          }
        }

      } else {
        if (this->m_LastLoopDataSymbol || this->m_LastLoopIndexSymbol) {
          // okay
        }
        // Symbol could not validate obviously.
        // but it will be processed inside of the symbolValidation func.
      }
    }
  } else {
  }

  return symbolInfo;
}

SymbolInfo Visitor::preVisit(ScalarOrLiteralAST &scalarAst) {
  SymbolInfo symbolInfo;

  symbolInfo.begin = scalarAst.m_SemLoc.begin;
  symbolInfo.end = scalarAst.m_SemLoc.end;
  symbolInfo.line = scalarAst.m_SemLoc.line;

  if (this->m_TypeChecking) {
    if (this->m_LastReferenced) {
      this->m_TypeErrorMessages.emplace_back(
          "Constant value cannot be referenced", symbolInfo);
    } else {
      if (!this->m_DefinedTypeFlag && !this->m_LastCondExpr) {
        if (this->m_ExpectedType == TypeSpecifier::SPEC_BOOL &&
            scalarAst.m_IsBoolean) {
        } else if ((this->m_ExpectedType == TypeSpecifier::SPEC_FLOAT ||
                    this->m_ExpectedType == TypeSpecifier::SPEC_F32 ||
                    this->m_ExpectedType == TypeSpecifier::SPEC_F64) &&
                   scalarAst.m_IsIntegral && scalarAst.m_IsFloat) {
        } else if (this->m_ExpectedType == TypeSpecifier::SPEC_VOID &&
                   this->m_LastSymbolInfo.indirectionLevel == 0) {
          this->m_TypeErrorMessages.emplace_back(
              "'void' is an incomplete type and cannot be used as a "
              "declaration type",
              m_LastSymbolInfo);
        } else if ((this->m_ExpectedType == TypeSpecifier::SPEC_CHAR ||
                    this->m_ExpectedType == TypeSpecifier::SPEC_UCHAR) &&
                   scalarAst.m_IsLetter &&
                   this->m_LastSymbolInfo.indirectionLevel == 0) {
        } else if ((this->m_ExpectedType == TypeSpecifier::SPEC_CHAR ||
                    this->m_ExpectedType == TypeSpecifier::SPEC_UCHAR) &&
                   scalarAst.m_IsLiteral &&
                   this->m_LastSymbolInfo.indirectionLevel > 0 &&
                   (this->m_LastSymbolInfo.isConstPtr ||
                    this->m_LastSymbolInfo.isReadOnly)) {
        } else if ((this->m_ExpectedType == TypeSpecifier::SPEC_U8 ||
                    this->m_ExpectedType == TypeSpecifier::SPEC_U16 ||
                    this->m_ExpectedType == TypeSpecifier::SPEC_U32 ||
                    this->m_ExpectedType == TypeSpecifier::SPEC_U64 ||
                    this->m_ExpectedType == TypeSpecifier::SPEC_U128 ||
                    this->m_ExpectedType == TypeSpecifier::SPEC_UINT ||
                    this->m_ExpectedType == TypeSpecifier::SPEC_USIZE) &&
                   scalarAst.m_IsIntegral &&
                   this->m_LastSymbolInfo.indirectionLevel == 0) {
          if (scalarAst.m_IsFloat) {
            this->m_TypeWarningMessages.emplace_back(
                "A 'float' type is casting to '" +
                    GetTypeStr(this->m_ExpectedType) +
                    "'. Potential data loss might be occured!",
                symbolInfo);
          }
        } else if ((this->m_ExpectedType == TypeSpecifier::SPEC_I8 ||
                    this->m_ExpectedType == TypeSpecifier::SPEC_I16 ||
                    this->m_ExpectedType == TypeSpecifier::SPEC_I32 ||
                    this->m_ExpectedType == TypeSpecifier::SPEC_I64 ||
                    this->m_ExpectedType == TypeSpecifier::SPEC_INT ||
                    this->m_ExpectedType == TypeSpecifier::SPEC_ISIZE) &&
                   scalarAst.m_IsIntegral &&
                   this->m_LastSymbolInfo.indirectionLevel == 0) {
          if (scalarAst.m_IsFloat) {
            this->m_TypeWarningMessages.emplace_back(
                "A 'float' type is casting to '" +
                    GetTypeStr(this->m_ExpectedType) +
                    "'. Potential data loss might be occured!",
                symbolInfo);
          }
        } else {
          symbolInfo.begin = scalarAst.m_SemLoc.begin;
          symbolInfo.end = scalarAst.m_SemLoc.end;
          symbolInfo.line = scalarAst.m_SemLoc.line;

          if (this->m_LastBinOp) {
            // it's okay
          } else {
            this->accumulateIncompatiblePtrErrMesg(symbolInfo);
          }
        }
      } else {
        if (!m_LastCondExpr) {
          this->accumulateIncompatiblePtrErrMesg(symbolInfo);
        }
      }
    }
  }
  return symbolInfo;
}

SymbolInfo Visitor::preVisit(BinaryOpAST &binaryOpAst) {
  SymbolInfo symbolInfo;
  if (this->m_TypeChecking) {
    ASTNode &lhs = binaryOpAst.m_LHS, &rhs = binaryOpAst.m_RHS;

    bool isPtrType = false;
    bool errorFlag = false;

    if (this->m_LastSymbolInfo.indirectionLevel != 0) {
      isPtrType = true;
    }

    m_LastBinOp = true;

    this->m_BinOpTermCount += 1;
    auto rhsSymbol = rhs->acceptBefore(*this);
    this->m_BinOpTermCount -= 1;

    if (isPtrType) {
      if (!rhsSymbol.typeCheckerInfo.isCompatiblePtr &&
          (this->m_LastBinOp && !this->m_LastBinOpHasAtLeastOnePtr) &&
          this->m_BinOpTermCount == 0) {
        this->m_TypeErrorMessages.emplace_back("Invalid operand '" +
                                                   rhsSymbol.symbolName + "'" +
                                                   " of binary operation",
                                               rhsSymbol);
        rhsSymbol.typeCheckerInfo.isCompatiblePtr = true;
        errorFlag = true;
      }
    }

    this->m_BinOpTermCount += 1;
    auto lhsSymbol = lhs->acceptBefore(*this);
    this->m_BinOpTermCount -= 1;
    if (isPtrType && !errorFlag) {
      if (!lhsSymbol.typeCheckerInfo.isCompatiblePtr &&
          (this->m_LastBinOp && !this->m_LastBinOpHasAtLeastOnePtr) &&
          this->m_BinOpTermCount == 0) {
        this->m_TypeErrorMessages.emplace_back("Invalid operand '" +
                                                   lhsSymbol.symbolName + "'" +
                                                   " of binary operation",
                                               lhsSymbol);
        lhsSymbol.typeCheckerInfo.isCompatiblePtr = true;
      }
    }

    this->m_LastBinOp = false;
  } else {
    // nothing to do
  }

  return symbolInfo;
}
SymbolInfo Visitor::preVisit(CastOpAST &castOpAst) {}

SymbolInfo Visitor::preVisit(FuncAST &funcAst) {
  SymbolInfo symbolInfo;

  symbolInfo.assocFuncName = funcAst.m_FuncName;
  symbolInfo.begin = funcAst.m_SemLoc.begin;
  symbolInfo.end = funcAst.m_SemLoc.end;
  symbolInfo.line = funcAst.m_SemLoc.line;

  if (m_TypeChecking) {
    this->m_LastScopeSymbols = this->m_LocalSymbolTable[funcAst.m_FuncName];
    for (auto &node : funcAst.m_Scope) {
      typeCheckerScopeHandler(node);
    }
  } else {
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
            this->m_TypeErrorMessages.emplace_back(
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
  }
  return symbolInfo;
}

SymbolInfo Visitor::preVisit(IfStmtAST &ifStmtAst) {
  SymbolInfo symbolInfo;
  enterScope(false);

  auto scopeLevel = this->m_ScopeLevel;
  auto scopeId = this->m_ScopeId;

  for (auto &block : ifStmtAst.m_Cond) {
    // type checking for condition
    if (m_TypeChecking) {
      this->m_LastCondExpr = true;
      block.second.first->acceptBefore(*this);
      this->m_LastCondExpr = false;
    }
    // there is only one node actually..
    for (auto &node : block.second.second) {
      scopeHandler(node, SymbolScope::IfSt, scopeLevel, scopeId);
    }
  }

  if (ifStmtAst.m_HasElif) {
    for (auto &entry : ifStmtAst.m_ElseIfs) {
      // type checking for condition
      if (m_TypeChecking) {
        this->m_LastCondExpr = true;
        entry.second.first->acceptBefore(*this);
        this->m_LastCondExpr = false;
      }
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

  if (m_TypeChecking) {
    if (loopStmtAst.m_RangeLoop) {
      if (loopStmtAst.m_Indexable) {
        if (loopStmtAst.m_HasNumericRange) {
          loopStmtAst.m_Min->acceptBefore(*this);
          loopStmtAst.m_Max->acceptBefore(*this);
        } else {
          // TODO: Need to extra check for iterable data.
          loopStmtAst.m_IterSymbol->acceptBefore(*this);
        }
        this->m_LastLoopIndexSymbol = true;
        loopStmtAst.m_IndexSymbol->acceptBefore(*this);
        this->m_LastLoopIndexSymbol = false;
      }
      this->m_LastLoopDataSymbol = true;
      loopStmtAst.m_DataSymbol->acceptBefore(*this);
      this->m_LastLoopDataSymbol = false;
    } else {
      this->m_LastCondExpr = true;
      loopStmtAst.m_Cond->acceptBefore(*this);
      this->m_LastCondExpr = false;
    }
  }
  auto scopeLevel = this->m_ScopeLevel;
  auto scopeId = this->m_ScopeId;

  for (auto &node : loopStmtAst.m_Scope) {
    scopeHandler(node, SymbolScope::LoopSt, scopeLevel, scopeId);
  }

  exitScope(false);
  return symbolInfo;
}

SymbolInfo Visitor::preVisit(FuncCallAST &funcCallAst) {
  //TODO: return type checking...
}


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
SymbolInfo Visitor::preVisit(UnaryOpAST &unaryOpAst) {
  SymbolInfo symbolInfo;

  symbolInfo.begin = unaryOpAst.m_SemLoc.begin;
  symbolInfo.end = unaryOpAst.m_SemLoc.end;
  symbolInfo.line = unaryOpAst.m_SemLoc.line;

  if (!unaryOpAst.m_Node) {
    assert(false && "Need to handle!");
  }

  if (m_TypeChecking) {
    switch (unaryOpAst.m_UnaryOpKind) {
      case U_REF: {
        // ok
        this->m_LastReferenced = true;
        symbolInfo = unaryOpAst.m_Node->acceptBefore(*this);
        this->m_LastReferenced = false;
        break;
      }
      case U_DEREF:
        break;
      case U_SIZEOF:
      case U_PREFIX:
      case U_POSTFIX:
      case U_POSITIVE:
      case U_NEGATIVE:
      case U_NOT:
      case U_BINNEG:
        unaryOpAst.m_Node->acceptBefore(*this);
        break;
      case U_TYPEOF:
        if (m_LastBinOp) {
          this->m_TypeErrorMessages.emplace_back(
              "'typeof' operator cannot be used as an operand of the binary "
              "operation for now. It",
              symbolInfo);
        }
        break;
      case U_MOVE:
        // wrong
        if (m_LastBinOp) {
          this->m_TypeErrorMessages.emplace_back(
              "'move' operator cannot be used as an operand of the binary "
              "operation",
              symbolInfo);
        }
        break;
      default:
        assert(false && "Unreacheable!");
    }
  }

  return symbolInfo;
}
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
    }
  } else if (node->m_ASTKind == ASTKind::Expr &&
             node->m_ExprKind == ExprKind::ParamExpr) {
    auto temp = node->acceptBefore(*this);
  } else if (node->m_ASTKind == ASTKind::Expr &&
             node->m_ExprKind == ExprKind::AssignmentExpr) {
    auto temp = node->acceptBefore(*this);
  }
}

bool Visitor::symbolValidation(std::string &symbolName, SymbolInfo &symbolInfo,
                               SymbolInfo &matchedSymbol) {
  bool isLocalSymbol = false;
  bool isGlobSymbol = false;

  size_t index = this->m_LastSymbolInfo.symbolId;
  for (auto &it : m_GlobalSymbolTable) {
    if (it.symbolName == symbolName) {
      if (it.symbolInfo.symbolId > index) {
        this->m_TypeWarningMessages.emplace_back(
            "'" + symbolName + "' was not in a valid place", it.symbolInfo);
        this->m_TypeErrorMessages.emplace_back(
            "'" + symbolName +
                "' was declared in the wrong place. Probably it used "
                "before "
                "declaration",
            symbolInfo);
        break;
      }
      matchedSymbol = it.symbolInfo;
      isGlobSymbol = true;
      break;
    }
  }

  for (auto &it : this->m_LastScopeSymbols) {
    if (it.symbolName == symbolName) {
      if (it.symbolInfo.symbolId > index) {
        this->m_TypeWarningMessages.emplace_back(
            "'" + symbolName + "' was not in a valid place", it.symbolInfo);
        this->m_TypeErrorMessages.emplace_back(
            "'" + symbolName +
                "' was declared in the wrong place. Probably it used "
                "before "
                "declaration",
            symbolInfo);
        break;
      }
      matchedSymbol = it.symbolInfo;
      isLocalSymbol = true;
      break;
    }
  }

  if (!isLocalSymbol && !isGlobSymbol) {
    this->m_TypeErrorMessages.emplace_back(
        "'" + symbolName + "' was not declared in this scope or global scope",
        symbolInfo);
    return false;
  }

  if (isLocalSymbol) {
    if (isGlobSymbol) {
      // variable shadowing
      this->m_TypeWarningMessages.emplace_back(
          "'" + symbolName + "' shadowing global variable", matchedSymbol);
    } else {
    }
  }

  return true;
}