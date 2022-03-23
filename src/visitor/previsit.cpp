#include <ast/assignment_ast.hpp>
#include <ast/ast.hpp>
#include <ast/binary_op_ast.hpp>
#include <ast/cast_op_ast.hpp>
#include <ast/fix_ast.hpp>
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

// It must be binaryExpr the param
size_t Visitor::getIndexesOfArray(IAST &expr) {
  BinaryOpAST *binaryExpr = nullptr;
  size_t i = 1;

  if (expr.m_ExprKind == BinOp) {
    binaryExpr = (BinaryOpAST *)&expr;
    i = 2;

    auto rhs = binaryExpr->m_RHS.get();
    auto op = binaryExpr->m_BinOpKind;

    while (rhs->m_ExprKind == ExprKind::BinOp && op == B_MARRS) {
      rhs = ((BinaryOpAST *)rhs)->m_RHS.get();
      op = ((BinaryOpAST *)rhs)->m_BinOpKind;

      i += 1;
    }
  }

  return i;
}

void Visitor::getElementsOfArray(IAST &binaryExpr,
                                 std::vector<BinOpOrVal> &vector) {
  if (binaryExpr.m_ExprKind == BinOp) {
    auto isBinExpr = (BinaryOpAST *)&binaryExpr;

    if (isBinExpr->m_BinOpKind != B_COMM) goto invalid_binop;

    auto lhs = isBinExpr->m_LHS.get();
    auto rhs = isBinExpr->m_RHS.get();

    if (lhs == nullptr || rhs == nullptr) {
      assert(false && "This should not be happened!");
    }

    if (lhs->m_ExprKind == BinOp) {
      getElementsOfArray(*(BinaryOpAST *)lhs, vector);
    } else {
      if (lhs->m_ExprKind == ScalarExpr) {
        auto expr = (ScalarOrLiteralAST *)lhs;
        vector.emplace_back(false, false, 0, expr->m_Value);
      } else if (lhs->m_ExprKind == SymbolExpr) {
        auto expr = (SymbolAST *)lhs;
        vector.emplace_back(false, true, 0, expr->m_SymbolName);
      } else if (lhs->m_ExprKind == BinOp) {
        auto expr = (uintptr_t)((BinaryOpAST *)lhs);
        vector.emplace_back(true, false, expr, "");
      } else {
        assert(false && "Unimplemented array expr is involved!");
      }
    }

    if (rhs->m_ExprKind == BinOp) {
      getElementsOfArray(*(BinaryOpAST *)rhs, vector);
    } else {
      if (rhs->m_ExprKind == ScalarExpr) {
        auto expr = (ScalarOrLiteralAST *)rhs;
        vector.emplace_back(false, false, 0, expr->m_Value);
      } else if (rhs->m_ExprKind == SymbolExpr) {
        auto expr = (SymbolAST *)rhs;
        vector.emplace_back(false, true, 0, expr->m_SymbolName);
      } else if (rhs->m_ExprKind == BinOp) {
        auto expr = (uintptr_t)((BinaryOpAST *)rhs);
        vector.emplace_back(true, false, expr, "");
      } else {
        assert(false && "Unimplemented array expr is involved!");
      }
    }
  } else {
  invalid_binop:
    if (binaryExpr.m_ExprKind == ScalarExpr) {
      auto expr = (ScalarOrLiteralAST *)&binaryExpr;
      vector.emplace_back(false, false, 0, expr->m_Value);
    } else if (binaryExpr.m_ExprKind == SymbolExpr) {
      auto expr = (SymbolAST *)&binaryExpr;
      vector.emplace_back(false, true, 0, expr->m_SymbolName);
    } else if (binaryExpr.m_ExprKind == BinOp) {
      auto expr = (uintptr_t)((BinaryOpAST *)&binaryExpr);
      vector.emplace_back(true, false, expr, "");
    } else {
      assert(false && "Unimplemented array expr is involved!");
    }
  }
}

// TODO: Need to fixed.
bool Visitor::validateArray(IAST &binaryExpr, size_t level, size_t &index) {
  if (binaryExpr.m_ExprKind == BinOp) {
    auto isBinExpr = (BinaryOpAST *)&binaryExpr;

    SymbolInfo symbolInfo;
    symbolInfo.begin = isBinExpr->m_SemLoc.begin;
    symbolInfo.end = isBinExpr->m_SemLoc.end;
    symbolInfo.line = isBinExpr->m_SemLoc.line;

    if (isBinExpr->m_BinOpKind != B_COMM) goto invalid_binop;

    auto lhs = isBinExpr->m_LHS.get();
    auto rhs = isBinExpr->m_RHS.get();

    if (lhs == nullptr || rhs == nullptr) {
      assert(false && "This should not be happened!");
    }

    if (lhs->m_ExprKind == BinOp) {
      validateArray(*(BinaryOpAST *)lhs, level + 1, index);
      if (level == 0) {
        if (index != m_LastArrayDims[m_LastArrayDims.size()]) {
          this->m_TypeErrorMessages.emplace_back(
              "Array initializer does not meet with size of array", symbolInfo);
        }
      }
    } else {
      index++;
    }

    if (rhs->m_ExprKind == BinOp) {
      validateArray(*(BinaryOpAST *)rhs, level, index);
    } else {
      index++;
    }
  } else {
  invalid_binop:
    std::cout << std::endl;
  }
}

void Visitor::accumulateIncompatiblePtrErrMesg(const SymbolInfo &symbolInfo,
                                               const std::string &s = "") {
  std::string typeQualifier;
  if (this->m_LastSymbolInfo.isReadOnly) typeQualifier = "readonly";
  if (this->m_LastSymbolInfo.isConstRef) typeQualifier = "constref";
  if (this->m_LastSymbolInfo.isConstPtr &&
      this->m_LastSymbolInfo.indirectionLevel > 1)
    typeQualifier = "constptr";
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

  auto extra = !this->m_LastSymbolInfo.isConstPtr &&
                       this->m_ExpectedType == TypeSpecifier::SPEC_CHAR
                   ? "String must have a 'constptr' or 'readonly' qualifier"
                   : "";

  if (s.empty()) {
    this->m_TypeErrorMessages.emplace_back(
        "Incompatible type. Expected a suitable value with '" +
            (typeQualifier.empty() ? "" : (typeQualifier + " ")) +
            GetTypeStr(this->m_ExpectedType) + indirection + "'. " + extra,
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

    symbolInfo.symbolId = this->m_SymbolId;
    symbolInfo.scopeId = this->m_ScopeId;
    symbolInfo.scopeLevel = this->m_ScopeLevel;
    this->m_SymbolId += 1;
  } else {
    symbolInfo.isGlob = true;
    symbolInfo.scopeLevel = 0;
    symbolInfo.scopeId = 0;

    symbolInfo.symbolId = this->m_SymbolId;
    symbolInfo.scopeId = this->m_ScopeId;
    symbolInfo.scopeLevel = this->m_ScopeLevel;
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
  symbolInfo.isCastable = true;

  symbolInfo.isNeededEval = true;

  if (!varAst.m_RHS) {
    // Well,the symbol it's not initialized.
    symbolInfo.isNeededEval = false;
  }

  // Do not need to look up the symbol table
  // for the declared name. since we can directly
  // take the information from decl.
  if (this->m_TypeChecking && symbolInfo.isNeededEval) {
    this->m_ExpectedType = varAst.m_TypeSpec;

    // Array intializer
    if (symbolInfo.isSubscriptable) {
      size_t dimension = 1;
      for (auto &v : varAst.m_ArrDim) {
        auto scalar = dynamic_cast<ScalarOrLiteralAST *>(v.get());
        symbolInfo.arrayDimensions.push_back(std::stoi(scalar->m_Value));
        std::reverse(symbolInfo.arrayDimensions.begin(),
                     symbolInfo.arrayDimensions.end());
        m_LastArrayDims = symbolInfo.arrayDimensions;
      }

      bool constant = varAst.m_ArrDim.size() == 1 &&
                      (varAst.m_RHS->m_ExprKind == ExprKind::ScalarExpr ||
                       varAst.m_RHS->m_ExprKind == ExprKind::SymbolExpr);

      if (varAst.m_RHS->m_ExprKind != ExprKind::BinOp &&
          varAst.m_RHS->m_ASTKind == ASTKind::Expr && !constant) {
        this->m_TypeErrorMessages.emplace_back(
            "Array initializer must be an initilizer list", symbolInfo);
      } else {
        if (constant) {
          auto scalar =
              dynamic_cast<ScalarOrLiteralAST *>(varAst.m_ArrDim[0].get());
          if (scalar->m_Value != "1") {
            this->m_TypeErrorMessages.emplace_back(
                "Array initializer does not meet with size of array",
                symbolInfo);
          }
        } else {
          std::vector<BinOpOrVal> vec;
          auto binaryExpr = dynamic_cast<BinaryOpAST *>(varAst.m_RHS.get());
          getElementsOfArray(*binaryExpr, vec);

          // size_t index = 1;
          // validateArray(*binaryExpr, 0, index);
        }
        m_LastArrayDims.clear();
      }
    }
    // --

    if (((symbolInfo.isConstPtr && !symbolInfo.isSubscriptable) &&
         (symbolInfo.indirectionLevel == 0 || symbolInfo.isRef)) ||
        (symbolInfo.isConstRef && !symbolInfo.isRef)) {
      // error
      this->m_TypeErrorMessages.emplace_back(
          "The type qualifier and the type of the variable does not meet",
          symbolInfo);
    }

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
  } else {
    if (varAst.m_RHS != nullptr) {
      this->m_LastSymbolInfo = symbolInfo;
      auto tempSmbolInfo = varAst.m_RHS->acceptBefore(*this);
      symbolInfo.ptrAliases = std::move(tempSmbolInfo.ptrAliases);
    }
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

      this->m_LastAssignment = true;
      this->m_LastSymbolInfo.symbolId = m_SymbolId;
      this->m_LastSymbolInfo.scopeId = m_ScopeId;
      this->m_LastSymbolInfo.scopeLevel = m_ScopeLevel;

      bool indirectable = false;
      if (symbolValidation(lhs->m_SymbolName, symbolInfo, matchedSymbol)) {
        // TODO: for different scenerarios.

        if (assignmentAst.m_IsDereferenced) {
          int a = static_cast<int>(matchedSymbol.indirectionLevel);
          int b = static_cast<int>(assignmentAst.m_DerefLevel);
          if (a - b < 0) {
            m_TypeErrorMessages.emplace_back(
                "It's not a indirectable or not a valid indirection level",
                symbolInfo);
          } else {
            matchedSymbol.indirectionLevel -= assignmentAst.m_DerefLevel;

            indirectable = true;
          }
        }

        if (assignmentAst.m_Subscriptable) {
          if (assignmentAst.m_SubscriptIndexes.size() !=
              matchedSymbol.arrayDimensions.size()) {
            m_TypeErrorMessages.emplace_back(
                "Array type with index(es) is not assignable", symbolInfo);
          } else {
            for (int i = 0; i < matchedSymbol.arrayDimensions.size(); ++i) {
              auto index = dynamic_cast<ScalarOrLiteralAST *>(
                  assignmentAst.m_SubscriptIndexes[i].get());
              auto indexVal = std::stoi(index->m_Value);
              if (indexVal > matchedSymbol.arrayDimensions[i]) {
                m_TypeWarningMessages.emplace_back(
                    "Array index '" + index->m_Value +
                        "' is after the beginning of the array",
                    symbolInfo);
              } else if (indexVal < 0) {
                m_TypeWarningMessages.emplace_back(
                    "Array index '" + index->m_Value +
                        "' is before the beginning of the array",
                    symbolInfo);
              }
            }
          }
        }

        //
        // type qualifier checking..
        //
        if (matchedSymbol.indirectionLevel == 0 &&
            (matchedSymbol.isConstRef || matchedSymbol.isConstVal)) {
          m_TypeErrorMessages.emplace_back(
              "const-qualified value is not assignable", symbolInfo);
        }

        if (matchedSymbol.isReadOnly) {
          m_TypeErrorMessages.emplace_back(
              "readonly-qualified symbol is neither assignable with a value "
              "nor pointer with a reference",
              symbolInfo);
        }

        if (matchedSymbol.indirectionLevel > 0 && matchedSymbol.isConstPtr) {
          m_TypeErrorMessages.emplace_back(
              "constptr-qualified pointer is not assignable", symbolInfo);
        } else {
          if (matchedSymbol.ptrAliases.count(assignmentAst.m_DerefLevel) > 0) {
            SymbolInfo indirectedSymbol;
            SymbolInfo &currentSymbol = matchedSymbol;
            size_t derefLevel = assignmentAst.m_DerefLevel;
            while (!currentSymbol.symbolName.empty()) {
              //              symbolValidation(currentSymbol.ptrAliases[derefLevel],symbolInfo,
              //              indirectedSymbol);
              if (currentSymbol.ptrAliases.count(derefLevel) > 0) {
                bool x = symbolValidation(currentSymbol.ptrAliases[derefLevel],
                                          symbolInfo, currentSymbol);
                derefLevel = currentSymbol.indirectionLevel;
              } else {
                break;
              }
            }

            if (currentSymbol.indirectionLevel == 0 &&
                (currentSymbol.isConstRef || currentSymbol.isConstVal)) {
              m_TypeErrorMessages.emplace_back(
                  "const-qualified value is not assignable", symbolInfo);
            }

            if (currentSymbol.isReadOnly) {
              m_TypeErrorMessages.emplace_back(
                  "readonly-qualified symbol is neither assignable with a "
                  "value "
                  "nor pointer with a reference",
                  symbolInfo);
            }

            if (currentSymbol.indirectionLevel > 0 &&
                currentSymbol.isConstPtr) {
              m_TypeErrorMessages.emplace_back(
                  "constptr-qualified pointer is not assignable", symbolInfo);
            }
          }
        }

        // ----------------------

        this->m_ExpectedType = matchedSymbol.type;

        if (matchedSymbol.type == TypeSpecifier::SPEC_DEFINED) {
          // definedTypeName...
          this->m_DefinedTypeFlag = true;
        }

        this->m_LastBinOpHasAtLeastOnePtr = false;
        this->m_LastSymbolInfo = matchedSymbol;

        //      this->m_LastIde
        //        if(indirectable)
        auto rhs = assignmentAst.m_RHS->acceptBefore(*this);

        // rhs symbol need to check

        this->m_LastReferenced = false;
        this->m_DereferenceLevel = 0;
      }

      this->m_LastAssignment = false;
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

  if (this->m_LastCondExpr || this->m_LastLoopDataSymbol ||
      this->m_LastLoopIndexSymbol) {
    this->m_LastSymbolInfo.symbolId = m_SymbolId;
    m_SymbolId += 1;
    this->m_LastSymbolInfo.scopeId = m_ScopeId;
    this->m_LastSymbolInfo.scopeLevel = m_ScopeLevel;
  }

  SymbolInfo matchedSymbol;
  if (this->m_TypeChecking) {
    if (!this->m_LastLoopDataSymbol && !this->m_LastLoopIndexSymbol &&
        !this->m_LastParamSymbol) {
      if (symbolValidation(symbolName, symbolInfo, matchedSymbol)) {
        this->m_MatchedSymbolType = matchedSymbol.type;

        if (!this->m_LastCondExpr && !this->m_LastFixExpr) {
          if (!matchedSymbol.isCastable &&
              matchedSymbol.type != this->m_LastSymbolInfo.type) {
            if (!matchedSymbol.isCastable) {
              this->m_TypeErrorMessages.emplace_back(
                  "Casting is not allowed for the symbol '" +
                      matchedSymbol.symbolName + "'",
                  symbolInfo);
            } else {
              this->m_TypeErrorMessages.emplace_back(
                  "Casting is not possible for the symbol '" +
                      matchedSymbol.symbolName + "'",
                  symbolInfo);
            }
          } else {
            if (matchedSymbol.type == this->m_LastSymbolInfo.type) {
              if (m_LastSubscriptable) {
                if (m_LastSubscriptable == matchedSymbol.isSubscriptable) {
                  // all arrays has indirection level since nature of being
                  // array.
                  matchedSymbol.indirectionLevel =
                      matchedSymbol.arrayDimensions.size();
                  // already checking in BinaryOpAst visitor..
                  //              int f =
                  //              static_cast<int>(matchedSymbol.arrayDimensions.size());
                  //              int s =
                  //              static_cast<int>(m_LastArrayIndexCount); if(f
                  //              - s < 0) {
                  //              }
                  if (m_LastSymbolInfo.indirectionLevel !=
                          (matchedSymbol.indirectionLevel -
                           m_LastArrayIndexCount -
                           (this->m_LastDereferenced ? this->m_DereferenceLevel
                                                     : 0) +
                           (m_LastReferenced ? 1 : 0)) &&
                      m_LastBinOp) {
                    symbolInfo.typeCheckerInfo.isCompatibleSubs = false;
                  } else if (m_LastSymbolInfo.indirectionLevel !=
                                 (matchedSymbol.indirectionLevel -
                                  m_LastArrayIndexCount +
                                  (m_LastReferenced ? 1 : 0)) &&
                             !m_LastBinOp) {
                    accumulateIncompatiblePtrErrMesg(symbolInfo);
                  }
                } else {
                  this->m_TypeErrorMessages.emplace_back(
                      "Incompatible array index(es)", symbolInfo);
                  accumulateIncompatiblePtrErrMesg(symbolInfo);
                }
              }

              if ((this->m_LastBinOp && matchedSymbol.indirectionLevel > 0 &&
                   (matchedSymbol.indirectionLevel - m_LastArrayIndexCount -
                    (this->m_LastDereferenced ? this->m_DereferenceLevel : 0) +
                    (m_LastReferenced ? 1 : 0)) ==
                       this->m_LastSymbolInfo.indirectionLevel)) {
                this->m_LastBinOpHasAtLeastOnePtr = true;
              }
              if (matchedSymbol.indirectionLevel == 0 &&
                  m_LastSymbolInfo.indirectionLevel == 1 &&
                  m_LastSymbolInfo.isRef && !this->m_LastBinOp) {
              } else {
                if (matchedSymbol.indirectionLevel +
                            (this->m_LastReferenced ? 1 : 0) -
                            (this->m_LastDereferenced ? this->m_DereferenceLevel
                                                      : 0) !=
                        this->m_LastSymbolInfo.indirectionLevel &&
                    this->m_LastBinOp) {
                  symbolInfo.typeCheckerInfo.isCompatiblePtr = false;
                } else if (matchedSymbol.indirectionLevel +
                                   (this->m_LastReferenced ? 1 : 0) -
                                   (this->m_LastDereferenced
                                        ? this->m_DereferenceLevel
                                        : 0) !=
                               this->m_LastSymbolInfo.indirectionLevel &&
                           !this->m_LastBinOp) {
                  accumulateIncompatiblePtrErrMesg(symbolInfo);
                }

                if ((this->m_LastBinOp && matchedSymbol.indirectionLevel > 0 &&
                     matchedSymbol.indirectionLevel +
                             (this->m_LastReferenced ? 1 : 0) -
                             (this->m_LastDereferenced
                                  ? this->m_DereferenceLevel
                                  : 0) ==
                         this->m_LastSymbolInfo.indirectionLevel)) {
                  this->m_LastBinOpHasAtLeastOnePtr = true;
                }
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

            // type qualifier of the variable decl
            if ((matchedSymbol.isConstVal != m_LastSymbolInfo.isConstVal) &&
                matchedSymbol.indirectionLevel != 0) {
              if (this->m_LastRetExpr) {
                this->m_TypeErrorMessages.emplace_back(
                    "Cannot return a variable with the different type of "
                    "qualifier",
                    symbolInfo);
              } else {
                this->m_TypeErrorMessages.emplace_back(
                    "Cannot initialize a variable with the different type of "
                    "qualifier",
                    symbolInfo);
              }
            }

            if (matchedSymbol.isConstRef != m_LastSymbolInfo.isConstRef) {
              if (this->m_LastRetExpr) {
                this->m_TypeErrorMessages.emplace_back(
                    "Cannot return a variable with the different type of "
                    "qualifier",
                    symbolInfo);
              } else {
                this->m_TypeErrorMessages.emplace_back(
                    "Cannot initialize a variable with the different type of "
                    "qualifier",
                    symbolInfo);
              }
            }

            if (matchedSymbol.isConstPtr != m_LastSymbolInfo.isConstPtr &&
                !matchedSymbol.isConstRef && !matchedSymbol.isConstVal) {
              if (this->m_LastRetExpr) {
                this->m_TypeErrorMessages.emplace_back(
                    "Cannot return a variable with the different type of "
                    "qualifier",
                    symbolInfo);
              } else {
                this->m_TypeErrorMessages.emplace_back(
                    "Cannot initialize a variable with the different type of "
                    "qualifier",
                    symbolInfo);
              }
            }
          }
        }
      } else {
        // Symbol could not validate obviously.
        // but it will be processed inside of the symbolValidation func.
      }
    } else {
      if (this->m_LastLoopDataSymbol || this->m_LastLoopIndexSymbol) {
        if (symbolValidation(symbolName, symbolInfo, matchedSymbol,
                             m_LastLoop)) {
          if (m_LastLoopIndexSymbol) {
            this->m_TypeWarningMessages.emplace_back(
                "The index symbol of the loop is shadowing a local or "
                "global variable",
                symbolInfo);
          } else {
            this->m_TypeWarningMessages.emplace_back(
                "The value symbol of the loop is shadowing a local or "
                "global variable",
                symbolInfo);
          }
        }
      }
    }
  } else {
    if (!this->m_LastLoopDataSymbol && !this->m_LastLoopIndexSymbol &&
        !this->m_LastParamSymbol) {
      symbolInfo.ptrAliases[this->m_LastSymbolInfo.indirectionLevel] =
          symbolName;
    }
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
                   (this->m_LastSymbolInfo.indirectionLevel == 0 ||
                    this->m_LastSymbolInfo.isConstRef &&
                        this->m_LastSymbolInfo.isRef)) {
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
        if (!m_LastCondExpr && !m_LastLoop) {
          this->accumulateIncompatiblePtrErrMesg(symbolInfo);
        }
      }

      // checking mutability
      if (m_LastSymbolInfo.isConstRef) {
        // okay
      } else if (m_LastSymbolInfo.isConstPtr) {
        // wrong
        //        this->m_TypeErrorMessages.emplace_back(
        //            "The value is not suitable with reference type",
        //            symbolInfo);
      } else if (m_LastSymbolInfo.isConstVal) {
        // okay
      } else if (m_LastSymbolInfo.isReadOnly) {
        // partially okay ( actually not really qualifying to the address of the
        // value. )
      }
    }
  } else {
    symbolInfo.value = scalarAst.m_Value;
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

    SymbolInfo rhsSymbol;
    if (binaryOpAst.m_BinOpKind == B_MARRS) {
      rhsSymbol.begin = binaryOpAst.m_SemLoc.begin;
      rhsSymbol.end = binaryOpAst.m_SemLoc.end;
      rhsSymbol.line = binaryOpAst.m_SemLoc.line;
      return rhsSymbol;
    } else {
      size_t indexCount = 0;
      size_t hasIndexes = false;
      if (binaryOpAst.m_BinOpKind == B_ARRS) {
        indexCount = this->getIndexesOfArray(*binaryOpAst.m_RHS.get());
        hasIndexes = true;
      } else {
        this->m_BinOpTermCount += 1;
        rhsSymbol = rhs->acceptBefore(*this);
        this->m_BinOpTermCount -= 1;
      }

      if (isPtrType) {
        if (!rhsSymbol.typeCheckerInfo.isCompatiblePtr &&
            (this->m_LastBinOp && !this->m_LastBinOpHasAtLeastOnePtr) &&
            this->m_BinOpTermCount == 0) {
          this->m_TypeErrorMessages.emplace_back(
              "Invalid operand '" + rhsSymbol.symbolName + "'" +
                  " of binary operation",
              rhsSymbol);
          rhsSymbol.typeCheckerInfo.isCompatiblePtr = true;
          errorFlag = true;
        }
      }

      // Array index op...
      if (this->m_LastBinOp && binaryOpAst.m_BinOpKind == B_ARRS) {
        SymbolInfo matchedSymbol;
        auto symbolName = ((SymbolAST *)binaryOpAst.m_LHS.get())->m_SymbolName;

        if (symbolValidation(symbolName, rhsSymbol, matchedSymbol)) {
          if (matchedSymbol.isSubscriptable &&
              indexCount <= matchedSymbol.arrayDimensions.size()) {
            m_LastArrayIndexCount = indexCount;
            m_LastSubscriptable = true;
          } else {
            if (indexCount > matchedSymbol.arrayDimensions.size()) {
              this->m_TypeErrorMessages.emplace_back("Invalid array index(es)",
                                                     rhsSymbol);
            }
          }
        }
      }

      this->m_BinOpTermCount += 1;
      auto lhsSymbol = lhs->acceptBefore(*this);
      this->m_BinOpTermCount -= 1;
      if (isPtrType && !errorFlag) {
        if (!lhsSymbol.typeCheckerInfo.isCompatiblePtr &&
            (this->m_LastBinOp && !this->m_LastBinOpHasAtLeastOnePtr) &&
            this->m_BinOpTermCount == 0) {
          this->m_TypeErrorMessages.emplace_back(
              "Invalid operand '" + lhsSymbol.symbolName + "'" +
                  " of binary operation",
              lhsSymbol);
          lhsSymbol.typeCheckerInfo.isCompatiblePtr = true;
        } else if (!lhsSymbol.typeCheckerInfo.isCompatibleSubs &&
                   (this->m_LastBinOp && !this->m_LastBinOpHasAtLeastOnePtr) &&
                   this->m_BinOpTermCount == 0) {
          this->m_TypeErrorMessages.emplace_back(
              "Invalid operand '" + lhsSymbol.symbolName + "'" +
                  " of binary operation",
              lhsSymbol);
        }
      }

      if (!lhsSymbol.typeCheckerInfo.isCompatibleSubs) {
        this->m_TypeErrorMessages.emplace_back("Invalid array index(es)",
                                               symbolInfo);
      }

      this->m_LastBinOp = false;
      this->m_LastArrayIndexCount = 0;
      this->m_LastSubscriptable = false;
    }
  } else {
    // nothing to do
  }

  return symbolInfo;
}
SymbolInfo Visitor::preVisit(CastOpAST &castOpAst) {
  SymbolInfo symbolInfo;

  return symbolInfo;
}

SymbolInfo Visitor::preVisit(FuncAST &funcAst) {
  SymbolInfo symbolInfo;

  if (funcAst.m_RetType != nullptr) {
    auto retType = dynamic_cast<TypeAST *>(funcAst.m_RetType.get());

    symbolInfo.type = retType->m_TypeSpec;
    symbolInfo.indirectionLevel = retType->m_IndirectLevel;
    symbolInfo.isConstRef = funcAst.m_RetTypeQualifier == Q_CONSTREF;
    symbolInfo.isConstPtr = funcAst.m_RetTypeQualifier == Q_CONSTPTR;
    symbolInfo.isReadOnly = funcAst.m_RetTypeQualifier == Q_READONLY;
    symbolInfo.isConstVal = funcAst.m_RetTypeQualifier == Q_CONST;
    symbolInfo.isRef = retType->m_IsRef;
    symbolInfo.isUnique = retType->m_IsUniquePtr;

    m_LastFuncRetTypeInfo = symbolInfo;
  }

  symbolInfo.assocFuncName = funcAst.m_FuncName;
  symbolInfo.begin = funcAst.m_SemLoc.begin;
  symbolInfo.end = funcAst.m_SemLoc.end;
  symbolInfo.line = funcAst.m_SemLoc.line;

  if (m_TypeChecking) {
    this->m_LastScopeSymbols = LocalSymbolTable[funcAst.m_FuncName];
    for (auto &param : funcAst.m_Params) {
      auto symbol = param->acceptBefore(*this);
      //      typeCheckerScopeHandler(param);
    }

    enterScope(false);
    for (auto &node : funcAst.m_Scope) {
      typeCheckerScopeHandler(node);
    }
  } else {
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
        symbol.symbolScope = SymbolScope::Func;
        this->m_SymbolInfos.push_back(symbol);
      }
    }

    enterScope(false);
    for (auto &node : funcAst.m_Scope) {
      scopeHandler(node, SymbolScope::Func);
    }
  }

  exitScope(false);
  return symbolInfo;
}

SymbolInfo Visitor::preVisit(IfStmtAST &ifStmtAst) {
  SymbolInfo symbolInfo;

  auto scopeLevel = this->m_ScopeLevel;

  for (auto &block : ifStmtAst.m_Cond) {
    // type checking for condition
    enterScope(false);
    if (m_TypeChecking) {
      this->m_LastCondExpr = true;
      block.second.first->acceptBefore(*this);
      this->m_LastCondExpr = false;
    }

    // there is only one node actually..
    for (auto &node : block.second.second) {
      scopeHandler(node, SymbolScope::IfSt);
    }
    exitScope(false);
  }

  if (ifStmtAst.m_HasElif) {
    for (auto &entry : ifStmtAst.m_ElseIfs) {
      // type checking for condition
      enterScope(false);
      if (m_TypeChecking) {
        this->m_LastCondExpr = true;
        entry.second.first->acceptBefore(*this);
        this->m_LastCondExpr = false;
      }
      auto &elseIfBlock = entry.second;
      // manually increasing
      this->m_SymbolId++;
      for (auto &node : elseIfBlock.second) {
        scopeHandler(node, SymbolScope::IfSt);
      }
      exitScope(false);
    }
  }

  enterScope(false);

  if (ifStmtAst.m_HasElse) {
    // manually increasing
    this->m_SymbolId++;
    for (auto &node : ifStmtAst.m_Else) {
      scopeHandler(node, SymbolScope::IfSt);
    }
  }

  exitScope(false);
  return symbolInfo;
}

SymbolInfo Visitor::preVisit(LoopStmtAST &loopStmtAst) {
  SymbolInfo symbolInfo;
  enterScope(false);

  this->m_LastLoop = true;

  if (m_TypeChecking) {
    if (loopStmtAst.m_RangeLoop) {
      if (loopStmtAst.m_Indexable) {
        this->m_LastLoopIndexSymbol = true;
        loopStmtAst.m_IndexSymbol->acceptBefore(*this);
        this->m_LastLoopIndexSymbol = false;
      }

      if (loopStmtAst.m_HasNumericRange) {
        // To checking symbols are valid and exists..
        // if they are symbols...
        this->m_LastCondExpr = true;
        loopStmtAst.m_Min->acceptBefore(*this);
        loopStmtAst.m_Max->acceptBefore(*this);
        this->m_LastCondExpr = false;
      } else {
        // TODO: Need to extra check for iterable data.
        this->m_LastLoopIter = true;
        if (loopStmtAst.m_IterSymbol->m_ExprKind == SymbolExpr) {
          // loopStmtAst.m_IterSymbol->acceptBefore(*this);
          this->m_LastLoopIter = false;
          auto iterSymbol = (SymbolAST *)loopStmtAst.m_IterSymbol.get();
          SymbolInfo matchedSymbol;
          if (symbolValidation(iterSymbol->m_SymbolName, symbolInfo,
                               matchedSymbol)) {
            if (!matchedSymbol.isSubscriptable) {
              symbolInfo.begin = iterSymbol->m_SemLoc.begin;
              symbolInfo.end = iterSymbol->m_SemLoc.end;
              symbolInfo.line = iterSymbol->m_SemLoc.line;
              this->m_TypeErrorMessages.emplace_back(
                  "Symbol must be iterable or provided a sequenceable trait "
                  "(built-in trait)",
                  symbolInfo);
            }
          }
        } else {
          this->m_TypeErrorMessages.emplace_back(
              "Iterable version of loop needs an symbol which is also iterable",
              symbolInfo);
        }
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

  for (auto &node : loopStmtAst.m_Scope) {
    scopeHandler(node, SymbolScope::LoopSt);
  }

  this->m_LastLoop = false;
  exitScope(false);

  return symbolInfo;
}

SymbolInfo Visitor::preVisit(FuncCallAST &funcCallAst) {
  SymbolInfo symbolInfo;
  // TODO: return type checking...

  return symbolInfo;
}

SymbolInfo Visitor::preVisit(ParamAST &paramAst) {
  SymbolInfo symbolInfo;

  symbolInfo.isParam = this->m_LastParamSymbol = true;

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
  }

  auto typeInfo = dynamic_cast<TypeAST *>(paramAst.m_TypeNode.get());

  this->m_LastSymbolInfo.symbolId = m_SymbolId;
  this->m_SymbolId += 1;
  this->m_LastSymbolInfo.scopeId = m_ScopeId;
  this->m_LastSymbolInfo.scopeLevel = m_ScopeLevel;

  symbolInfo.isSubscriptable = paramAst.m_IsSubscriptable;
  symbolInfo.isConstRef = paramAst.m_TypeQualifier == Q_CONSTREF;
  symbolInfo.isConstPtr = paramAst.m_TypeQualifier == Q_CONSTPTR;
  symbolInfo.isReadOnly = paramAst.m_TypeQualifier == Q_READONLY;
  symbolInfo.isConstVal = paramAst.m_TypeQualifier == Q_CONST;
  symbolInfo.isCastable = paramAst.m_IsCastable;

  if (typeInfo != nullptr) {
    symbolInfo.isRef = typeInfo->m_IsRef;
    symbolInfo.isUnique = typeInfo->m_IsUniquePtr;
    symbolInfo.indirectionLevel = typeInfo->m_IndirectLevel;
  }

  if (paramAst.m_IsSubscriptable) {
    for (auto &v : paramAst.m_ArrDim) {
      auto scalar = dynamic_cast<ScalarOrLiteralAST *>(v.get());
      symbolInfo.arrayDimensions.push_back(std::stoi(scalar->m_Value));
    }
  }

  m_LastParamSymbol = false;

  if (paramAst.m_IsPrimitive) {
    symbolInfo.type = typeInfo->m_TypeSpec;
  } else {
    symbolInfo.type = TypeSpecifier::SPEC_DEFINED;
  }

  if (m_TypeChecking) {
    if (((symbolInfo.isConstPtr && !symbolInfo.isSubscriptable) &&
         (symbolInfo.indirectionLevel == 0 || symbolInfo.isRef)) ||
        (symbolInfo.isConstRef && !symbolInfo.isRef)) {
      // error
      this->m_TypeErrorMessages.emplace_back(
          "The type qualifier and the type of the variable does not meet",
          symbolInfo);
    }
  }

  // symbolInfo.isNeededEval = true;

  return symbolInfo;
}
SymbolInfo Visitor::preVisit(RetAST &retAst) {
  SymbolInfo symbolInfo;

  if (m_TypeChecking) {
    this->m_LastRetExpr = true;
    m_LastSymbolInfo = m_LastFuncRetTypeInfo;
    m_ExpectedType = m_LastFuncRetTypeInfo.type;
    this->m_LastSymbolInfo.symbolId = m_SymbolId;
    this->m_LastSymbolInfo.scopeId = m_ScopeId;
    this->m_LastSymbolInfo.scopeLevel = m_ScopeLevel;
    if (retAst.m_RetExpr != nullptr) retAst.m_RetExpr->acceptBefore(*this);
    this->m_LastRetExpr = false;
  }

  return symbolInfo;
}

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
      case U_DEREF: {
        this->m_LastDereferenced = true;
        symbolInfo = unaryOpAst.m_Node->acceptBefore(*this);
        if (unaryOpAst.m_Node->m_ExprKind == ExprKind::SymbolExpr) {
          this->m_LastDereferenced = false;
          this->m_DereferenceLevel = 1;
        } else {
          this->m_DereferenceLevel += 1;
        }
        break;
      }
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
  } else {
    this->m_LastReferenced = true;
    symbolInfo = unaryOpAst.m_Node->acceptBefore(*this);
    this->m_LastReferenced = false;
  }

  return symbolInfo;
}
SymbolInfo Visitor::preVisit(TypeAST &typeAst) {
  SymbolInfo symbolInfo;

  symbolInfo.type = typeAst.m_TypeSpec;
  symbolInfo.isPrimitive = typeAst.m_IsPrimitiveType;

  return symbolInfo;
}

SymbolInfo Visitor::preVisit(FixAST &fixAst) {
  SymbolInfo symbolInfo;

  // Problem symbolId den kaynaklanıyor. Validate ederken
  // symbol ıd si olmadığı için hata veriyor. Bunun uzerinde düşünmen
  // gerekiyor.
  Visitor::SymbolId += 1;
  if (m_TypeChecking) {
    this->m_LastFixExpr = true;
    this->m_LastSymbolInfo = symbolInfo;
    fixAst.m_Symbol->acceptBefore(*this);
    this->m_LastFixExpr = false;
  }

  return symbolInfo;
}

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
  } else if (node->m_ASTKind == ASTKind::Expr &&
             node->m_ExprKind == ExprKind::RetExpr) {
    node->acceptBefore(*this);
  } else if (node->m_ASTKind == ASTKind::Expr &&
             node->m_ExprKind == ExprKind::FixExpr) {
    node->acceptBefore(*this);
  }
}

bool Visitor::symbolValidation(std::string &symbolName, SymbolInfo &symbolInfo,
                               SymbolInfo &matchedSymbol, bool noError) {
  bool isLocalSymbol = false;
  bool isGlobSymbol = false;

  size_t index = this->m_LastSymbolInfo.symbolId;
  size_t scopeId = this->m_LastSymbolInfo.scopeId;
  size_t scopeLevel = this->m_LastSymbolInfo.scopeLevel;

  for (auto &it : GlobalSymbolTable) {
    if (it.symbolName == symbolName) {
      if (it.symbolInfo.scopeLevel < scopeLevel) {
        // nothing
      } else {
        if (it.symbolInfo.scopeLevel == scopeLevel) {
          if (it.symbolInfo.symbolId > index ||
              it.symbolInfo.scopeId != scopeId) {
            goto pitfallGlob;
          } else {
            goto doneGlob;
          }
        } else {
          continue;
        }
      pitfallGlob:
        if (!noError) {
          this->m_TypeWarningMessages.emplace_back(
              "'" + symbolName + "' was not in a valid place", it.symbolInfo);
          this->m_TypeErrorMessages.emplace_back(
              "'" + symbolName +
                  "' was declared in the wrong place. Probably it used "
                  "before "
                  "declaration",
              symbolInfo);
        }
        break;
      }
    doneGlob:
      matchedSymbol = it.symbolInfo;
      isGlobSymbol = true;
      break;
    }
  }

  for (auto &it : this->m_LastScopeSymbols) {
    if (it.symbolName == symbolName) {
      if (it.symbolInfo.scopeLevel < scopeLevel) {
        // nothing
      } else {
        if (it.symbolInfo.scopeLevel == scopeLevel) {
          if (scopeLevel == 1) {
            if (it.symbolInfo.symbolId > index) {
              goto pitfall;
            } else {
              goto done;
            }
          } else {
            if (it.symbolInfo.symbolId > index ||
                it.symbolInfo.scopeId != scopeId) {
              goto pitfall;
            } else {
              goto done;
            }
          }
        } else {
          continue;
        }
      pitfall:
        if (!noError) {
          this->m_TypeWarningMessages.emplace_back(
              "'" + symbolName + "' was not in a valid place", it.symbolInfo);
          this->m_TypeErrorMessages.emplace_back(
              "'" + symbolName +
                  "' was declared in the wrong place. Probably it used "
                  "before "
                  "declaration",
              symbolInfo);
        }
        break;
      }
    done:
      matchedSymbol = it.symbolInfo;
      isLocalSymbol = true;
      break;
    }
  }

  if (!isLocalSymbol && !isGlobSymbol && !noError) {
    this->m_TypeErrorMessages.emplace_back(
        "'" + symbolName + "' was not declared in this scope or global scope",
        symbolInfo);
    return false;
  }

  if (noError && !isLocalSymbol && !isGlobSymbol) {
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