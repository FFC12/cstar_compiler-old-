#include <ast/assignment_ast.hpp>
#include <ast/ast.hpp>
#include <ast/binary_op_ast.hpp>
#include <ast/cast_op_ast.hpp>
#include <ast/control_flow_ast.hpp>
#include <ast/fix_ast.hpp>
#include <ast/func_ast.hpp>
#include <ast/func_call_ast.hpp>
#include <ast/if_stmt.hpp>
#include <ast/loop_stmt.hpp>
#include <ast/new_ast.hpp>
#include <ast/param_ast.hpp>
#include <ast/ret_ast.hpp>
#include <ast/scalar_ast.hpp>
#include <ast/struct_ast.hpp>
#include <ast/symbol_ast.hpp>
#include <ast/trait_ast.hpp>
#include <ast/type_ast.hpp>
#include <ast/unary_op_ast.hpp>
#include <ast/var_ast.hpp>
#include <visitor/visitor.hpp>

#include <algorithm>
#include <set>

using DiagnosticCode = cstar::diagnostics::DiagnosticCode;

static std::string SymbolStateKey(const SymbolInfo &symbolInfo) {
  return std::to_string(symbolInfo.scopeId) + "#" +
         std::to_string(symbolInfo.scopeLevel) + "#" + symbolInfo.symbolName;
}

static bool SplitStructMethodName(const std::string &name,
                                  std::string &owner,
                                  std::string &method) {
  const auto dot = name.find('.');
  if (dot == std::string::npos || dot == 0 || dot + 1 >= name.size()) {
    return false;
  }

  owner = name.substr(0, dot);
  method = name.substr(dot + 1);
  return true;
}

static std::string ValueOperatorMethodName(BinOpKind kind) {
  switch (kind) {
    case B_ADD:
      return "operator+";
    case B_SUB:
      return "operator-";
    case B_MUL:
      return "operator*";
    case B_DIV:
      return "operator/";
    case B_MOD:
      return "operator%";
    case B_EQ:
      return "operator==";
    case B_NEQ:
      return "operator!=";
    case B_LT:
      return "operator<";
    case B_LTEQ:
      return "operator<=";
    case B_GT:
      return "operator>";
    case B_GTEQ:
      return "operator>=";
    default:
      return {};
  }
}

IAST *Visitor::methodCallReceiver(IAST *node) const {
  if (node == nullptr || node->m_ExprKind != ExprKind::BinOp) {
    return nullptr;
  }

  auto *binOp = static_cast<BinaryOpAST *>(node);
  if (binOp->m_BinOpKind != BinOpKind::B_DOT ||
      binOp->m_LHS == nullptr || binOp->m_RHS == nullptr ||
      binOp->m_LHS->m_ExprKind != ExprKind::SymbolExpr ||
      binOp->m_RHS->m_ExprKind != ExprKind::SymbolExpr) {
    return nullptr;
  }

  auto *receiver = static_cast<SymbolAST *>(binOp->m_LHS.get());
  if (ModuleAliases.count(receiver->m_SymbolName) != 0) {
    return nullptr;
  }

  return binOp->m_LHS.get();
}

bool Visitor::constructorInitializer(VarAST &varAst,
                                     std::string &constructorName) {
  if (varAst.m_TypeSpec != TypeSpecifier::SPEC_DEFINED ||
      varAst.m_IndirectLevel != 0 || varAst.m_RHS == nullptr ||
      varAst.m_RHS->m_ExprKind != ExprKind::FuncCallExpr ||
      varAst.m_Typename == nullptr ||
      varAst.m_Typename->m_ExprKind != ExprKind::SymbolExpr) {
    return false;
  }

  auto *typeSymbol = static_cast<SymbolAST *>(varAst.m_Typename.get());
  auto *call = static_cast<FuncCallAST *>(varAst.m_RHS.get());
  if (call->m_FuncSymbol == nullptr ||
      call->m_FuncSymbol->m_ExprKind != ExprKind::SymbolExpr) {
    return false;
  }

  auto *calledType = static_cast<SymbolAST *>(call->m_FuncSymbol.get());
  if (calledType->m_SymbolName != typeSymbol->m_SymbolName) {
    return false;
  }

  constructorName = typeSymbol->m_SymbolName + ".constructor";
  return true;
}

static std::vector<TypeQualifier> BuildQualifierLevels(
    TypeQualifier qualifier, size_t indirectionLevel, bool isRef) {
  std::vector<TypeQualifier> levels(indirectionLevel + 1, Q_NONE);

  switch (qualifier) {
    case Q_CONST:
      levels[0] = Q_CONST;
      break;
    case Q_CONSTREF:
      if (isRef) {
        levels[0] = Q_CONSTREF;
      }
      break;
    case Q_CONSTPTR:
      if (indirectionLevel > 0 && !isRef) {
        levels[indirectionLevel] = Q_CONSTPTR;
      }
      break;
    case Q_READONLY:
      std::fill(levels.begin(), levels.end(), Q_READONLY);
      break;
    case Q_NONE:
      break;
  }

  return levels;
}

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
      return 0;
    case SPEC_NIL:
      return 8;
    case SPEC_DEFINED:
      assert(false && "Not implemented yet!");
      return 0;
    default:
      assert(false && "Unreacheable");
      return 0;
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
      return "<not-implemented>";

    case SPEC_NIL:
      assert(false && "!");
      return "nil";
    case SPEC_DEFINED:
      return "defined";
    default:
      assert(false && "Unreacheable");
      return "<invalid>";
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

static bool IsIntegerType(TypeSpecifier typeSpecifier) {
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
      return true;
    default:
      return false;
  }
}

static bool IsFloatingType(TypeSpecifier typeSpecifier) {
  switch (typeSpecifier) {
    case SPEC_F32:
    case SPEC_F64:
    case SPEC_FLOAT:
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
        auto expr = (uintptr_t)(SymbolAST *)rhs;
        vector.emplace_back(false, true, expr, "");
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
        auto expr = (uintptr_t)(SymbolAST *)rhs;
        vector.emplace_back(false, true, expr, "");
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
      auto expr = (uintptr_t)(SymbolAST *)&binaryExpr;
      vector.emplace_back(false, true, expr, "");
    } else if (binaryExpr.m_ExprKind == BinOp) {
      auto expr = (uintptr_t)((BinaryOpAST *)&binaryExpr);
      vector.emplace_back(true, false, expr, "");
    } else {
      assert(false && "Unimplemented array expr is involved!");
    }
  }
}

// FIXME: Need to fixed.
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

  return true;
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

  if (this->m_LastSymbolInfo.isRef) {
    indirection += "&";
  } else if (!this->m_LastSymbolInfo.isConstRef) {
    for (int i = 0; i < this->m_LastSymbolInfo.indirectionLevel; i++) {
      if (this->m_LastSymbolInfo.isUnique) {
        indirection += "^";
      } else {
        indirection += "*";
      }
    }
  }

  auto extra = !this->m_LastSymbolInfo.isConstPtr &&
                       !this->m_LastSymbolInfo.isConstVal &&
                       this->m_ExpectedType == TypeSpecifier::SPEC_CHAR
                   ? "String must have a 'const', 'constptr' or 'readonly' qualifier"
                   : "";

  if (s.empty()) {
    this->m_TypeErrorMessages.emplace_back(
        "Incompatible type. Expected a suitable value with '" +
            (typeQualifier.empty() ? "" : (typeQualifier + " ")) +
            GetTypeStr(this->m_ExpectedType) + indirection + "'. " + extra,
        symbolInfo,
        extra[0] == '\0' ? DiagnosticCode::SemanticError
                         : DiagnosticCode::SemanticQualifierMismatch);
  } else {
    this->m_TypeErrorMessages.emplace_back(
        s + " '" + (typeQualifier.empty() ? "" : (typeQualifier + " ")) +
            GetTypeStr(this->m_ExpectedType) + indirection + "'",
        symbolInfo);
  }
}

SymbolInfo Visitor::preVisit(VarAST &varAst) {
  SymbolInfo symbolInfo;
  const auto previousExpectedType = m_ExpectedType;
  const auto previousLastSymbolInfo = m_LastSymbolInfo;
  const auto previousDefinedTypeFlag = m_DefinedTypeFlag;
  const auto previousDefinedTypeName = m_DefinedTypeName;
  const auto previousLastBinOpHasPtr = m_LastBinOpHasAtLeastOnePtr;
  const auto previousLastVarDecl = m_LastVarDecl;

  symbolInfo.symbolName = varAst.m_Name;
  symbolInfo.begin = varAst.m_SemLoc.begin;
  symbolInfo.end = varAst.m_SemLoc.end;
  symbolInfo.line = varAst.m_SemLoc.line;

  this->m_LastVarDecl = true;
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
  if (varAst.m_TypeSpec == TypeSpecifier::SPEC_DEFINED &&
      varAst.m_Typename != nullptr &&
      varAst.m_Typename->m_ExprKind == ExprKind::SymbolExpr) {
    auto *typeSymbol = static_cast<SymbolAST *>(varAst.m_Typename.get());
    symbolInfo.definedTypeName = typeSymbol->m_SymbolName;
  }
  symbolInfo.isSubscriptable = varAst.m_IsInitializerList;
  symbolInfo.indirectionLevel = varAst.m_IndirectLevel;
  symbolInfo.isConstRef = varAst.m_TypeQualifier == Q_CONSTREF;
  symbolInfo.isConstPtr = varAst.m_TypeQualifier == Q_CONSTPTR;
  symbolInfo.isReadOnly = varAst.m_TypeQualifier == Q_READONLY;
  symbolInfo.isConstVal = varAst.m_TypeQualifier == Q_CONST;
  symbolInfo.isRef = varAst.m_IsRef;
  symbolInfo.isUnique = varAst.m_IsUniquePtr;
  symbolInfo.isCastable = true;
  symbolInfo.isPublic = varAst.m_AccessSpec == ACCESS_PUBLIC;
  symbolInfo.isStatic = varAst.m_IsStatic ||
                        varAst.m_VisibilitySpec == VisibilitySpecifier::VIS_STATIC;
  symbolInfo.isExported =
      varAst.m_VisibilitySpec == VisibilitySpecifier::VIS_EXPORT;
  symbolInfo.isImported =
      varAst.m_VisibilitySpec == VisibilitySpecifier::VIS_IMPORT;
  symbolInfo.qualifierLevels =
      BuildQualifierLevels(varAst.m_TypeQualifier, symbolInfo.indirectionLevel,
                           symbolInfo.isRef);

  symbolInfo.isNeededEval = true;

  if (!varAst.m_RHS) {
    // Well,the symbol it's not initialized.
    symbolInfo.isNeededEval = false;
  }

  if (symbolInfo.isSubscriptable) {
    for (auto &v : varAst.m_ArrDim) {
      auto scalar = dynamic_cast<ScalarOrLiteralAST *>(v.get());
      symbolInfo.arrayDimensions.push_back(std::stoi(scalar->m_Value));
    }
    m_LastArrayDims = symbolInfo.arrayDimensions;
  }

  // Do not need to look up the symbol table
  // for the declared name. since we can directly
  // take the information from decl.
  if (this->m_TypeChecking && symbolInfo.isNeededEval) {
    this->m_ExpectedType = varAst.m_TypeSpec;

    // Array intializer
    if (symbolInfo.isSubscriptable) {
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
          "Invalid qualifier/type combination",
          symbolInfo, DiagnosticCode::SemanticInvalidQualifier);
    }

    if (varAst.m_TypeSpec == TypeSpecifier::SPEC_DEFINED) {
      if (varAst.m_Typename->m_ExprKind == ExprKind::SymbolExpr) {
        auto typeName =
            dynamic_cast<SymbolAST *>(varAst.m_Typename.get())->m_SymbolName;
        this->m_DefinedTypeName = typeName;
        this->m_DefinedTypeFlag = true;
        if (m_TypeChecking && this->m_TypeTable.count(typeName) == 0) {
          this->m_TypeErrorMessages.emplace_back(
              "Unknown type '" + typeName + "'", symbolInfo);
        }
      }
    }

    // reset all state flags.
    this->m_LastBinOpHasAtLeastOnePtr = false;

    this->m_LastSymbolInfo = symbolInfo;
    auto rhsAsSymbol = dynamic_cast<SymbolAST *>(varAst.m_RHS.get());
    auto rhsAsUnary = dynamic_cast<UnaryOpAST *>(varAst.m_RHS.get());
    const bool rhsIsMoveExpr =
        rhsAsUnary != nullptr && rhsAsUnary->m_UnaryOpKind == U_MOVE;
    std::string constructorName;
    const bool isConstructorInitializer =
        constructorInitializer(varAst, constructorName);

    auto getPointerSource = [&](SymbolInfo &source) -> bool {
      SymbolAST *sourceSymbol = rhsAsSymbol;
      if (rhsIsMoveExpr) {
        sourceSymbol = dynamic_cast<SymbolAST *>(rhsAsUnary->m_Node.get());
      }

      if (sourceSymbol == nullptr) {
        return false;
      }

      SymbolInfo lookup;
      auto sourceName = sourceSymbol->m_SymbolName;
      if (!symbolValidation(sourceName, lookup, source, true)) {
        return false;
      }

      return source.indirectionLevel > 0;
    };

    SymbolInfo uniqueMoveSource;
    bool markUniqueMoveSource = false;
    if (varAst.m_IsMoveInit) {
      SymbolInfo source;
      if (symbolInfo.indirectionLevel == 0) {
        this->m_TypeErrorMessages.emplace_back(
            "':=' move initializer requires a pointer target",
            symbolInfo, DiagnosticCode::SemanticOwnership);
      } else if (!getPointerSource(source)) {
        this->m_TypeErrorMessages.emplace_back(
            "':=' move initializer requires a pointer source",
            symbolInfo, DiagnosticCode::SemanticOwnership);
      } else if (source.isNoMove) {
        this->m_TypeErrorMessages.emplace_back(
            "'nomove' pointer cannot be moved", symbolInfo,
            DiagnosticCode::SemanticOwnership);
      } else if (symbolInfo.isUnique != source.isUnique) {
        this->m_TypeErrorMessages.emplace_back(
            "':=' move initializer requires matching pointer ownership kind",
            symbolInfo, DiagnosticCode::SemanticOwnership);
      } else {
        uniqueMoveSource = source;
        markUniqueMoveSource = true;
      }
    } else if (symbolInfo.isUnique && rhsAsSymbol != nullptr) {
      SymbolInfo source;
      if (getPointerSource(source) && source.isUnique) {
        this->m_TypeErrorMessages.emplace_back(
            "unique pointer cannot be copied; use ':=' or 'move' to transfer "
            "ownership",
            symbolInfo, DiagnosticCode::SemanticOwnership);
      }
    }

    if (isConstructorInitializer) {
      if (!varAst.m_IsLocal) {
        this->m_TypeErrorMessages.emplace_back(
            "Constructor initializer is only supported for local struct "
            "variables",
            symbolInfo);
      }

      auto signatureIt = FunctionTable.find(constructorName);
      if (signatureIt == FunctionTable.end()) {
        this->m_TypeErrorMessages.emplace_back(
            "Struct '" + symbolInfo.definedTypeName +
                "' does not declare a constructor",
            symbolInfo);
      } else {
        auto *call = static_cast<FuncCallAST *>(varAst.m_RHS.get());
        std::vector<IAST *> argNodes;
        auto collectArgs = [&](auto &self, IAST *node) -> void {
          if (node == nullptr) {
            return;
          }

          if (node->m_ExprKind == ExprKind::BinOp) {
            auto *binOp = static_cast<BinaryOpAST *>(node);
            if (binOp->m_BinOpKind == BinOpKind::B_COMM) {
              self(self, binOp->m_LHS.get());
              self(self, binOp->m_RHS.get());
              return;
            }
          }

          argNodes.push_back(node);
        };
        collectArgs(collectArgs, call->m_Args.get());

        const auto expectedArgs =
            signatureIt->second.params.empty()
                ? 0
                : signatureIt->second.params.size() - 1;
        if (argNodes.size() != expectedArgs) {
          this->m_TypeErrorMessages.emplace_back(
              "Constructor for '" + symbolInfo.definedTypeName + "' expects " +
                  std::to_string(expectedArgs) + " argument(s), but " +
                  std::to_string(argNodes.size()) + " provided",
              symbolInfo);
        } else {
          for (size_t i = 0; i < argNodes.size(); ++i) {
            const auto &param = signatureIt->second.params[i + 1];
            m_LastSymbolInfo = param;
            if (m_LastSymbolInfo.isRef) {
              m_LastSymbolInfo.indirectionLevel += 1;
            }
            m_LastSymbolInfo.symbolId = m_SymbolId;
            m_LastSymbolInfo.scopeId = m_ScopeId;
            m_LastSymbolInfo.scopeLevel = m_ScopeLevel;
            m_ExpectedType = param.type;
            m_DefinedTypeFlag = param.type == TypeSpecifier::SPEC_DEFINED;
            argNodes[i]->acceptBefore(*this);
          }
        }
      }
    } else {
      auto tempSymbolInfo = varAst.m_RHS->acceptBefore(*this);
      symbolInfo.ptrAliases = std::move(tempSymbolInfo.ptrAliases);
      if (markUniqueMoveSource) {
        m_MovedUniqueSymbols.insert(SymbolStateKey(uniqueMoveSource));
        m_MovedUniqueSymbols.erase(SymbolStateKey(symbolInfo));
      } else if (symbolInfo.indirectionLevel > 0) {
        m_MovedUniqueSymbols.erase(SymbolStateKey(symbolInfo));
      }
    }
  } else {
    if (varAst.m_RHS != nullptr) {
      this->m_LastSymbolInfo = symbolInfo;
      auto tempSmbolInfo = varAst.m_RHS->acceptBefore(*this);
      symbolInfo.ptrAliases = std::move(tempSmbolInfo.ptrAliases);
    }
  }

  this->m_LastVarDecl = previousLastVarDecl;
  this->m_ExpectedType = previousExpectedType;
  this->m_LastSymbolInfo = previousLastSymbolInfo;
  this->m_DefinedTypeFlag = previousDefinedTypeFlag;
  this->m_DefinedTypeName = previousDefinedTypeName;
  this->m_LastBinOpHasAtLeastOnePtr = previousLastBinOpHasPtr;

  return symbolInfo;
}

SymbolInfo Visitor::preVisit(AssignmentAST &assignmentAst) {
  SymbolInfo symbolInfo;
  const auto previousExpectedType = m_ExpectedType;
  const auto previousLastSymbolInfo = m_LastSymbolInfo;
  const auto previousDefinedTypeFlag = m_DefinedTypeFlag;
  const auto previousLastBinOpHasPtr = m_LastBinOpHasAtLeastOnePtr;
  const auto previousLastAssignment = m_LastAssignment;

  if (m_TypeChecking) {
    if (assignmentAst.m_LHS->m_ASTKind != ASTKind::Expr) {
      assert(false && "Assignment target must be an expression.");
    } else {
      SymbolInfo matchedSymbol;
      SymbolAST *lhs = nullptr;
      bool isStructFieldAssignment = false;
      std::string lhsName;
      this->m_LastAssignment = true;
      const size_t assignmentSymbolId = m_SymbolId;
      const size_t assignmentScopeId = m_ScopeId;
      const size_t assignmentScopeLevel = m_ScopeLevel;
      this->m_LastSymbolInfo.symbolId = assignmentSymbolId;
      this->m_LastSymbolInfo.scopeId = assignmentScopeId;
      this->m_LastSymbolInfo.scopeLevel = assignmentScopeLevel;

      if (assignmentAst.m_LHS->m_ExprKind == ExprKind::SymbolExpr) {
        lhs = static_cast<SymbolAST *>(assignmentAst.m_LHS.get());
        lhsName = lhs->m_SymbolName;
        symbolInfo.symbolName = lhs->m_SymbolName;
        symbolInfo.begin = lhs->m_SemLoc.begin;
        symbolInfo.end = lhs->m_SemLoc.end;
        symbolInfo.line = lhs->m_SemLoc.line;
      } else if (assignmentAst.m_LHS->m_ExprKind == ExprKind::BinOp) {
        auto resolveFieldAccess =
            [&](auto &self, IAST *node) -> SymbolInfo {
          SymbolInfo resolved;
          if (node == nullptr) {
            this->m_TypeErrorMessages.emplace_back(
                "Assignment target must be a symbol or struct field",
                symbolInfo);
            return resolved;
          }

          if (node->m_ExprKind == ExprKind::SymbolExpr) {
            auto *symbol = static_cast<SymbolAST *>(node);
            SymbolInfo lookup;
            lookup.symbolName = symbol->m_SymbolName;
            lookup.begin = symbol->m_SemLoc.begin;
            lookup.end = symbol->m_SemLoc.end;
            lookup.line = symbol->m_SemLoc.line;
            symbolValidation(symbol->m_SymbolName, lookup, resolved);
            if (resolved.type == TypeSpecifier::SPEC_DEFINED &&
                resolved.indirectionLevel > 0) {
              resolved.indirectionLevel = 0;
              resolved.isUnique = false;
              resolved.isRef = false;
            }
            return resolved;
          }

          if (node->m_ExprKind != ExprKind::BinOp) {
            this->m_TypeErrorMessages.emplace_back(
                "Assignment target must be a symbol or struct field",
                symbolInfo);
            return resolved;
          }

          auto *fieldAccess = static_cast<BinaryOpAST *>(node);
          if (fieldAccess->m_BinOpKind != B_DOT ||
              fieldAccess->m_RHS == nullptr ||
              fieldAccess->m_RHS->m_ExprKind != ExprKind::SymbolExpr) {
            this->m_TypeErrorMessages.emplace_back(
                "Assignment target must be a symbol or struct field",
                symbolInfo);
            return resolved;
          }

          auto base = self(self, fieldAccess->m_LHS.get());
          auto *fieldSymbol = static_cast<SymbolAST *>(fieldAccess->m_RHS.get());
          if (base.type != TypeSpecifier::SPEC_DEFINED) {
            this->m_TypeErrorMessages.emplace_back(
                "Field assignment requires a struct value", base);
            return resolved;
          }

          auto structIt = StructTable.find(base.definedTypeName);
          if (structIt == StructTable.end()) {
            this->m_TypeErrorMessages.emplace_back(
                "Unknown struct type '" + base.definedTypeName + "'", base);
            return resolved;
          }

          auto fieldIt =
              structIt->second.fieldIndexes.find(fieldSymbol->m_SymbolName);
          if (fieldIt == structIt->second.fieldIndexes.end()) {
            this->m_TypeErrorMessages.emplace_back(
                "Struct '" + base.definedTypeName + "' has no field '" +
                    fieldSymbol->m_SymbolName + "'",
                base);
            return resolved;
          }

          const auto &field = structIt->second.fields[fieldIt->second];
          resolved = base;
          resolved.symbolName = base.symbolName + "." + field.name;
          resolved.type = field.type;
          resolved.definedTypeName = field.definedTypeName;
          resolved.indirectionLevel = field.indirectionLevel;
          resolved.isUnique = field.isUnique;
          resolved.isRef = field.isRef;
          resolved.isSubscriptable = false;
          resolved.arrayDimensions.clear();
          resolved.isConstVal = false;
          resolved.isConstPtr = false;
          resolved.isConstRef = false;
          resolved.isReadOnly = false;
          resolved.begin = fieldSymbol->m_SemLoc.begin;
          resolved.end = fieldSymbol->m_SemLoc.end;
          resolved.line = fieldSymbol->m_SemLoc.line;
          return resolved;
        };

        matchedSymbol =
            resolveFieldAccess(resolveFieldAccess, assignmentAst.m_LHS.get());
        lhsName = matchedSymbol.symbolName;
        symbolInfo = matchedSymbol;
        isStructFieldAssignment = true;
      } else {
        this->m_TypeErrorMessages.emplace_back(
            "Assignment target must be a symbol or struct field", symbolInfo);
        return symbolInfo;
      }

      bool indirectable = false;
      if (isStructFieldAssignment ||
          symbolValidation(lhsName, symbolInfo, matchedSymbol)) {
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
              if (index == nullptr) {
                continue;
              }
              auto indexVal = std::stoi(index->m_Value);
              if (indexVal >= matchedSymbol.arrayDimensions[i]) {
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
              "const-qualified value is not assignable", symbolInfo,
              DiagnosticCode::SemanticConstAssignment);
        }

        if (matchedSymbol.isReadOnly) {
          m_TypeErrorMessages.emplace_back(
              "readonly-qualified symbol is neither assignable with a value "
              "nor pointer with a reference",
              symbolInfo, DiagnosticCode::SemanticReadonlyAssignment);
        }

        if (matchedSymbol.indirectionLevel > 0 && matchedSymbol.isConstPtr) {
          m_TypeErrorMessages.emplace_back(
              "constptr-qualified pointer is not assignable", symbolInfo,
              DiagnosticCode::SemanticConstPtrAssignment);
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
                  "const-qualified value is not assignable", symbolInfo,
                  DiagnosticCode::SemanticConstAssignment);
            }

            if (currentSymbol.isReadOnly) {
              m_TypeErrorMessages.emplace_back(
                  "readonly-qualified symbol is neither assignable with a "
                  "value "
                  "nor pointer with a reference",
                  symbolInfo, DiagnosticCode::SemanticReadonlyAssignment);
            }

            if (currentSymbol.indirectionLevel > 0 &&
                currentSymbol.isConstPtr) {
              m_TypeErrorMessages.emplace_back(
                  "constptr-qualified pointer is not assignable", symbolInfo,
                  DiagnosticCode::SemanticConstPtrAssignment);
            }
          }
        }

        // ----------------------

        this->m_ExpectedType = matchedSymbol.type;

        this->m_DefinedTypeFlag =
            matchedSymbol.type == TypeSpecifier::SPEC_DEFINED;

        this->m_LastBinOpHasAtLeastOnePtr = false;
        this->m_LastSymbolInfo = matchedSymbol;
        this->m_LastSymbolInfo.symbolId = assignmentSymbolId;
        this->m_LastSymbolInfo.scopeId = assignmentScopeId;
        this->m_LastSymbolInfo.scopeLevel = assignmentScopeLevel;

        auto rhsAsSymbol = dynamic_cast<SymbolAST *>(assignmentAst.m_RHS.get());
        auto rhsAsUnary =
            dynamic_cast<UnaryOpAST *>(assignmentAst.m_RHS.get());
        const bool rhsIsMoveExpr =
            rhsAsUnary != nullptr && rhsAsUnary->m_UnaryOpKind == U_MOVE;

        auto getPointerSource = [&](SymbolInfo &source) -> bool {
          SymbolAST *sourceSymbol = rhsAsSymbol;
          if (rhsIsMoveExpr) {
            sourceSymbol = dynamic_cast<SymbolAST *>(rhsAsUnary->m_Node.get());
          }

          if (sourceSymbol == nullptr) {
            return false;
          }

          SymbolInfo lookup;
          auto sourceName = sourceSymbol->m_SymbolName;
          if (!symbolValidation(sourceName, lookup, source, true)) {
            return false;
          }

          return source.indirectionLevel > 0;
        };

        SymbolInfo uniqueMoveSource;
        bool markUniqueMoveSource = false;
        if (assignmentAst.m_ShortcutOp == ShortcutOp::S_MOV) {
          SymbolInfo source;
          if (matchedSymbol.indirectionLevel == 0 ||
              assignmentAst.m_IsDereferenced || assignmentAst.m_Subscriptable) {
            this->m_TypeErrorMessages.emplace_back(
                "':=' move assignment requires a direct pointer target",
                symbolInfo, DiagnosticCode::SemanticOwnership);
          } else if (!getPointerSource(source)) {
            this->m_TypeErrorMessages.emplace_back(
                "':=' move assignment requires a pointer source",
                symbolInfo, DiagnosticCode::SemanticOwnership);
          } else if (source.isNoMove) {
            this->m_TypeErrorMessages.emplace_back(
                "'nomove' pointer cannot be moved", symbolInfo,
                DiagnosticCode::SemanticOwnership);
          } else if (matchedSymbol.isUnique != source.isUnique) {
            this->m_TypeErrorMessages.emplace_back(
                "':=' move assignment requires matching pointer ownership kind",
                symbolInfo, DiagnosticCode::SemanticOwnership);
          } else {
            uniqueMoveSource = source;
            markUniqueMoveSource = true;
          }
        } else if (matchedSymbol.isUnique && rhsAsSymbol != nullptr) {
          SymbolInfo source;
          if (getPointerSource(source) && source.isUnique) {
            this->m_TypeErrorMessages.emplace_back(
                "unique pointer cannot be copied; use ':=' or 'move' to "
                "transfer ownership",
                symbolInfo, DiagnosticCode::SemanticOwnership);
          }
        }

        //      this->m_LastIde
        //        if(indirectable)
        auto rhs = assignmentAst.m_RHS->acceptBefore(*this);
        if (markUniqueMoveSource) {
          m_MovedUniqueSymbols.insert(SymbolStateKey(uniqueMoveSource));
          m_MovedUniqueSymbols.erase(SymbolStateKey(matchedSymbol));
        } else if (matchedSymbol.indirectionLevel > 0 &&
                   !assignmentAst.m_IsDereferenced &&
                   !assignmentAst.m_Subscriptable) {
          m_MovedUniqueSymbols.erase(SymbolStateKey(matchedSymbol));
        }

        // rhs symbol need to check

        this->m_LastReferenced = false;
        this->m_DereferenceLevel = 1;
      }

      this->m_LastAssignment = previousLastAssignment;
    }
  }

  this->m_ExpectedType = previousExpectedType;
  this->m_LastSymbolInfo = previousLastSymbolInfo;
  this->m_DefinedTypeFlag = previousDefinedTypeFlag;
  this->m_LastBinOpHasAtLeastOnePtr = previousLastBinOpHasPtr;
  this->m_LastAssignment = previousLastAssignment;

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
        if (matchedSymbol.indirectionLevel > 0 &&
            m_MovedUniqueSymbols.count(SymbolStateKey(matchedSymbol)) > 0) {
          this->m_TypeErrorMessages.emplace_back(
              "pointer '" + matchedSymbol.symbolName +
                  "' was moved and cannot be used before being reinitialized",
              symbolInfo, DiagnosticCode::SemanticOwnership);
        }

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
                  m_LastSymbolInfo.isRef && m_LastReferenced &&
                  !this->m_LastBinOp) {
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
            const bool readingConstValueThroughPointer =
                matchedSymbol.isConstVal && m_LastDereferenced;
            if ((matchedSymbol.isConstVal && !m_LastSymbolInfo.isConstVal) &&
                matchedSymbol.indirectionLevel != 0 &&
                !readingConstValueThroughPointer) {
              if (this->m_LastRetExpr) {
                this->m_TypeErrorMessages.emplace_back(
                    "Qualifier mismatch: cannot return value with "
                    "incompatible qualifier",
                    symbolInfo, DiagnosticCode::SemanticQualifierMismatch);
              } else {
                this->m_TypeErrorMessages.emplace_back(
                    "Qualifier mismatch: cannot initialize value with "
                    "incompatible qualifier",
                    symbolInfo, DiagnosticCode::SemanticQualifierMismatch);
              }
            }

            const bool bindingToConstRef =
                m_LastSymbolInfo.isConstRef && m_LastSymbolInfo.isRef;
            const bool readingConstRefAsValue =
                matchedSymbol.isConstRef && !m_LastSymbolInfo.isRef &&
                m_LastSymbolInfo.indirectionLevel == 0;
            if (matchedSymbol.isConstRef != m_LastSymbolInfo.isConstRef &&
                !bindingToConstRef && !readingConstRefAsValue) {
              if (this->m_LastRetExpr) {
                this->m_TypeErrorMessages.emplace_back(
                    "Qualifier mismatch: cannot return value with "
                    "incompatible qualifier",
                    symbolInfo, DiagnosticCode::SemanticQualifierMismatch);
              } else {
                this->m_TypeErrorMessages.emplace_back(
                    "Qualifier mismatch: cannot initialize value with "
                    "incompatible qualifier",
                    symbolInfo, DiagnosticCode::SemanticQualifierMismatch);
              }
            }

            const bool bindingToConstPtr =
                m_LastSymbolInfo.isConstPtr &&
                m_LastSymbolInfo.indirectionLevel > 0 && m_LastReferenced;
            const bool readingThroughConstPtr =
                matchedSymbol.isConstPtr && m_LastDereferenced;
            if (matchedSymbol.isConstPtr != m_LastSymbolInfo.isConstPtr &&
                !bindingToConstPtr && !readingThroughConstPtr &&
                !matchedSymbol.isConstRef && !matchedSymbol.isConstVal) {
              if (this->m_LastRetExpr) {
                this->m_TypeErrorMessages.emplace_back(
                    "Qualifier mismatch: cannot return value with "
                    "incompatible qualifier",
                    symbolInfo, DiagnosticCode::SemanticQualifierMismatch);
              } else {
                this->m_TypeErrorMessages.emplace_back(
                    "Qualifier mismatch: cannot initialize value with "
                    "incompatible qualifier",
                    symbolInfo, DiagnosticCode::SemanticQualifierMismatch);
              }
            }
          }
        } else {
          matchedSymbol.indirectionLevel =
              m_LastSymbolInfo.arrayDimensions.size();
          if (m_LastSymbolInfo.isSubscriptable) {
            if (0 !=
                    (matchedSymbol.indirectionLevel - m_LastArrayIndexCount -
                     (this->m_LastDereferenced ? this->m_DereferenceLevel : 0) +
                     (m_LastReferenced ? 1 : 0)) &&
                m_LastBinOp) {
              symbolInfo.typeCheckerInfo.isCompatibleSubsForBinOp = false;
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
                    this->m_LastSymbolInfo.isConstVal ||
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
        // partially okay ( actually not really qualifying to the address of
        // the value. )
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

    if (binaryOpAst.m_BinOpKind == B_DOT) {
      SymbolInfo accessInfo;
      accessInfo.begin = binaryOpAst.m_SemLoc.begin;
      accessInfo.end = binaryOpAst.m_SemLoc.end;
      accessInfo.line = binaryOpAst.m_SemLoc.line;

      auto resolveFieldAccess =
          [&](auto &self, IAST *node) -> SymbolInfo {
        SymbolInfo resolved;
        resolved.begin = accessInfo.begin;
        resolved.end = accessInfo.end;
        resolved.line = accessInfo.line;

        if (node == nullptr) {
          this->m_TypeErrorMessages.emplace_back(
              "Struct field access expects `value.field`", accessInfo);
          return resolved;
        }

        if (node->m_ExprKind == ExprKind::SymbolExpr) {
          auto *symbol = static_cast<SymbolAST *>(node);
          SymbolInfo lookup;
          lookup.symbolName = symbol->m_SymbolName;
          lookup.begin = symbol->m_SemLoc.begin;
          lookup.end = symbol->m_SemLoc.end;
          lookup.line = symbol->m_SemLoc.line;
          symbolValidation(symbol->m_SymbolName, lookup, resolved);
          if (resolved.type == TypeSpecifier::SPEC_DEFINED &&
              resolved.indirectionLevel > 0) {
            resolved.indirectionLevel = 0;
            resolved.isUnique = false;
            resolved.isRef = false;
          }
          return resolved;
        }

        if (node->m_ExprKind != ExprKind::BinOp) {
          this->m_TypeErrorMessages.emplace_back(
              "Struct field access expects `value.field`", accessInfo);
          return resolved;
        }

        auto *fieldAccess = static_cast<BinaryOpAST *>(node);
        if (fieldAccess->m_BinOpKind != B_DOT ||
            fieldAccess->m_RHS == nullptr ||
            fieldAccess->m_RHS->m_ExprKind != ExprKind::SymbolExpr) {
          this->m_TypeErrorMessages.emplace_back(
              "Struct field access expects `value.field`", accessInfo);
          return resolved;
        }

        auto base = self(self, fieldAccess->m_LHS.get());
        auto *fieldSymbol = static_cast<SymbolAST *>(fieldAccess->m_RHS.get());
        if (base.type != TypeSpecifier::SPEC_DEFINED) {
          this->m_TypeErrorMessages.emplace_back(
              "Field access requires a struct value", base);
          return resolved;
        }

        auto structIt = StructTable.find(base.definedTypeName);
        if (structIt == StructTable.end()) {
          this->m_TypeErrorMessages.emplace_back(
              "Unknown struct type '" + base.definedTypeName + "'", base);
          return resolved;
        }

        auto fieldIt =
            structIt->second.fieldIndexes.find(fieldSymbol->m_SymbolName);
        if (fieldIt == structIt->second.fieldIndexes.end()) {
          this->m_TypeErrorMessages.emplace_back(
              "Struct '" + base.definedTypeName + "' has no field '" +
                  fieldSymbol->m_SymbolName + "'",
              accessInfo);
          return resolved;
        }

        const auto &field = structIt->second.fields[fieldIt->second];
        resolved = base;
        resolved.symbolName = base.symbolName + "." + field.name;
        resolved.type = field.type;
        resolved.definedTypeName = field.definedTypeName;
        resolved.indirectionLevel = field.indirectionLevel;
        resolved.isUnique = field.isUnique;
        resolved.isRef = field.isRef;
        resolved.isSubscriptable = false;
        resolved.arrayDimensions.clear();
        resolved.begin = fieldSymbol->m_SemLoc.begin;
        resolved.end = fieldSymbol->m_SemLoc.end;
        resolved.line = fieldSymbol->m_SemLoc.line;
        return resolved;
      };

      return resolveFieldAccess(resolveFieldAccess, &binaryOpAst);
    }

    const auto overloadMethod = ValueOperatorMethodName(binaryOpAst.m_BinOpKind);
    if (!overloadMethod.empty()) {
      auto previousExpectedType = m_ExpectedType;
      auto previousLastSymbolInfo = m_LastSymbolInfo;
      auto previousDefinedTypeFlag = m_DefinedTypeFlag;
      auto previousLastBinOpHasPtr = m_LastBinOpHasAtLeastOnePtr;

      auto lhsSymbol = lhs->acceptBefore(*this);
      auto rhsSymbol = rhs->acceptBefore(*this);

      m_ExpectedType = previousExpectedType;
      m_LastSymbolInfo = previousLastSymbolInfo;
      m_DefinedTypeFlag = previousDefinedTypeFlag;
      m_LastBinOpHasAtLeastOnePtr = previousLastBinOpHasPtr;

      if (lhsSymbol.type == TypeSpecifier::SPEC_DEFINED &&
          lhsSymbol.indirectionLevel == 0) {
        const auto functionName =
            lhsSymbol.definedTypeName + "." + overloadMethod;
        auto signatureIt = FunctionTable.find(functionName);
        if (signatureIt != FunctionTable.end()) {
          const auto &signature = signatureIt->second;
          if (signature.params.size() != 2) {
            this->m_TypeErrorMessages.emplace_back(
                "Value operator '" + functionName +
                    "' must have exactly one explicit parameter",
                lhsSymbol);
          } else {
            const auto &rhsParam = signature.params[1];
            if (rhsParam.type != rhsSymbol.type ||
                rhsParam.definedTypeName != rhsSymbol.definedTypeName ||
                rhsParam.indirectionLevel != rhsSymbol.indirectionLevel) {
              this->m_TypeErrorMessages.emplace_back(
                  "Right operand does not match value operator parameter",
                  rhsSymbol);
            }
          }
          return signature.returnType;
        }
      }
    }

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

        size_t arraySize = m_LastVarDecl
                               ? matchedSymbol.arrayDimensions.size()
                               : getIndexesOfArray(*binaryOpAst.m_RHS.get());

        if (symbolValidation(symbolName, rhsSymbol, matchedSymbol)) {
          if (matchedSymbol.isSubscriptable && indexCount <= arraySize) {
            m_LastArrayIndexCount = indexCount;
            m_LastSubscriptable = true;
            std::vector<ScalarOrLiteralAST *> indexes;
            auto collectArrayIndexLiterals =
                [&](auto &self, IAST *expr,
                    std::vector<ScalarOrLiteralAST *> &indexes) -> void {
              if (expr == nullptr) {
                return;
              }

              if (expr->m_ExprKind == ExprKind::ScalarExpr) {
                indexes.push_back(static_cast<ScalarOrLiteralAST *>(expr));
                return;
              }

              if (expr->m_ExprKind != ExprKind::BinOp) {
                indexes.push_back(nullptr);
                return;
              }

              auto *binaryExpr = static_cast<BinaryOpAST *>(expr);
              if (binaryExpr->m_BinOpKind != B_MARRS) {
                indexes.push_back(nullptr);
                return;
              }

              self(self, binaryExpr->m_LHS.get(), indexes);
              self(self, binaryExpr->m_RHS.get(), indexes);
            };
            collectArrayIndexLiterals(collectArrayIndexLiterals,
                                      binaryOpAst.m_RHS.get(), indexes);
            const size_t checkCount =
                std::min(indexes.size(), matchedSymbol.arrayDimensions.size());
            for (size_t i = 0; i < checkCount; ++i) {
              if (indexes[i] == nullptr) {
                continue;
              }

              auto indexVal = std::stoi(indexes[i]->m_Value);
              if (indexVal >= matchedSymbol.arrayDimensions[i]) {
                m_TypeWarningMessages.emplace_back(
                    "Array index '" + indexes[i]->m_Value +
                        "' is after the beginning of the array",
                    rhsSymbol);
              } else if (indexVal < 0) {
                m_TypeWarningMessages.emplace_back(
                    "Array index '" + indexes[i]->m_Value +
                        "' is before the beginning of the array",
                    rhsSymbol);
              }
            }
          } else {
            if (indexCount > matchedSymbol.arrayDimensions.size()) {
              if (m_LastVarDecl) {
                this->m_TypeErrorMessages.emplace_back(
                    "Invalid array index(es)", m_LastSymbolInfo);
              } else {
                this->m_TypeErrorMessages.emplace_back(
                    "Invalid array index(es)", rhsSymbol);
              }
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
      } else if (!lhsSymbol.typeCheckerInfo.isCompatibleSubsForBinOp) {
        this->m_TypeErrorMessages.emplace_back(
            "Invalid array index(es). Subscripted value is not array",
            lhsSymbol);
        this->m_TypeWarningMessages.emplace_back(
            "You exceeded to the maximum dimension count of the array",
            lhsSymbol);
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

  symbolInfo.begin = castOpAst.m_SemLoc.begin;
  symbolInfo.end = castOpAst.m_SemLoc.end;
  symbolInfo.line = castOpAst.m_SemLoc.line;

  if (!m_TypeChecking) {
    return symbolInfo;
  }

  if (!castOpAst.m_HasTypeAttrib || castOpAst.m_TypeNode == nullptr) {
    this->m_TypeErrorMessages.emplace_back(
        "Cast expression requires an explicit target type", symbolInfo);
    return symbolInfo;
  }

  auto *targetType = dynamic_cast<TypeAST *>(castOpAst.m_TypeNode.get());
  if (targetType == nullptr &&
      castOpAst.m_TypeNode->m_ExprKind == ExprKind::SymbolExpr) {
    this->m_TypeErrorMessages.emplace_back(
        "User-defined type casts require the struct/type system first",
        symbolInfo);
    return symbolInfo;
  }

  if (targetType == nullptr) {
    this->m_TypeErrorMessages.emplace_back(
        "Cast target must be a concrete type", symbolInfo);
    return symbolInfo;
  }

  if (targetType->m_TypeSpec == TypeSpecifier::SPEC_VOID &&
      targetType->m_IndirectLevel == 0) {
    this->m_TypeErrorMessages.emplace_back(
        "Cannot cast to plain 'void'", symbolInfo);
    return symbolInfo;
  }

  if (targetType->m_TypeSpec == TypeSpecifier::SPEC_DEFINED) {
    this->m_TypeErrorMessages.emplace_back(
        "User-defined type casts require the struct/type system first",
        symbolInfo);
    return symbolInfo;
  }

  if (castOpAst.m_CastOpKind == CastOpKind::C_UNSAFE_CAST) {
    symbolInfo.type = targetType->m_TypeSpec;
    symbolInfo.indirectionLevel = targetType->m_IndirectLevel;
    symbolInfo.isRef = targetType->m_IsRef;
    symbolInfo.isUnique = targetType->m_IsUniquePtr;
    return symbolInfo;
  }

  SymbolInfo sourceInfo;
  if (castOpAst.m_Node == nullptr) {
    this->m_TypeErrorMessages.emplace_back(
        "Cast expression requires a source expression", symbolInfo);
    return symbolInfo;
  }

  if (castOpAst.m_Node->m_ExprKind == ExprKind::SymbolExpr) {
    auto *sourceSymbol = static_cast<SymbolAST *>(castOpAst.m_Node.get());
    SymbolInfo lookupInfo;
    lookupInfo.symbolName = sourceSymbol->m_SymbolName;
    lookupInfo.begin = sourceSymbol->m_SemLoc.begin;
    lookupInfo.end = sourceSymbol->m_SemLoc.end;
    lookupInfo.line = sourceSymbol->m_SemLoc.line;
    symbolValidation(sourceSymbol->m_SymbolName, lookupInfo, sourceInfo);
  } else if (castOpAst.m_Node->m_ExprKind == ExprKind::ScalarExpr) {
    auto *scalar = static_cast<ScalarOrLiteralAST *>(castOpAst.m_Node.get());
    sourceInfo.begin = scalar->m_SemLoc.begin;
    sourceInfo.end = scalar->m_SemLoc.end;
    sourceInfo.line = scalar->m_SemLoc.line;

    if (scalar->m_IsLiteral) {
      sourceInfo.type = TypeSpecifier::SPEC_CHAR;
      sourceInfo.indirectionLevel = 1;
    } else if (scalar->m_IsBoolean) {
      sourceInfo.type = TypeSpecifier::SPEC_BOOL;
    } else if (scalar->m_IsFloat) {
      sourceInfo.type = TypeSpecifier::SPEC_F64;
    } else if (scalar->m_IsLetter) {
      sourceInfo.type = TypeSpecifier::SPEC_CHAR;
    } else {
      sourceInfo.type = TypeSpecifier::SPEC_I64;
    }
  } else {
    const auto previousExpectedType = m_ExpectedType;
    const auto previousLastSymbolInfo = m_LastSymbolInfo;
    const auto previousDefinedTypeFlag = m_DefinedTypeFlag;
    const auto previousLastBinOpHasPtr = m_LastBinOpHasAtLeastOnePtr;

    m_ExpectedType = TypeSpecifier::SPEC_I64;
    m_LastSymbolInfo.type = TypeSpecifier::SPEC_I64;
    m_LastSymbolInfo.indirectionLevel = 0;
    m_LastSymbolInfo.isRef = false;
    m_LastSymbolInfo.isUnique = false;
    m_DefinedTypeFlag = false;
    m_LastBinOpHasAtLeastOnePtr = false;

    sourceInfo = castOpAst.m_Node->acceptBefore(*this);

    m_ExpectedType = previousExpectedType;
    m_LastSymbolInfo = previousLastSymbolInfo;
    m_DefinedTypeFlag = previousDefinedTypeFlag;
    m_LastBinOpHasAtLeastOnePtr = previousLastBinOpHasPtr;
  }

  if (castOpAst.m_CastOpKind != CastOpKind::C_UNSAFE_CAST) {
    const bool targetPointer = targetType->m_IndirectLevel > 0;
    const bool sourcePointer =
        sourceInfo.indirectionLevel > 0 || sourceInfo.isRef;

    if (targetPointer != sourcePointer) {
      this->m_TypeErrorMessages.emplace_back(
          "Safe cast cannot cross pointer and value categories", symbolInfo);
    }

    const bool sourceHasPointerQualifier =
        sourcePointer &&
        (sourceInfo.isConstVal || sourceInfo.isConstPtr ||
         sourceInfo.isConstRef || sourceInfo.isReadOnly ||
         std::any_of(sourceInfo.qualifierLevels.begin(),
                     sourceInfo.qualifierLevels.end(),
                     [](TypeQualifier qualifier) {
                       return qualifier != TypeQualifier::Q_NONE;
                     }));

    if (sourceHasPointerQualifier) {
      this->m_TypeErrorMessages.emplace_back(
          "Safe cast cannot discard pointer qualifiers", symbolInfo,
          DiagnosticCode::SemanticQualifierMismatch);
    }
  }

  symbolInfo.type = targetType->m_TypeSpec;
  symbolInfo.indirectionLevel = targetType->m_IndirectLevel;
  symbolInfo.isRef = targetType->m_IsRef;
  symbolInfo.isUnique = targetType->m_IsUniquePtr;

  return symbolInfo;
}

SymbolInfo Visitor::preVisit(FuncAST &funcAst) {
  SymbolInfo symbolInfo;

  if (funcAst.m_RetType != nullptr) {
    auto retType = dynamic_cast<TypeAST *>(funcAst.m_RetType.get());

    symbolInfo.type = retType->m_TypeSpec;
    if (retType->m_TypeSpec == TypeSpecifier::SPEC_DEFINED &&
        retType->m_Symbol != nullptr &&
        retType->m_Symbol->m_ExprKind == ExprKind::SymbolExpr) {
      auto *typeSymbol = static_cast<SymbolAST *>(retType->m_Symbol.get());
      symbolInfo.definedTypeName = typeSymbol->m_SymbolName;
    }
    symbolInfo.indirectionLevel = retType->m_IndirectLevel;
    symbolInfo.isConstRef = funcAst.m_RetTypeQualifier == Q_CONSTREF;
    symbolInfo.isConstPtr = funcAst.m_RetTypeQualifier == Q_CONSTPTR;
    symbolInfo.isReadOnly = funcAst.m_RetTypeQualifier == Q_READONLY;
    symbolInfo.isConstVal = funcAst.m_RetTypeQualifier == Q_CONST;
    symbolInfo.isRef = retType->m_IsRef;
    symbolInfo.isUnique = retType->m_IsUniquePtr;
    symbolInfo.qualifierLevels = BuildQualifierLevels(
        funcAst.m_RetTypeQualifier, symbolInfo.indirectionLevel,
        symbolInfo.isRef);

    m_LastFuncRetTypeInfo = symbolInfo;
  }

  symbolInfo.assocFuncName = funcAst.m_FuncName;
  symbolInfo.begin = funcAst.m_SemLoc.begin;
  symbolInfo.end = funcAst.m_SemLoc.end;
  symbolInfo.line = funcAst.m_SemLoc.line;
  symbolInfo.isPublic = funcAst.m_Access == ACCESS_PUBLIC;
  symbolInfo.isStatic = funcAst.m_IsStatic;
  symbolInfo.isExported = funcAst.m_IsExported;
  symbolInfo.isImported = funcAst.m_IsForwardDecl && !funcAst.m_IsExported;

  if (funcAst.m_IsVariadic && !funcAst.m_IsForwardDecl) {
    this->m_TypeErrorMessages.emplace_back(
        "Variadic parameter marker '...' is currently supported only for "
        "native import/export declarations",
        symbolInfo);
  }

  std::string methodOwner;
  std::string methodName;
  if (SplitStructMethodName(funcAst.m_FuncName, methodOwner, methodName) &&
      methodName == "new") {
    if (!funcAst.m_IsStatic) {
      this->m_TypeErrorMessages.emplace_back(
          "struct 'new' allocation entry must be static and called as '" +
              methodOwner + "::new(...)'",
          symbolInfo);
    }

    if (symbolInfo.type != TypeSpecifier::SPEC_DEFINED ||
        symbolInfo.definedTypeName != methodOwner) {
      this->m_TypeErrorMessages.emplace_back(
          "struct 'new' allocation entry must return '" + methodOwner + "^' "
          "or '" + methodOwner + "*'",
          symbolInfo);
    } else if (symbolInfo.indirectionLevel == 0) {
      this->m_TypeErrorMessages.emplace_back(
          "by-value construction uses the constructor syntax '" + methodOwner +
              "(...)'; 'new' is reserved for allocation entries",
          symbolInfo);
    } else {
      this->m_TypeErrorMessages.emplace_back(
          "struct 'new' allocation lowering requires allocator/control-block "
          "support and is not implemented yet",
          symbolInfo);
    }
  }

  if (m_TypeChecking) {
    const bool previousStaticFunction = m_CurrentFunctionIsStatic;
    m_DroppedSemanticSymbols.clear();
    m_CurrentFunctionIsStatic = funcAst.m_IsStatic;
    this->m_LastScopeSymbols = LocalSymbolTable[funcAst.m_FuncName];
    for (auto &param : funcAst.m_Params) {
      auto symbol = param->acceptBefore(*this);
      //      typeCheckerScopeHandler(param);
    }

    enterScope(false);
    for (auto &node : funcAst.m_Scope) {
      typeCheckerScopeHandler(node);
    }
    m_CurrentFunctionIsStatic = previousStaticFunction;
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

  const bool wasInsideLoop = this->m_LastLoop;
  this->m_LastLoop = true;
  auto makeLoopSymbol = [&](SymbolAST *symbolAst, TypeSpecifier type,
                            size_t indirectionLevel = 0) {
    SymbolInfo loopSymbol;
    loopSymbol.symbolName = symbolAst->m_SymbolName;
    loopSymbol.begin = symbolAst->m_SemLoc.begin;
    loopSymbol.end = symbolAst->m_SemLoc.end;
    loopSymbol.line = symbolAst->m_SemLoc.line;
    loopSymbol.type = type;
    loopSymbol.indirectionLevel = indirectionLevel;
    loopSymbol.isPrimitive = IsPrimitiveType(type);
    loopSymbol.isCastable = true;
    loopSymbol.symbolScope = SymbolScope::LoopSt;
    loopSymbol.scopeId = m_ScopeId;
    loopSymbol.scopeLevel = m_ScopeLevel;
    loopSymbol.symbolId = 0;
    if (!m_TypeChecking) {
      this->m_SymbolInfos.push_back(loopSymbol);
      this->m_LastScopeSymbols.emplace_back(loopSymbol.symbolName, loopSymbol);
    }
  };
  auto findKnownSymbol = [&](const std::string &name,
                             SymbolInfo &matchedSymbol) {
    SymbolInfo lookupInfo;
    auto lookupName = name;
    if (symbolValidation(lookupName, lookupInfo, matchedSymbol, true)) {
      return true;
    }

    for (auto it = m_SymbolInfos.rbegin(); it != m_SymbolInfos.rend(); ++it) {
      if (it->symbolName == name) {
        matchedSymbol = *it;
        return true;
      }
    }

    for (auto &entry : GlobalSymbolTable) {
      if (entry.symbolName == name) {
        matchedSymbol = entry.symbolInfo;
        return true;
      }
    }

    return false;
  };

  if (m_TypeChecking) {
    if (loopStmtAst.m_RangeLoop) {
      if (loopStmtAst.m_Indexable) {
        auto *indexSymbol =
            static_cast<SymbolAST *>(loopStmtAst.m_IndexSymbol.get());
        makeLoopSymbol(indexSymbol, TypeSpecifier::SPEC_I64);
      }

      bool iterSymbolNotExist = false;
      if (loopStmtAst.m_HasNumericRange) {
        auto *dataSymbol =
            static_cast<SymbolAST *>(loopStmtAst.m_DataSymbol.get());
        makeLoopSymbol(dataSymbol, TypeSpecifier::SPEC_I64);

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

          symbolInfo.begin = iterSymbol->m_SemLoc.begin;
          symbolInfo.end = iterSymbol->m_SemLoc.end;
          symbolInfo.line = iterSymbol->m_SemLoc.line;

          SymbolInfo matchedSymbol;
          if (symbolValidation(iterSymbol->m_SymbolName, symbolInfo,
                               matchedSymbol)) {
            if (!matchedSymbol.isSubscriptable) {
              symbolInfo.begin = iterSymbol->m_SemLoc.begin;
              symbolInfo.end = iterSymbol->m_SemLoc.end;
              symbolInfo.line = iterSymbol->m_SemLoc.line;
              this->m_TypeErrorMessages.emplace_back(
                  "Symbol must be iterable or provided a sequenceable "
                  "trait "
                  "(built-in trait)",
                  symbolInfo);
            }

            auto *dataSymbol =
                static_cast<SymbolAST *>(loopStmtAst.m_DataSymbol.get());
            makeLoopSymbol(dataSymbol, matchedSymbol.type);
          } else {
            iterSymbolNotExist = true;
            this->m_TypeErrorMessages.emplace_back(
                "Iterable symbol is not valid", symbolInfo);
          }
        } else {
          this->m_TypeErrorMessages.emplace_back(
              "Iterable version of loop needs an symbol which is also "
              "iterable",
              symbolInfo);
        }
      }

      (void)iterSymbolNotExist;
    } else {
      this->m_LastCondExpr = true;
      loopStmtAst.m_Cond->acceptBefore(*this);
      this->m_LastCondExpr = false;
    }
  } else if (loopStmtAst.m_RangeLoop) {
    if (loopStmtAst.m_Indexable) {
      auto *indexSymbol =
          static_cast<SymbolAST *>(loopStmtAst.m_IndexSymbol.get());
      makeLoopSymbol(indexSymbol, TypeSpecifier::SPEC_I64);
    }

    if (loopStmtAst.m_HasNumericRange) {
      auto *dataSymbol =
          static_cast<SymbolAST *>(loopStmtAst.m_DataSymbol.get());
      makeLoopSymbol(dataSymbol, TypeSpecifier::SPEC_I64);
      } else if (loopStmtAst.m_IterSymbol != nullptr &&
               loopStmtAst.m_IterSymbol->m_ExprKind == SymbolExpr) {
      auto *iterSymbol = static_cast<SymbolAST *>(loopStmtAst.m_IterSymbol.get());
      auto iterName = iterSymbol->m_SymbolName;
      SymbolInfo matchedSymbol;
      if (findKnownSymbol(iterName, matchedSymbol)) {
        auto *dataSymbol =
            static_cast<SymbolAST *>(loopStmtAst.m_DataSymbol.get());
        makeLoopSymbol(dataSymbol, matchedSymbol.type);
      }
    }
  }

  for (auto &node : loopStmtAst.m_Scope) {
    scopeHandler(node, SymbolScope::LoopSt);
  }

  this->m_LastLoop = wasInsideLoop;
  exitScope(false);

  return symbolInfo;
}

SymbolInfo Visitor::preVisit(BreakStmtAST &breakStmtAst) {
  SymbolInfo symbolInfo;
  symbolInfo.begin = breakStmtAst.m_SemLoc.begin;
  symbolInfo.end = breakStmtAst.m_SemLoc.end;
  symbolInfo.line = breakStmtAst.m_SemLoc.line;

  if (m_TypeChecking && !m_LastLoop) {
    this->m_TypeErrorMessages.emplace_back(
        "`break` can only be used inside a loop", symbolInfo);
  }

  return symbolInfo;
}

SymbolInfo Visitor::preVisit(ContinueStmtAST &continueStmtAst) {
  SymbolInfo symbolInfo;
  symbolInfo.begin = continueStmtAst.m_SemLoc.begin;
  symbolInfo.end = continueStmtAst.m_SemLoc.end;
  symbolInfo.line = continueStmtAst.m_SemLoc.line;

  if (m_TypeChecking && !m_LastLoop) {
    this->m_TypeErrorMessages.emplace_back(
        "`continue` can only be used inside a loop", symbolInfo);
  }

  return symbolInfo;
}

SymbolInfo Visitor::preVisit(NewAST &newAst) {
  SymbolInfo symbolInfo;
  symbolInfo.begin = newAst.m_SemLoc.begin;
  symbolInfo.end = newAst.m_SemLoc.end;
  symbolInfo.line = newAst.m_SemLoc.line;
  symbolInfo.symbolName = "new " + newAst.m_TypeName;
  symbolInfo.type = TypeSpecifier::SPEC_DEFINED;
  symbolInfo.definedTypeName = newAst.m_TypeName;
  symbolInfo.indirectionLevel = 1;
  symbolInfo.isUnique = !newAst.m_IsShared;

  if (!m_TypeChecking) {
    return symbolInfo;
  }

  if (m_TypeTable.count(newAst.m_TypeName) == 0 ||
      StructTable.count(newAst.m_TypeName) == 0) {
    this->m_TypeErrorMessages.emplace_back(
        "`new` expects a known struct type", symbolInfo);
    return symbolInfo;
  }

  if (newAst.m_Allocator != nullptr) {
    if (newAst.m_Allocator->m_ExprKind != ExprKind::SymbolExpr) {
      this->m_TypeErrorMessages.emplace_back(
          "`new(allocator)` expects a named allocator value", symbolInfo);
    }

    SymbolInfo allocatorInfo;
    if (newAst.m_Allocator->m_ExprKind == ExprKind::SymbolExpr) {
      auto *allocatorSymbol =
          static_cast<SymbolAST *>(newAst.m_Allocator.get());
      SymbolInfo lookupInfo;
      lookupInfo.symbolName = allocatorSymbol->m_SymbolName;
      lookupInfo.begin = allocatorSymbol->m_SemLoc.begin;
      lookupInfo.end = allocatorSymbol->m_SemLoc.end;
      lookupInfo.line = allocatorSymbol->m_SemLoc.line;
      symbolValidation(allocatorSymbol->m_SymbolName, lookupInfo,
                       allocatorInfo, true);
    } else {
      allocatorInfo = newAst.m_Allocator->acceptBefore(*this);
    }
    if (allocatorInfo.type != TypeSpecifier::SPEC_DEFINED) {
      this->m_TypeErrorMessages.emplace_back(
          "`new(allocator)` expects a struct value implementing Allocator",
          symbolInfo);
    } else {
      const auto allocName = allocatorInfo.definedTypeName + ".alloc";
      const auto freeName = allocatorInfo.definedTypeName + ".free";
      bool satisfiesAllocator = false;
      auto structIt = StructTable.find(allocatorInfo.definedTypeName);
      if (TraitTable.count("Allocator") != 0 && structIt != StructTable.end()) {
        satisfiesAllocator =
            std::find(structIt->second.traits.begin(),
                      structIt->second.traits.end(), "Allocator") !=
            structIt->second.traits.end();
      } else {
        satisfiesAllocator =
            FunctionTable.count(allocName) != 0 &&
            FunctionTable.count(freeName) != 0;
      }

      if (!satisfiesAllocator) {
        this->m_TypeErrorMessages.emplace_back(
            "allocator type '" + allocatorInfo.definedTypeName +
                "' must implement Allocator with alloc(bytes, align) and "
                "free(ptr, bytes, align)",
            symbolInfo);
      }
    }
  }

  std::vector<IAST *> argNodes;
  auto collectArgs = [&](auto &self, IAST *node) -> void {
    if (node == nullptr) {
      return;
    }
    if (node->m_ExprKind == ExprKind::BinOp) {
      auto *binOp = static_cast<BinaryOpAST *>(node);
      if (binOp->m_BinOpKind == BinOpKind::B_COMM) {
        self(self, binOp->m_LHS.get());
        self(self, binOp->m_RHS.get());
        return;
      }
    }
    argNodes.push_back(node);
  };
  collectArgs(collectArgs, newAst.m_Args.get());

  const auto constructorName = newAst.m_TypeName + ".constructor";
  auto signatureIt = FunctionTable.find(constructorName);
  if (signatureIt == FunctionTable.end()) {
    if (!argNodes.empty()) {
      this->m_TypeErrorMessages.emplace_back(
          "Struct '" + newAst.m_TypeName +
              "' does not declare a constructor for `new` arguments",
          symbolInfo);
    }
    return symbolInfo;
  }

  const auto expectedArgs =
      signatureIt->second.params.empty() ? 0 : signatureIt->second.params.size() - 1;
  if (argNodes.size() != expectedArgs) {
    this->m_TypeErrorMessages.emplace_back(
        "Constructor for '" + newAst.m_TypeName + "' expects " +
            std::to_string(expectedArgs) + " argument(s), but " +
            std::to_string(argNodes.size()) + " provided",
        symbolInfo);
    return symbolInfo;
  }

  for (size_t i = 0; i < argNodes.size(); ++i) {
    const auto &param = signatureIt->second.params[i + 1];
    m_LastSymbolInfo = param;
    if (m_LastSymbolInfo.isRef) {
      m_LastSymbolInfo.indirectionLevel += 1;
    }
    m_LastSymbolInfo.symbolId = m_SymbolId;
    m_LastSymbolInfo.scopeId = m_ScopeId;
    m_LastSymbolInfo.scopeLevel = m_ScopeLevel;
    m_ExpectedType = param.type;
    m_DefinedTypeFlag = param.type == TypeSpecifier::SPEC_DEFINED;
    argNodes[i]->acceptBefore(*this);
  }

  return symbolInfo;
}

SymbolInfo Visitor::preVisit(DropStmtAST &dropStmtAst) {
  SymbolInfo symbolInfo;
  symbolInfo.symbolName = dropStmtAst.m_SymbolName;
  symbolInfo.begin = dropStmtAst.m_SemLoc.begin;
  symbolInfo.end = dropStmtAst.m_SemLoc.end;
  symbolInfo.line = dropStmtAst.m_SemLoc.line;

  if (!m_TypeChecking) {
    return symbolInfo;
  }

  m_LastSymbolInfo.symbolId = m_SymbolId;
  m_LastSymbolInfo.scopeId = m_ScopeId;
  m_LastSymbolInfo.scopeLevel = m_ScopeLevel;

  SymbolInfo matchedSymbol;
  auto symbolName = dropStmtAst.m_SymbolName;
  if (!symbolValidation(symbolName, symbolInfo, matchedSymbol)) {
    return symbolInfo;
  }

  if (matchedSymbol.type != TypeSpecifier::SPEC_DEFINED) {
    this->m_TypeErrorMessages.emplace_back(
        "`drop` expects a struct value or struct ownership pointer",
        symbolInfo, DiagnosticCode::SemanticOwnership);
    return symbolInfo;
  }

  m_DroppedSemanticSymbols.insert(SymbolStateKey(matchedSymbol));
  if (matchedSymbol.indirectionLevel > 0) {
    m_MovedUniqueSymbols.insert(SymbolStateKey(matchedSymbol));
  }

  return symbolInfo;
}

std::string Visitor::resolveFunctionCallName(IAST *node, SymbolInfo &symbolInfo,
                                             bool emitDiagnostics) {
  if (node == nullptr) {
    return {};
  }

  if (node->m_ExprKind == ExprKind::SymbolExpr) {
    auto *symbol = static_cast<SymbolAST *>(node);
    symbolInfo.symbolName = symbol->m_SymbolName;
    symbolInfo.begin = symbol->m_SemLoc.begin;
    symbolInfo.end = symbol->m_SemLoc.end;
    symbolInfo.line = symbol->m_SemLoc.line;
    return symbol->m_SymbolName;
  }

  if (node->m_ExprKind == ExprKind::BinOp) {
    auto *binOp = static_cast<BinaryOpAST *>(node);
    if ((binOp->m_BinOpKind == BinOpKind::B_DOT ||
         binOp->m_BinOpKind == BinOpKind::B_CCOL) &&
        binOp->m_LHS != nullptr && binOp->m_RHS != nullptr &&
        binOp->m_LHS->m_ExprKind == ExprKind::SymbolExpr &&
        binOp->m_RHS->m_ExprKind == ExprKind::SymbolExpr) {
      auto *alias = static_cast<SymbolAST *>(binOp->m_LHS.get());
      auto *member = static_cast<SymbolAST *>(binOp->m_RHS.get());
      symbolInfo.symbolName = member->m_SymbolName;
      symbolInfo.begin = member->m_SemLoc.begin;
      symbolInfo.end = member->m_SemLoc.end;
      symbolInfo.line = member->m_SemLoc.line;

      if (binOp->m_BinOpKind == BinOpKind::B_CCOL) {
        auto methodName = alias->m_SymbolName + "." + member->m_SymbolName;
        if (emitDiagnostics) {
          if (m_TypeTable.count(alias->m_SymbolName) == 0) {
            this->m_TypeErrorMessages.emplace_back(
                "`::` method calls require a type name on the left side",
                symbolInfo);
            return {};
          }

          auto signatureIt = FunctionTable.find(methodName);
          if (signatureIt == FunctionTable.end()) {
            this->m_TypeErrorMessages.emplace_back(
                "Struct '" + alias->m_SymbolName +
                    "' has no static method '" + member->m_SymbolName + "'",
                symbolInfo);
            return {};
          }

          if (!signatureIt->second.returnType.isStatic) {
            this->m_TypeErrorMessages.emplace_back(
                "`::` can only call static struct methods; use '.' for "
                "instance methods",
                symbolInfo);
            return {};
          }
        }

        return methodName;
      }

      if (ModuleAliases.count(alias->m_SymbolName) != 0) {
        return member->m_SymbolName;
      }

      if (emitDiagnostics) {
        SymbolInfo receiverInfo;
        SymbolInfo matchedReceiver;
        receiverInfo.symbolName = alias->m_SymbolName;
        receiverInfo.begin = alias->m_SemLoc.begin;
        receiverInfo.end = alias->m_SemLoc.end;
        receiverInfo.line = alias->m_SemLoc.line;
        auto receiverName = alias->m_SymbolName;
        m_LastSymbolInfo.symbolId = m_SymbolId;
        m_LastSymbolInfo.scopeId = m_ScopeId;
        m_LastSymbolInfo.scopeLevel = m_ScopeLevel;
        if (!symbolValidation(receiverName, receiverInfo, matchedReceiver)) {
          return {};
        }

        if (matchedReceiver.type != TypeSpecifier::SPEC_DEFINED) {
          this->m_TypeErrorMessages.emplace_back(
              "Method call receiver must be a struct value or pointer",
              receiverInfo);
          return {};
        }

        auto methodName =
            matchedReceiver.definedTypeName + "." + member->m_SymbolName;
        if (FunctionTable.count(methodName) == 0) {
          this->m_TypeErrorMessages.emplace_back(
              "Struct '" + matchedReceiver.definedTypeName +
                  "' has no method '" + member->m_SymbolName + "'",
              symbolInfo);
          return {};
        }

        if (member->m_SymbolName == "destructor") {
          this->m_TypeErrorMessages.emplace_back(
              "destructor cannot be called directly; use `drop " +
                  alias->m_SymbolName + ";` for explicit early release",
              symbolInfo, DiagnosticCode::SemanticOwnership);
          return {};
        }

        return methodName;
      }

      auto receiverInfo = getSymbolInfo(alias->m_SymbolName);
      if (receiverInfo.type == TypeSpecifier::SPEC_DEFINED) {
        return receiverInfo.definedTypeName + "." + member->m_SymbolName;
      }

      if (emitDiagnostics) {
        if (emitDiagnostics) {
          this->m_TypeErrorMessages.emplace_back(
              "Module alias '" + alias->m_SymbolName + "' was not declared",
              symbolInfo);
        }
        return {};
      }
    }
  }

  if (emitDiagnostics) {
    this->m_TypeErrorMessages.emplace_back(
        "Function call target must be a symbol or module alias member",
        symbolInfo);
  }
  return {};
}

SymbolInfo Visitor::preVisit(FuncCallAST &funcCallAst) {
  SymbolInfo symbolInfo;

  symbolInfo.begin = funcCallAst.m_SemLoc.begin;
  symbolInfo.end = funcCallAst.m_SemLoc.end;
  symbolInfo.line = funcCallAst.m_SemLoc.line;

  if (!m_TypeChecking) {
    return symbolInfo;
  }

  auto funcName =
      resolveFunctionCallName(funcCallAst.m_FuncSymbol.get(), symbolInfo, true);
  if (funcName.empty()) {
    return symbolInfo;
  }

  std::vector<IAST *> argNodes;
  auto collectArgs = [&](auto &self, IAST *node) -> void {
    if (node == nullptr) {
      return;
    }

    if (node->m_ExprKind == ExprKind::BinOp) {
      auto *binOp = static_cast<BinaryOpAST *>(node);
      if (binOp->m_BinOpKind == BinOpKind::B_COMM) {
        self(self, binOp->m_LHS.get());
        self(self, binOp->m_RHS.get());
        return;
      }
    }

    argNodes.push_back(node);
  };
  collectArgs(collectArgs, funcCallAst.m_Args.get());
  const bool hasImplicitMethodReceiver =
      methodCallReceiver(funcCallAst.m_FuncSymbol.get()) != nullptr;
  if (auto *receiver = methodCallReceiver(funcCallAst.m_FuncSymbol.get())) {
    argNodes.insert(argNodes.begin(), receiver);
  }

  if (funcName == "print") {
    if (argNodes.empty()) {
      this->m_TypeErrorMessages.emplace_back(
          "Builtin 'print' expects at least 1 argument", symbolInfo);
    }
    symbolInfo.type = TypeSpecifier::SPEC_VOID;
    return symbolInfo;
  }

  if (funcName == "input_int") {
    if (!argNodes.empty()) {
      this->m_TypeErrorMessages.emplace_back(
          "Builtin 'input_int' does not accept arguments", symbolInfo);
    }
    symbolInfo.type = TypeSpecifier::SPEC_I64;
    symbolInfo.indirectionLevel = 0;
    return symbolInfo;
  }

  if (funcName == "input_string") {
    if (!argNodes.empty()) {
      this->m_TypeErrorMessages.emplace_back(
          "Builtin 'input_string' does not accept arguments", symbolInfo);
    }
    symbolInfo.type = TypeSpecifier::SPEC_CHAR;
    symbolInfo.indirectionLevel = 1;
    return symbolInfo;
  }

  if (funcName == "clear_screen") {
    if (!argNodes.empty()) {
      this->m_TypeErrorMessages.emplace_back(
          "Builtin 'clear_screen' does not accept arguments", symbolInfo);
    }
    symbolInfo.type = TypeSpecifier::SPEC_VOID;
    return symbolInfo;
  }

  if (funcName == "flush_output") {
    if (!argNodes.empty()) {
      this->m_TypeErrorMessages.emplace_back(
          "Builtin 'flush_output' does not accept arguments", symbolInfo);
    }
    symbolInfo.type = TypeSpecifier::SPEC_VOID;
    return symbolInfo;
  }

  if (funcName == "sleep_ms") {
    if (argNodes.size() != 1) {
      this->m_TypeErrorMessages.emplace_back(
          "Builtin 'sleep_ms' expects exactly 1 argument", symbolInfo);
    }
    symbolInfo.type = TypeSpecifier::SPEC_VOID;
    return symbolInfo;
  }

  if (funcName == "enable_raw_input") {
    if (!argNodes.empty()) {
      this->m_TypeErrorMessages.emplace_back(
          "Builtin 'enable_raw_input' does not accept arguments", symbolInfo);
    }
    symbolInfo.type = TypeSpecifier::SPEC_VOID;
    return symbolInfo;
  }

  if (funcName == "disable_raw_input") {
    if (!argNodes.empty()) {
      this->m_TypeErrorMessages.emplace_back(
          "Builtin 'disable_raw_input' does not accept arguments", symbolInfo);
    }
    symbolInfo.type = TypeSpecifier::SPEC_VOID;
    return symbolInfo;
  }

  if (funcName == "read_key") {
    if (!argNodes.empty()) {
      this->m_TypeErrorMessages.emplace_back(
          "Builtin 'read_key' does not accept arguments", symbolInfo);
    }
    symbolInfo.type = TypeSpecifier::SPEC_I32;
    symbolInfo.indirectionLevel = 0;
    return symbolInfo;
  }

  if (funcName == "strong_count") {
    if (argNodes.size() != 1) {
      this->m_TypeErrorMessages.emplace_back(
          "Builtin 'strong_count' expects exactly 1 argument", symbolInfo);
    } else if (argNodes[0]->m_ExprKind != ExprKind::SymbolExpr) {
      this->m_TypeErrorMessages.emplace_back(
          "Builtin 'strong_count' expects a shared pointer symbol",
          symbolInfo);
    } else {
      auto *argSymbol = static_cast<SymbolAST *>(argNodes[0]);
      SymbolInfo lookupInfo;
      SymbolInfo matchedSymbol;
      auto argName = argSymbol->m_SymbolName;
      if (symbolValidation(argName, lookupInfo, matchedSymbol)) {
        if (matchedSymbol.indirectionLevel == 0 || matchedSymbol.isUnique) {
          this->m_TypeErrorMessages.emplace_back(
              "Builtin 'strong_count' expects a shared pointer symbol",
              symbolInfo, DiagnosticCode::SemanticOwnership);
        }
      }
    }
    symbolInfo.type = TypeSpecifier::SPEC_I64;
    symbolInfo.indirectionLevel = 0;
    return symbolInfo;
  }

  auto signatureIt = FunctionTable.find(funcName);
  if (signatureIt == FunctionTable.end()) {
    this->m_TypeErrorMessages.emplace_back(
        "Function '" + funcName + "' was not declared",
        symbolInfo);
    return symbolInfo;
  }

  const auto &signature = signatureIt->second;
  if (m_TypeChecking && m_CurrentFunctionIsStatic &&
      !signature.returnType.isStatic) {
    this->m_TypeErrorMessages.emplace_back(
        "static function cannot call non-static function '" + funcName + "'",
        symbolInfo);
  }
  if ((!signature.isVariadic && signature.params.size() != argNodes.size()) ||
      (signature.isVariadic && argNodes.size() < signature.params.size())) {
    this->m_TypeErrorMessages.emplace_back(
        "Function '" + funcName + "' expects " +
            (signature.isVariadic
                 ? "at least " + std::to_string(signature.params.size())
                 : std::to_string(signature.params.size())) +
            " argument(s), but " +
            std::to_string(argNodes.size()) + " provided",
        symbolInfo);
    return signature.returnType;
  }

  const bool callValueIsChecked =
      m_LastVarDecl || m_LastAssignment || m_LastRetExpr || m_LastBinOp;
  const auto expectedCallResultType =
      m_LastRetExpr ? m_LastFuncRetTypeInfo.type : m_ExpectedType;
  auto previousExpectedType = m_ExpectedType;
  auto previousLastSymbolInfo = m_LastSymbolInfo;
  auto previousDefinedTypeFlag = m_DefinedTypeFlag;
  auto previousLastBinOpHasPtr = m_LastBinOpHasAtLeastOnePtr;

  auto moveSourceSymbol = [](IAST *node) -> SymbolAST * {
    if (node == nullptr || node->m_ExprKind != ExprKind::UnaryOp) {
      return nullptr;
    }

    auto *unary = static_cast<UnaryOpAST *>(node);
    if (unary->m_UnaryOpKind != U_MOVE ||
        unary->m_Node->m_ExprKind != ExprKind::SymbolExpr) {
      return nullptr;
    }

    return static_cast<SymbolAST *>(unary->m_Node.get());
  };

  for (size_t i = 0; i < argNodes.size(); ++i) {
    const bool isVariadicArg = i >= signature.params.size();
    if (hasImplicitMethodReceiver && i == 0 && signature.params[i].isRef) {
      continue;
    }

    if (isVariadicArg) {
      if (argNodes[i]->m_ExprKind == ExprKind::SymbolExpr) {
        auto *argSymbol = static_cast<SymbolAST *>(argNodes[i]);
        SymbolInfo argInfo;
        SymbolInfo matchedSymbol;
        argInfo.symbolName = argSymbol->m_SymbolName;
        argInfo.begin = argSymbol->m_SemLoc.begin;
        argInfo.end = argSymbol->m_SemLoc.end;
        argInfo.line = argSymbol->m_SemLoc.line;
        symbolValidation(argSymbol->m_SymbolName, argInfo, matchedSymbol);
      }
      continue;
    }

    m_LastSymbolInfo = signature.params[i];
    if (m_LastSymbolInfo.isRef) {
      m_LastSymbolInfo.indirectionLevel += 1;
    }
    m_LastSymbolInfo.symbolId = m_SymbolId;
    m_LastSymbolInfo.scopeId = m_ScopeId;
    m_LastSymbolInfo.scopeLevel = m_ScopeLevel;
    m_ExpectedType = signature.params[i].type;
    m_DefinedTypeFlag = signature.params[i].type == TypeSpecifier::SPEC_DEFINED;
    m_LastBinOpHasAtLeastOnePtr = false;

    const bool parameterIsOwnedPointer =
        signature.params[i].indirectionLevel > 0 &&
        !signature.params[i].isRef && !signature.params[i].isSubscriptable;
    auto *argMoveSource = moveSourceSymbol(argNodes[i]);
    SymbolInfo movedCallSource;
    bool markMovedCallSource = false;
    if (parameterIsOwnedPointer && argMoveSource != nullptr) {
      SymbolInfo lookupInfo;
      SymbolInfo movedSource;
      auto sourceName = argMoveSource->m_SymbolName;
      if (symbolValidation(sourceName, lookupInfo, movedSource, true)) {
        if (movedSource.isNoMove) {
          SymbolInfo argInfo;
          argInfo.begin = argMoveSource->m_SemLoc.begin;
          argInfo.end = argMoveSource->m_SemLoc.end;
          argInfo.line = argMoveSource->m_SemLoc.line;
          this->m_TypeErrorMessages.emplace_back(
              "'nomove' pointer cannot be moved", argInfo,
              DiagnosticCode::SemanticOwnership);
        } else if (movedSource.indirectionLevel == 0 ||
            movedSource.isUnique != signature.params[i].isUnique) {
          SymbolInfo argInfo;
          argInfo.begin = argMoveSource->m_SemLoc.begin;
          argInfo.end = argMoveSource->m_SemLoc.end;
          argInfo.line = argMoveSource->m_SemLoc.line;
          this->m_TypeErrorMessages.emplace_back(
              "'move' argument requires matching pointer ownership kind",
              argInfo, DiagnosticCode::SemanticOwnership);
        } else {
          movedCallSource = movedSource;
          markMovedCallSource = true;
        }
      }
    }

    if (argNodes[i]->m_ExprKind == ExprKind::ScalarExpr) {
      auto *scalar = static_cast<ScalarOrLiteralAST *>(argNodes[i]);
      const auto expectedType = signature.params[i].type;
      const bool expectedInteger = IsIntegerType(expectedType);

      if ((expectedInteger && scalar->m_IsBoolean) ||
          (expectedType == TypeSpecifier::SPEC_BOOL && !scalar->m_IsBoolean)) {
        SymbolInfo argInfo;
        argInfo.begin = scalar->m_SemLoc.begin;
        argInfo.end = scalar->m_SemLoc.end;
        argInfo.line = scalar->m_SemLoc.line;
        this->m_TypeErrorMessages.emplace_back(
            "Function argument " + std::to_string(i + 1) +
                " is incompatible with parameter type '" +
                GetTypeStr(expectedType) + "'",
            argInfo);
        continue;
      }
    } else if (argNodes[i]->m_ExprKind == ExprKind::SymbolExpr) {
      auto *argSymbol = static_cast<SymbolAST *>(argNodes[i]);
      SymbolInfo argInfo;
      SymbolInfo matchedSymbol;
      argInfo.symbolName = argSymbol->m_SymbolName;
      argInfo.begin = argSymbol->m_SemLoc.begin;
      argInfo.end = argSymbol->m_SemLoc.end;
      argInfo.line = argSymbol->m_SemLoc.line;

      auto argSymbolName = argSymbol->m_SymbolName;
      if (!symbolValidation(argSymbolName, argInfo, matchedSymbol)) {
        continue;
      }

      const auto expectedType = signature.params[i].type;
      const auto actualType = matchedSymbol.type;
      if (signature.params[i].indirectionLevel > 0 &&
          !signature.params[i].isRef && signature.params[i].isUnique &&
          matchedSymbol.indirectionLevel > 0 && matchedSymbol.isUnique) {
        this->m_TypeErrorMessages.emplace_back(
            "unique pointer function argument cannot be copied; use 'move' "
            "to transfer ownership",
            argInfo, DiagnosticCode::SemanticOwnership);
        continue;
      }

      if (signature.params[i].isSubscriptable != matchedSymbol.isSubscriptable) {
        this->m_TypeErrorMessages.emplace_back(
            "Function argument " + std::to_string(i + 1) +
                " is incompatible with array parameter",
            argInfo);
        continue;
      }

      if (signature.params[i].isSubscriptable) {
        if (signature.params[i].arrayDimensions.size() !=
            matchedSymbol.arrayDimensions.size()) {
          this->m_TypeErrorMessages.emplace_back(
              "Function argument " + std::to_string(i + 1) +
                  " has incompatible array dimension count",
              argInfo);
          continue;
        }

        bool dimensionMismatch = false;
        for (size_t dim = 0; dim < signature.params[i].arrayDimensions.size();
             ++dim) {
          if (signature.params[i].arrayDimensions[dim] !=
              matchedSymbol.arrayDimensions[dim]) {
            dimensionMismatch = true;
            break;
          }
        }

        if (dimensionMismatch) {
          this->m_TypeErrorMessages.emplace_back(
              "Function argument " + std::to_string(i + 1) +
                  " has incompatible array size",
              argInfo);
          continue;
        }
      }

      const bool boolMismatch =
          (IsIntegerType(expectedType) && actualType == TypeSpecifier::SPEC_BOOL) ||
          (expectedType == TypeSpecifier::SPEC_BOOL && IsIntegerType(actualType));

      if (boolMismatch) {
        this->m_TypeErrorMessages.emplace_back(
            "Function argument " + std::to_string(i + 1) +
                " is incompatible with parameter type '" +
                GetTypeStr(expectedType) + "'",
            argInfo);
        continue;
      }
    }

    if (!(hasImplicitMethodReceiver && i == 0 && signature.params[i].isRef)) {
      argNodes[i]->acceptBefore(*this);
    }
    if (markMovedCallSource) {
      m_MovedUniqueSymbols.insert(SymbolStateKey(movedCallSource));
    }
  }

  m_ExpectedType = previousExpectedType;
  m_LastSymbolInfo = previousLastSymbolInfo;
  m_DefinedTypeFlag = previousDefinedTypeFlag;
  m_LastBinOpHasAtLeastOnePtr = previousLastBinOpHasPtr;

  symbolInfo = signature.returnType;
  symbolInfo.symbolName = funcName;

  if (callValueIsChecked && IsPrimitiveType(expectedCallResultType) &&
      IsPrimitiveType(symbolInfo.type) &&
      expectedCallResultType != symbolInfo.type) {
    const bool expectedInteger = IsIntegerType(expectedCallResultType);
    const bool actualInteger = IsIntegerType(symbolInfo.type);
    const bool boolMismatch =
        expectedCallResultType == TypeSpecifier::SPEC_BOOL ||
        symbolInfo.type == TypeSpecifier::SPEC_BOOL;

    if (boolMismatch || expectedCallResultType == TypeSpecifier::SPEC_VOID ||
        symbolInfo.type == TypeSpecifier::SPEC_VOID) {
      this->m_TypeErrorMessages.emplace_back(
          "Function '" + funcName + "' returns '" +
              GetTypeStr(symbolInfo.type) + "', but '" +
              GetTypeStr(expectedCallResultType) + "' is expected",
          symbolInfo);
    } else if (expectedInteger && actualInteger &&
               LosslessCasting(expectedCallResultType, symbolInfo.type)) {
      this->m_TypeWarningMessages.emplace_back(
          "Function '" + funcName + "' returns '" +
              GetTypeStr(symbolInfo.type) + "', which is casting to '" +
              GetTypeStr(expectedCallResultType) +
              "'. Potential data loss might be occured!",
          symbolInfo);
    }
  }

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
    if (paramAst.m_Symbol0 != nullptr) {
      symbolInfo = paramAst.m_Symbol0->acceptBefore(*this);
    } else {
      symbolInfo.begin = paramAst.m_SemLoc.begin;
      symbolInfo.end = paramAst.m_SemLoc.end;
      symbolInfo.line = paramAst.m_SemLoc.line;
    }
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
  symbolInfo.isNoMove = paramAst.m_IsNoMove;

  if (typeInfo != nullptr) {
    symbolInfo.isRef = typeInfo->m_IsRef;
    symbolInfo.isUnique = typeInfo->m_IsUniquePtr;
    symbolInfo.indirectionLevel = typeInfo->m_IndirectLevel;
  }
  symbolInfo.qualifierLevels =
      BuildQualifierLevels(paramAst.m_TypeQualifier, symbolInfo.indirectionLevel,
                           symbolInfo.isRef);

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
    if (typeInfo != nullptr && typeInfo->m_Symbol != nullptr &&
        typeInfo->m_Symbol->m_ExprKind == ExprKind::SymbolExpr) {
      auto *typeSymbol = static_cast<SymbolAST *>(typeInfo->m_Symbol.get());
      symbolInfo.definedTypeName = typeSymbol->m_SymbolName;
    }
  }
  symbolInfo.isParam = true;

  if (m_TypeChecking) {
    if (symbolInfo.isNoMove &&
        (symbolInfo.indirectionLevel == 0 || symbolInfo.isRef)) {
      this->m_TypeErrorMessages.emplace_back(
          "'nomove' requires a by-value pointer parameter", symbolInfo,
          DiagnosticCode::SemanticOwnership);
    }

    if (((symbolInfo.isConstPtr && !symbolInfo.isSubscriptable) &&
         (symbolInfo.indirectionLevel == 0 || symbolInfo.isRef)) ||
        (symbolInfo.isConstRef && !symbolInfo.isRef)) {
      // error
      this->m_TypeErrorMessages.emplace_back(
          "Invalid qualifier/type combination",
          symbolInfo, DiagnosticCode::SemanticInvalidQualifier);
    }
  }

  // symbolInfo.isNeededEval = true;

  return symbolInfo;
}
SymbolInfo Visitor::preVisit(RetAST &retAst) {
  SymbolInfo symbolInfo;
  const auto previousExpectedType = m_ExpectedType;
  const auto previousLastSymbolInfo = m_LastSymbolInfo;
  const auto previousDefinedTypeFlag = m_DefinedTypeFlag;
  const auto previousLastRetExpr = m_LastRetExpr;
  symbolInfo.begin = retAst.m_SemLoc.begin;
  symbolInfo.end = retAst.m_SemLoc.end;
  symbolInfo.line = retAst.m_SemLoc.line;

  if (m_TypeChecking) {
    this->m_LastRetExpr = true;
    m_LastSymbolInfo = m_LastFuncRetTypeInfo;
    m_ExpectedType = m_LastFuncRetTypeInfo.type;
    this->m_LastSymbolInfo.symbolId = m_SymbolId;
    this->m_LastSymbolInfo.scopeId = m_ScopeId;
    this->m_LastSymbolInfo.scopeLevel = m_ScopeLevel;
    SymbolInfo movedReturnSource;
    bool markMovedReturnSource = false;
    if (retAst.m_RetExpr != nullptr) {
      auto *retSymbol = dynamic_cast<SymbolAST *>(retAst.m_RetExpr.get());
      auto *retUnary = dynamic_cast<UnaryOpAST *>(retAst.m_RetExpr.get());
      const bool returnIsMove =
          retUnary != nullptr && retUnary->m_UnaryOpKind == U_MOVE;
      SymbolAST *sourceSymbol = retSymbol;
      if (returnIsMove && retUnary->m_Node->m_ExprKind == ExprKind::SymbolExpr) {
        sourceSymbol = static_cast<SymbolAST *>(retUnary->m_Node.get());
      }

      if (m_LastFuncRetTypeInfo.indirectionLevel > 0 &&
          !m_LastFuncRetTypeInfo.isRef && sourceSymbol != nullptr) {
        SymbolInfo lookupInfo;
        SymbolInfo source;
        auto sourceName = sourceSymbol->m_SymbolName;
        if (symbolValidation(sourceName, lookupInfo, source, true) &&
            source.indirectionLevel > 0) {
          if (m_LastFuncRetTypeInfo.isUnique && source.isUnique &&
              !returnIsMove) {
            this->m_TypeErrorMessages.emplace_back(
                "unique pointer return cannot be copied; use 'move' to "
                "transfer ownership",
                symbolInfo, DiagnosticCode::SemanticOwnership);
          } else if (returnIsMove && source.isNoMove) {
            this->m_TypeErrorMessages.emplace_back(
                "'nomove' pointer cannot be moved", symbolInfo,
                DiagnosticCode::SemanticOwnership);
          } else if (returnIsMove &&
                     source.isUnique != m_LastFuncRetTypeInfo.isUnique) {
            this->m_TypeErrorMessages.emplace_back(
                "'move' return requires matching pointer ownership kind",
                symbolInfo, DiagnosticCode::SemanticOwnership);
          } else if (returnIsMove) {
            movedReturnSource = source;
            markMovedReturnSource = true;
          }
        }
      }

      retAst.m_RetExpr->acceptBefore(*this);
      if (markMovedReturnSource) {
        m_MovedUniqueSymbols.insert(SymbolStateKey(movedReturnSource));
      }
    }
    this->m_LastRetExpr = previousLastRetExpr;
  }

  this->m_ExpectedType = previousExpectedType;
  this->m_LastSymbolInfo = previousLastSymbolInfo;
  this->m_DefinedTypeFlag = previousDefinedTypeFlag;
  this->m_LastRetExpr = previousLastRetExpr;

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
        const bool wasDereferenced = this->m_LastDereferenced;
        this->m_LastDereferenced = true;
        if (wasDereferenced) {
          this->m_DereferenceLevel += 1;
        } else {
          this->m_DereferenceLevel = 1;
        }
        symbolInfo = unaryOpAst.m_Node->acceptBefore(*this);
        if (!wasDereferenced) {
          this->m_LastDereferenced = false;
          this->m_DereferenceLevel = 1;
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
              "'typeof' operator cannot be used as an operand of the "
              "binary "
              "operation for now. It",
              symbolInfo);
        }
        break;
      case U_MOVE:
        if (m_LastBinOp) {
          this->m_TypeErrorMessages.emplace_back(
              "'move' operator cannot be used as an operand of the binary "
              "operation",
              symbolInfo);
        }
        symbolInfo = unaryOpAst.m_Node->acceptBefore(*this);
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
  if (typeAst.m_TypeSpec == TypeSpecifier::SPEC_DEFINED &&
      typeAst.m_Symbol != nullptr &&
      typeAst.m_Symbol->m_ExprKind == ExprKind::SymbolExpr) {
    auto *typeSymbol = static_cast<SymbolAST *>(typeAst.m_Symbol.get());
    symbolInfo.definedTypeName = typeSymbol->m_SymbolName;
  }

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

SymbolInfo Visitor::preVisit(StructAST &structAst) {
  SymbolInfo symbolInfo;
  symbolInfo.symbolName = structAst.m_Name;
  symbolInfo.definedTypeName = structAst.m_Name;
  symbolInfo.begin = structAst.m_SemLoc.begin;
  symbolInfo.end = structAst.m_SemLoc.end;
  symbolInfo.line = structAst.m_SemLoc.line;
  symbolInfo.type = TypeSpecifier::SPEC_DEFINED;

  StructInfo info;
  info.name = structAst.m_Name;
  info.traits = structAst.m_Traits;
  for (const auto &field : structAst.m_Fields) {
    if (info.fieldIndexes.count(field.name) != 0) {
      SymbolInfo fieldSymbol = symbolInfo;
      fieldSymbol.symbolName = field.name;
      this->m_TypeErrorMessages.emplace_back(
          "Redefinition of struct field '" + field.name + "'", fieldSymbol);
      continue;
    }

    if (field.type == TypeSpecifier::SPEC_DEFINED &&
        field.indirectionLevel == 0) {
      if (field.definedTypeName == structAst.m_Name) {
        SymbolInfo fieldSymbol = symbolInfo;
        fieldSymbol.symbolName = field.name;
        this->m_TypeErrorMessages.emplace_back(
            "Struct cannot contain itself by value", fieldSymbol);
      } else if (m_TypeChecking &&
                 this->m_TypeTable.count(field.definedTypeName) == 0) {
        SymbolInfo fieldSymbol = symbolInfo;
        fieldSymbol.symbolName = field.name;
        this->m_TypeErrorMessages.emplace_back(
            "Unknown type '" + field.definedTypeName + "'", fieldSymbol);
      }
    }

    info.fieldIndexes[field.name] = info.fields.size();
    info.fields.push_back(field);
  }

  StructTable[info.name] = std::move(info);

  if (m_TypeChecking) {
    const auto &registered = StructTable[structAst.m_Name];
    for (const auto &traitName : registered.traits) {
      auto traitIt = TraitTable.find(traitName);
      if (traitIt == TraitTable.end()) {
        SymbolInfo traitSymbol = symbolInfo;
        traitSymbol.symbolName = traitName;
        this->m_TypeErrorMessages.emplace_back(
            "Unknown trait '" + traitName + "'", traitSymbol);
        continue;
      }

      for (const auto &requirement : traitIt->second.requirements) {
        const auto methodName = structAst.m_Name + "." + requirement.name;
        if (FunctionTable.count(methodName) == 0) {
          SymbolInfo requirementSymbol = symbolInfo;
          requirementSymbol.symbolName = requirement.name;
          this->m_TypeErrorMessages.emplace_back(
              "Struct '" + structAst.m_Name + "' does not satisfy trait '" +
                  traitName + "': missing method '" + requirement.name + "'",
              requirementSymbol);
        }
      }
    }
  }

  return symbolInfo;
}

SymbolInfo Visitor::preVisit(TraitAST &traitAst) {
  SymbolInfo symbolInfo;
  symbolInfo.symbolName = traitAst.m_Name;
  symbolInfo.definedTypeName = traitAst.m_Name;
  symbolInfo.begin = traitAst.m_SemLoc.begin;
  symbolInfo.end = traitAst.m_SemLoc.end;
  symbolInfo.line = traitAst.m_SemLoc.line;
  symbolInfo.type = TypeSpecifier::SPEC_DEFINED;

  TraitInfo info;
  info.name = traitAst.m_Name;
  std::set<std::string> names;
  for (const auto &requirement : traitAst.m_Requirements) {
    if (names.count(requirement.name) != 0) {
      SymbolInfo requirementSymbol = symbolInfo;
      requirementSymbol.symbolName = requirement.name;
      this->m_TypeErrorMessages.emplace_back(
          "Redefinition of trait requirement '" + requirement.name + "'",
          requirementSymbol);
      continue;
    }
    names.insert(requirement.name);
    info.requirements.push_back(requirement);
  }

  TraitTable[info.name] = std::move(info);
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
      if (it.symbolInfo.symbolScope == SymbolScope::LoopSt &&
          it.symbolInfo.scopeLevel <= scopeLevel) {
        goto done;
      }

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

  if (m_TypeChecking && m_CurrentFunctionIsStatic && isGlobSymbol &&
      !matchedSymbol.isStatic && !matchedSymbol.isImported) {
    this->m_TypeErrorMessages.emplace_back(
        "static function cannot access non-static global symbol '" +
            symbolName + "'",
        symbolInfo);
  }

  if (m_TypeChecking &&
      m_DroppedSemanticSymbols.count(SymbolStateKey(matchedSymbol)) > 0 &&
      !noError) {
    this->m_TypeErrorMessages.emplace_back(
        "value '" + matchedSymbol.symbolName +
            "' was dropped and cannot be used before being reinitialized",
        symbolInfo, DiagnosticCode::SemanticOwnership);
  }

  return true;
}
