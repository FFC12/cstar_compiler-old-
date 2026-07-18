#include <visitor/semantic/semantic_private.hpp>

SymbolInfo Visitor::preVisit(AttributeAST &attributeAst) {
  SymbolInfo symbolInfo;
  symbolInfo.symbolName = attributeAst.m_Name;
  symbolInfo.begin = attributeAst.m_SemLoc.begin;
  symbolInfo.end = attributeAst.m_SemLoc.end;
  symbolInfo.line = attributeAst.m_SemLoc.line;
  symbolInfo.type = TypeSpecifier::SPEC_VOID;
  return symbolInfo;
}

SymbolInfo Visitor::preVisit(MacroAST &macroAst) {
  SymbolInfo symbolInfo;
  symbolInfo.symbolName = macroAst.m_Name;
  symbolInfo.begin = macroAst.m_SemLoc.begin;
  symbolInfo.end = macroAst.m_SemLoc.end;
  symbolInfo.line = macroAst.m_SemLoc.line;
  symbolInfo.type = TypeSpecifier::SPEC_VOID;
  return symbolInfo;
}

SymbolInfo Visitor::preVisit(DirectiveAST &directiveAst) {
  SymbolInfo symbolInfo;
  symbolInfo.symbolName = directiveAst.m_Name;
  symbolInfo.begin = directiveAst.m_SemLoc.begin;
  symbolInfo.end = directiveAst.m_SemLoc.end;
  symbolInfo.line = directiveAst.m_SemLoc.line;
  symbolInfo.type = TypeSpecifier::SPEC_VOID;
  return symbolInfo;
}

std::string SymbolStateKey(const SymbolInfo &symbolInfo) {
  return std::to_string(symbolInfo.scopeId) + "#" +
         std::to_string(symbolInfo.scopeLevel) + "#" + symbolInfo.symbolName;
}

bool SplitStructMethodName(const std::string &name,
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

std::string SemValueOperatorMethodName(BinOpKind kind) {
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

bool IsFixableScalarType(TypeSpecifier type) {
  switch (type) {
    case TypeSpecifier::SPEC_I8:
    case TypeSpecifier::SPEC_I16:
    case TypeSpecifier::SPEC_I32:
    case TypeSpecifier::SPEC_I64:
    case TypeSpecifier::SPEC_INT:
    case TypeSpecifier::SPEC_U8:
    case TypeSpecifier::SPEC_U16:
    case TypeSpecifier::SPEC_U32:
    case TypeSpecifier::SPEC_U64:
    case TypeSpecifier::SPEC_U128:
    case TypeSpecifier::SPEC_UINT:
    case TypeSpecifier::SPEC_ISIZE:
    case TypeSpecifier::SPEC_USIZE:
    case TypeSpecifier::SPEC_F32:
    case TypeSpecifier::SPEC_F64:
    case TypeSpecifier::SPEC_FLOAT:
      return true;
    default:
      return false;
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

std::vector<TypeQualifier> BuildQualifierLevels(
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

size_t GetBitSize(TypeSpecifier typeSpecifier) {
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

bool TryGetNonNegativeIntegerLiteral(IAST *node, uint64_t &value) {
  auto *scalar = dynamic_cast<ScalarOrLiteralAST *>(node);
  if (scalar == nullptr || scalar->isFloat()) {
    return false;
  }

  const auto literal = scalar->getValue();
  if (literal.empty() || literal[0] == '-') {
    return false;
  }

  try {
    size_t parsedChars = 0;
    value = std::stoull(literal, &parsedChars);
    return parsedChars == literal.size();
  } catch (...) {
    return false;
  }
}

uint64_t ProductOfDimensions(const std::vector<size_t> &dimensions) {
  uint64_t product = 1;
  for (auto dimension : dimensions) {
    if (dimension == 0 ||
        product > std::numeric_limits<uint64_t>::max() / dimension) {
      return 0;
    }
    product *= dimension;
  }
  return product;
}

uint64_t PrimitiveTypeStorageBytes(TypeSpecifier typeSpecifier) {
  if (typeSpecifier == TypeSpecifier::SPEC_DEFINED) {
    return 0;
  }
  return std::max<uint64_t>(1, GetBitSize(typeSpecifier) / 8);
}

bool IsConstantArrayIndexOutOfBounds(int64_t index, size_t dimension) {
  if (dimension == 0) {
    return true;
  }

  if (index >= 0) {
    return static_cast<uint64_t>(index) >= dimension;
  }

  if (index == std::numeric_limits<int64_t>::min()) {
    return true;
  }

  return static_cast<uint64_t>(-index) > dimension;
}

bool Visitor::tryGetConstantIntegerLiteral(IAST *node, int64_t &value) const {
  auto *scalar = dynamic_cast<ScalarOrLiteralAST *>(node);
  if (scalar != nullptr && !scalar->isFloat()) {
    try {
      size_t parsedChars = 0;
      value = std::stoll(scalar->getValue(), &parsedChars);
      return parsedChars == scalar->getValue().size();
    } catch (...) {
      return false;
    }
  }

  auto *unary = dynamic_cast<UnaryOpAST *>(node);
  if (unary == nullptr ||
      (unary->m_UnaryOpKind != UnaryOpKind::U_NEGATIVE &&
       unary->m_UnaryOpKind != UnaryOpKind::U_POSITIVE)) {
    return false;
  }

  int64_t innerValue = 0;
  if (!tryGetConstantIntegerLiteral(unary->m_Node.get(), innerValue)) {
    return false;
  }

  value = unary->m_UnaryOpKind == UnaryOpKind::U_NEGATIVE ? -innerValue
                                                          : innerValue;
  return true;
}

std::string GetTypeStr(TypeSpecifier typeSpecifier) {
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
bool IsPrimitiveType(TypeSpecifier typeSpecifier) {
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

bool IsIntegerType(TypeSpecifier typeSpecifier) {
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

bool IsFloatingType(TypeSpecifier typeSpecifier) {
  switch (typeSpecifier) {
    case SPEC_F32:
    case SPEC_F64:
    case SPEC_FLOAT:
      return true;
    default:
      return false;
  }
}

bool IsNumericPrimitiveType(TypeSpecifier typeSpecifier) {
  return IsIntegerType(typeSpecifier) || IsFloatingType(typeSpecifier) ||
         typeSpecifier == SPEC_CHAR || typeSpecifier == SPEC_UCHAR;
}

bool IsVoidValue(const SymbolInfo &symbolInfo) {
  return symbolInfo.type == TypeSpecifier::SPEC_VOID &&
         symbolInfo.indirectionLevel == 0 && !symbolInfo.isRef;
}

bool IsPointerLike(const SymbolInfo &symbolInfo) {
  return symbolInfo.indirectionLevel > 0 || symbolInfo.isRef ||
         symbolInfo.isSubscriptable;
}

bool SameQualifierShape(const SymbolInfo &lhs,
                               const SymbolInfo &rhs) {
  return lhs.isUnique == rhs.isUnique && lhs.isRef == rhs.isRef &&
         lhs.isConstVal == rhs.isConstVal &&
         lhs.isConstPtr == rhs.isConstPtr &&
         lhs.isConstRef == rhs.isConstRef &&
         lhs.isReadOnly == rhs.isReadOnly;
}

bool SameTypeShape(const SymbolInfo &lhs, const SymbolInfo &rhs) {
  return lhs.type == rhs.type && lhs.definedTypeName == rhs.definedTypeName &&
         lhs.indirectionLevel == rhs.indirectionLevel &&
         lhs.isSubscriptable == rhs.isSubscriptable &&
         SameQualifierShape(lhs, rhs);
}

SymbolInfo InferScalarLiteralType(ScalarOrLiteralAST *scalar) {
  SymbolInfo info;
  if (scalar == nullptr) {
    return info;
  }

  const auto loc = scalar->getSemLoc();
  info.begin = loc.begin;
  info.end = loc.end;
  info.line = loc.line;
  info.value = scalar->getValue();
  if (scalar->isNil()) {
    info.type = TypeSpecifier::SPEC_NIL;
  } else if (scalar->isLiteral()) {
    info.type = TypeSpecifier::SPEC_CHAR;
    info.indirectionLevel = 1;
    info.isConstVal = true;
  } else if (scalar->isBoolean()) {
    info.type = TypeSpecifier::SPEC_BOOL;
  } else if (scalar->isFloat()) {
    info.type = TypeSpecifier::SPEC_F64;
  } else if (scalar->isLetter()) {
    info.type = TypeSpecifier::SPEC_CHAR;
  } else {
    info.type = TypeSpecifier::SPEC_I64;
  }
  return info;
}

bool ContainsTernarySelectSideEffect(IAST *node) {
  if (node == nullptr) {
    return false;
  }

  switch (node->getExprKind()) {
    case ExprKind::FuncCallExpr:
    case ExprKind::AssignmentExpr:
    case ExprKind::NewExpr:
      return true;
    default:
      return false;
  }
}

const EnumMemberInfo *SemFindEnumMember(const std::string &enumName,
                                            const std::string &memberName) {
  auto enumIt = Visitor::EnumTable.find(enumName);
  if (enumIt == Visitor::EnumTable.end()) {
    return nullptr;
  }

  auto memberIt = enumIt->second.memberIndexes.find(memberName);
  if (memberIt == enumIt->second.memberIndexes.end()) {
    return nullptr;
  }

  return &enumIt->second.members[memberIt->second];
}

bool IsEnumBitwiseOperator(BinOpKind kind) {
  return kind == B_AND || kind == B_OR || kind == B_XOR;
}

bool IsEnumEqualityOperator(BinOpKind kind) {
  return kind == B_EQ || kind == B_NEQ;
}

bool IsLogicalOperator(BinOpKind kind) {
  return kind == B_LAND || kind == B_LOR;
}

bool IsComparisonOperator(BinOpKind kind) {
  switch (kind) {
    case B_EQ:
    case B_NEQ:
    case B_LT:
    case B_LTEQ:
    case B_GT:
    case B_GTEQ:
      return true;
    default:
      return false;
  }
}

bool IsOrderedComparisonOperator(BinOpKind kind) {
  return kind == B_LT || kind == B_LTEQ || kind == B_GT || kind == B_GTEQ;
}

bool IsValueOperator(BinOpKind kind) {
  switch (kind) {
    case B_ADD:
    case B_SUB:
    case B_MUL:
    case B_DIV:
    case B_MOD:
    case B_AND:
    case B_OR:
    case B_XOR:
    case B_SHL:
    case B_SHR:
      return true;
    default:
      return false;
  }
}

SymbolInfo MakeBoolInfo(const SemanticLoc &loc) {
  SymbolInfo info;
  info.begin = loc.begin;
  info.end = loc.end;
  info.line = loc.line;
  info.type = TypeSpecifier::SPEC_BOOL;
  return info;
}

bool LosslessCasting(TypeSpecifier target, TypeSpecifier source) {
  return GetBitSize(target) < GetBitSize(source);
}

// It must be binaryExpr the param
size_t Visitor::getIndexesOfArray(IAST &expr) {
  BinaryOpAST *binaryExpr = nullptr;
  size_t i = 1;

  if (expr.m_ExprKind == BinOp) {
    binaryExpr = (BinaryOpAST *)&expr;
    if (binaryExpr->m_BinOpKind != B_MARRS) {
      return 1;
    }

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
                                               const std::string &s) {
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

  const bool stringLiteralTarget =
      this->m_ExpectedType == TypeSpecifier::SPEC_CHAR &&
      this->m_LastSymbolInfo.indirectionLevel > 0;
  auto extra = stringLiteralTarget &&
                       (this->m_LastSymbolInfo.isUnique ||
                        (!this->m_LastSymbolInfo.isConstVal &&
                         !this->m_LastSymbolInfo.isReadOnly))
                   ? "String literal requires an immutable non-owning target: "
                     "use 'const char*' or 'readonly char*'; use an explicit "
                     "copy for 'char^'"
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
