#include <visitor/semantic/semantic_private.hpp>

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
        if (symbolInfo.isNullable &&
            m_NonNullFlowSymbols.count(symbolInfo.symbolName) == 0) {
          this->m_TypeErrorMessages.emplace_back(
              "Nullable pointer must be proven non-null before dereference; "
              "guard it with `if (ptr)` first",
              symbolInfo, DiagnosticCode::SemanticQualifierMismatch);
        }
        if (!wasDereferenced) {
          this->m_LastDereferenced = false;
          this->m_DereferenceLevel = 1;
        }
        if (symbolInfo.indirectionLevel == 0 && !symbolInfo.isRef) {
          this->m_TypeErrorMessages.emplace_back(
              "Dereference requires a pointer or reference operand",
              symbolInfo);
        } else if (symbolInfo.indirectionLevel > 0) {
          symbolInfo.indirectionLevel -= 1;
          symbolInfo.isUnique = false;
          symbolInfo.isRef = false;
          symbolInfo.isNullable = false;
          symbolInfo.isSubscriptable = false;
          symbolInfo.arrayDimensions.clear();
        } else if (symbolInfo.isRef) {
          symbolInfo.isRef = false;
        }
        break;
      }
      case U_SIZEOF:
      case U_PREFIX:
      case U_POSTFIX:
        unaryOpAst.m_Node->acceptBefore(*this);
        break;
      case U_POSITIVE:
      case U_NEGATIVE: {
        symbolInfo = unaryOpAst.m_Node->acceptBefore(*this);
        if (auto *scalar =
                dynamic_cast<ScalarOrLiteralAST *>(unaryOpAst.m_Node.get())) {
          symbolInfo = InferScalarLiteralType(scalar);
          if (IsNumericPrimitiveType(m_LastSymbolInfo.type) &&
              m_LastSymbolInfo.indirectionLevel == 0) {
            symbolInfo.type = m_LastSymbolInfo.type;
          }
        }

        if (symbolInfo.indirectionLevel > 0 || symbolInfo.isRef ||
            !IsNumericPrimitiveType(symbolInfo.type)) {
          this->m_TypeErrorMessages.emplace_back(
              std::string(unaryOpAst.m_UnaryOpKind == U_POSITIVE ? "Unary '+'"
                                                                 : "Unary '-'") +
                  " requires a numeric operand",
              symbolInfo);
        }
        break;
      }
      case U_NOT: {
        const auto previousExpectedType = m_ExpectedType;
        const auto previousLastSymbolInfo = m_LastSymbolInfo;
        const auto previousDefinedTypeFlag = m_DefinedTypeFlag;
        const auto previousDefinedTypeName = m_DefinedTypeName;
        const auto previousLastCondExpr = m_LastCondExpr;

        m_LastCondExpr = true;
        m_ExpectedType = TypeSpecifier::SPEC_BOOL;
        m_LastSymbolInfo.type = TypeSpecifier::SPEC_BOOL;
        m_LastSymbolInfo.indirectionLevel = 0;
        m_LastSymbolInfo.isRef = false;
        m_DefinedTypeFlag = false;
        m_DefinedTypeName.clear();

        unaryOpAst.m_Node->acceptBefore(*this);

        m_ExpectedType = previousExpectedType;
        m_LastSymbolInfo = previousLastSymbolInfo;
        m_DefinedTypeFlag = previousDefinedTypeFlag;
        m_DefinedTypeName = previousDefinedTypeName;
        m_LastCondExpr = previousLastCondExpr;

        symbolInfo.type = TypeSpecifier::SPEC_BOOL;
        symbolInfo.indirectionLevel = 0;
        symbolInfo.isRef = false;
        break;
      }
      case U_BINNEG: {
        if (auto *scalar =
                dynamic_cast<ScalarOrLiteralAST *>(unaryOpAst.m_Node.get())) {
          symbolInfo = InferScalarLiteralType(scalar);
          if (!scalar->isFloat() && IsIntegerType(m_LastSymbolInfo.type) &&
              m_LastSymbolInfo.indirectionLevel == 0) {
            symbolInfo.type = m_LastSymbolInfo.type;
          }
        } else {
          symbolInfo = unaryOpAst.m_Node->acceptBefore(*this);
        }
        const bool isEnum =
            symbolInfo.type == TypeSpecifier::SPEC_DEFINED &&
            EnumTable.count(symbolInfo.definedTypeName) != 0;
        if (isEnum) {
          const auto &enumInfo = EnumTable[symbolInfo.definedTypeName];
          if (!enumInfo.isFlags) {
            this->m_TypeErrorMessages.emplace_back(
                "Bitwise '~' requires a 'flags enum'; enum '" +
                    enumInfo.name + "' is scalar",
                symbolInfo);
          }
        } else if (symbolInfo.indirectionLevel > 0 ||
                   !IsIntegerType(symbolInfo.type)) {
          this->m_TypeErrorMessages.emplace_back(
              "Bitwise '~' requires an integer or flags enum operand",
              symbolInfo);
        }
        break;
      }
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
