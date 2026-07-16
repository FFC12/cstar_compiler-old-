#include <visitor/semantic/semantic_private.hpp>

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
