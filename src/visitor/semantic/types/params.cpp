#include <visitor/semantic/semantic_private.hpp>

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
    symbolInfo.isNullable = typeInfo->m_IsNullable;
    symbolInfo.isDynamicTraitObject = typeInfo->m_IsDynamicTraitObject;
  }
  symbolInfo.qualifierLevels =
      BuildQualifierLevels(paramAst.m_TypeQualifier, symbolInfo.indirectionLevel,
                           symbolInfo.isRef);

  if (paramAst.m_IsSubscriptable) {
    for (auto &dimensionNode : paramAst.m_ArrDim) {
      uint64_t dimension = 0;
      if (!TryGetNonNegativeIntegerLiteral(dimensionNode.get(), dimension)) {
        this->m_TypeErrorMessages.emplace_back(
            "Array parameter dimensions must be compile-time positive integer "
            "literals",
            symbolInfo);
        continue;
      }

      if (dimension == 0) {
        this->m_TypeErrorMessages.emplace_back(
            "Array parameter dimensions must be greater than zero",
            symbolInfo);
        continue;
      }

      if (dimension > std::numeric_limits<size_t>::max()) {
        this->m_TypeErrorMessages.emplace_back(
            "Array parameter dimension is too large", symbolInfo);
        continue;
      }

      symbolInfo.arrayDimensions.push_back(static_cast<size_t>(dimension));
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

    if (symbolInfo.isDynamicTraitObject) {
      if (TraitTable.count(symbolInfo.definedTypeName) == 0) {
        this->m_TypeErrorMessages.emplace_back(
            "dynamic trait object target '" + symbolInfo.definedTypeName +
                "' must name a trait",
            symbolInfo);
      } else {
        this->m_TypeErrorMessages.emplace_back(
            "dynamic trait object ABI/vtable lowering is not implemented yet",
            symbolInfo);
      }
    }
  }

  // symbolInfo.isNeededEval = true;

  return symbolInfo;
}
