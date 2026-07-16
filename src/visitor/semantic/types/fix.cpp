#include <visitor/semantic/semantic_private.hpp>

SymbolInfo Visitor::preVisit(FixAST &fixAst) {
  SymbolInfo symbolInfo;

  if (m_TypeChecking) {
    if (fixAst.m_Symbol == nullptr ||
        fixAst.m_Symbol->m_ExprKind != ExprKind::SymbolExpr) {
      symbolInfo.begin = fixAst.m_SemLoc.begin;
      symbolInfo.end = fixAst.m_SemLoc.end;
      symbolInfo.line = fixAst.m_SemLoc.line;
      this->m_TypeErrorMessages.emplace_back(
          "increment/decrement target must be a symbol", symbolInfo);
      return symbolInfo;
    }

    auto *symbol = static_cast<SymbolAST *>(fixAst.m_Symbol.get());
    symbolInfo.symbolName = symbol->m_SymbolName;
    symbolInfo.begin = symbol->m_SemLoc.begin;
    symbolInfo.end = symbol->m_SemLoc.end;
    symbolInfo.line = symbol->m_SemLoc.line;

    SymbolInfo matchedSymbol;
    this->m_LastSymbolInfo = symbolInfo;
    this->m_LastSymbolInfo.symbolId = m_SymbolId;
    this->m_LastSymbolInfo.scopeId = m_ScopeId;
    this->m_LastSymbolInfo.scopeLevel = m_ScopeLevel;
    this->m_LastFixExpr = true;
    const bool found =
        symbolValidation(symbol->m_SymbolName, symbolInfo, matchedSymbol);
    this->m_LastFixExpr = false;

    if (!found) {
      return symbolInfo;
    }

    symbolInfo = matchedSymbol;
    symbolInfo.begin = symbol->m_SemLoc.begin;
    symbolInfo.end = symbol->m_SemLoc.end;
    symbolInfo.line = symbol->m_SemLoc.line;

    if (!fixAst.m_IsIncrement && !fixAst.m_IsDecrement) {
      this->m_TypeErrorMessages.emplace_back(
          "increment/decrement expression is missing an operator", symbolInfo);
      return symbolInfo;
    }

    if (symbolInfo.indirectionLevel > 0 || symbolInfo.isRef ||
        symbolInfo.type == TypeSpecifier::SPEC_DEFINED ||
        !IsFixableScalarType(symbolInfo.type)) {
      this->m_TypeErrorMessages.emplace_back(
          "increment/decrement requires a mutable numeric scalar", symbolInfo);
    }

    if (symbolInfo.isConstVal || symbolInfo.isConstRef) {
      this->m_TypeErrorMessages.emplace_back(
          "const-qualified value is not assignable", symbolInfo,
          DiagnosticCode::SemanticConstAssignment);
    }

    if (symbolInfo.isReadOnly) {
      this->m_TypeErrorMessages.emplace_back(
          "readonly-qualified symbol is neither assignable with a value nor "
          "pointer with a reference",
          symbolInfo, DiagnosticCode::SemanticReadonlyAssignment);
    }
  }

  return symbolInfo;
}
