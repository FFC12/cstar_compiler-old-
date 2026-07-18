#include <visitor/semantic/semantic_private.hpp>

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

  const bool isHeapAllocation =
      m_SemanticHeapAllocations.count(SymbolStateKey(matchedSymbol)) > 0;
  if (matchedSymbol.type != TypeSpecifier::SPEC_DEFINED &&
      !(isHeapAllocation && matchedSymbol.indirectionLevel > 0 &&
        !matchedSymbol.isRef)) {
    this->m_TypeErrorMessages.emplace_back(
        "`drop` expects a struct value, struct ownership pointer, or "
        "`new`-allocated ownership pointer",
        symbolInfo, DiagnosticCode::SemanticOwnership);
    return symbolInfo;
  }

  m_DroppedSemanticSymbols.insert(SymbolStateKey(matchedSymbol));
  if (matchedSymbol.indirectionLevel > 0) {
    m_MovedUniqueSymbols.insert(SymbolStateKey(matchedSymbol));
  }

  return symbolInfo;
}
