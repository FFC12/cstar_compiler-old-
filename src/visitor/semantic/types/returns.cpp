#include <visitor/semantic/semantic_private.hpp>

SymbolInfo Visitor::preVisit(RetAST &retAst) {
  SymbolInfo symbolInfo;
  const auto previousExpectedType = m_ExpectedType;
  const auto previousLastSymbolInfo = m_LastSymbolInfo;
  const auto previousDefinedTypeFlag = m_DefinedTypeFlag;
  const auto previousDefinedTypeName = m_DefinedTypeName;
  const auto previousLastRetExpr = m_LastRetExpr;
  symbolInfo.begin = retAst.m_SemLoc.begin;
  symbolInfo.end = retAst.m_SemLoc.end;
  symbolInfo.line = retAst.m_SemLoc.line;

  if (m_TypeChecking) {
    this->m_LastRetExpr = true;
    m_LastSymbolInfo = m_LastFuncRetTypeInfo;
    m_ExpectedType = m_LastFuncRetTypeInfo.type;
    m_DefinedTypeFlag =
        m_LastFuncRetTypeInfo.type == TypeSpecifier::SPEC_DEFINED;
    m_DefinedTypeName = m_LastFuncRetTypeInfo.definedTypeName;
    this->m_LastSymbolInfo.symbolId = m_SymbolId;
    this->m_LastSymbolInfo.scopeId = m_ScopeId;
    this->m_LastSymbolInfo.scopeLevel = m_ScopeLevel;
    SymbolInfo movedReturnSource;
    bool markMovedReturnSource = false;
    if (retAst.m_RetExpr != nullptr) {
      if (m_LastFuncRetTypeInfo.type == TypeSpecifier::SPEC_VOID &&
          m_LastFuncRetTypeInfo.indirectionLevel == 0) {
        this->m_TypeErrorMessages.emplace_back(
            "void function cannot return a value; use 'ret;'",
            symbolInfo);
        this->m_LastRetExpr = previousLastRetExpr;
        this->m_ExpectedType = previousExpectedType;
        this->m_LastSymbolInfo = previousLastSymbolInfo;
        this->m_DefinedTypeFlag = previousDefinedTypeFlag;
        this->m_DefinedTypeName = previousDefinedTypeName;
        return symbolInfo;
      }

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

      auto returnedInfo = retAst.m_RetExpr->acceptBefore(*this);
      if (returnedInfo.isNullable && !m_LastFuncRetTypeInfo.isNullable &&
          returnedInfo.indirectionLevel > 0 &&
          m_NonNullFlowSymbols.count(returnedInfo.symbolName) == 0) {
        this->m_TypeErrorMessages.emplace_back(
            "Nullable pointer cannot be returned as a non-null pointer "
            "without an `if (ptr)` non-null proof",
            symbolInfo, DiagnosticCode::SemanticQualifierMismatch);
      }
      for (const auto& expectedState : m_LastFuncRetTypeInfo.protocolStates) {
        auto actualIt = returnedInfo.protocolStates.find(expectedState.first);
        std::string actualState;
        if (actualIt != returnedInfo.protocolStates.end()) {
          actualState = actualIt->second;
        } else if (!returnedInfo.symbolName.empty()) {
          auto flowIt = m_LocalProtocolStates.find(returnedInfo.symbolName);
          if (flowIt != m_LocalProtocolStates.end()) {
            auto flowStateIt = flowIt->second.find(expectedState.first);
            if (flowStateIt != flowIt->second.end()) {
              actualState = flowStateIt->second;
            }
          }
        }
        if (!actualState.empty() && actualState != expectedState.second) {
          this->m_TypeErrorMessages.emplace_back(
              "return value state mismatch for protocol '" +
                  expectedState.first + "': expected '" +
                  expectedState.second + "', got '" + actualState + "'",
              symbolInfo);
        }
      }
      if (markMovedReturnSource) {
        m_MovedUniqueSymbols.insert(SymbolStateKey(movedReturnSource));
      }
    }
    this->m_LastRetExpr = previousLastRetExpr;
  }

  this->m_ExpectedType = previousExpectedType;
  this->m_LastSymbolInfo = previousLastSymbolInfo;
  this->m_DefinedTypeFlag = previousDefinedTypeFlag;
  this->m_DefinedTypeName = previousDefinedTypeName;
  this->m_LastRetExpr = previousLastRetExpr;

  return symbolInfo;
}
