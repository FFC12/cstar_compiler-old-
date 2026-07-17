#include <visitor/semantic/semantic_private.hpp>

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
  if (!typeAst.m_StateQualifiers.empty()) {
    for (const auto& state : typeAst.m_StateQualifiers) {
      bool matched = false;
      for (const auto& entry : ProtocolTable) {
        const auto& protocol = entry.second;
        if (protocol.targetTypeName != symbolInfo.definedTypeName) {
          continue;
        }
        if (std::find(protocol.states.begin(), protocol.states.end(), state) !=
            protocol.states.end()) {
          symbolInfo.protocolStates[protocol.name] = state;
          matched = true;
          break;
        }
      }
      if (!matched && m_TypeChecking) {
        symbolInfo.begin = typeAst.m_SemLoc.begin;
        symbolInfo.end = typeAst.m_SemLoc.end;
        symbolInfo.line = typeAst.m_SemLoc.line;
        this->m_TypeErrorMessages.emplace_back(
            "Unknown protocol state qualifier '" + state + "' for type '" +
                symbolInfo.definedTypeName + "'",
            symbolInfo);
      }
    }
  }

  return symbolInfo;
}
