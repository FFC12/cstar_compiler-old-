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

  return symbolInfo;
}
