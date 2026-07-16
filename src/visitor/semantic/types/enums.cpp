#include <visitor/semantic/semantic_private.hpp>

SymbolInfo Visitor::preVisit(EnumAST &enumAst) {
  SymbolInfo symbolInfo;
  symbolInfo.symbolName = enumAst.m_Name;
  symbolInfo.definedTypeName = enumAst.m_Name;
  symbolInfo.begin = enumAst.m_SemLoc.begin;
  symbolInfo.end = enumAst.m_SemLoc.end;
  symbolInfo.line = enumAst.m_SemLoc.line;
  symbolInfo.type = TypeSpecifier::SPEC_DEFINED;

  EnumInfo info;
  info.name = enumAst.m_Name;
  info.underlyingType = enumAst.m_UnderlyingType;
  info.isFlags = enumAst.m_IsFlags;

  for (const auto &member : enumAst.m_Members) {
    if (info.memberIndexes.count(member.name) != 0) {
      SymbolInfo memberSymbol = symbolInfo;
      memberSymbol.symbolName = member.name;
      this->m_TypeErrorMessages.emplace_back(
          "Redefinition of enum member '" + member.name + "'", memberSymbol);
      continue;
    }

    info.memberIndexes[member.name] = info.members.size();
    info.members.push_back(member);
  }

  EnumTable[info.name] = std::move(info);
  return symbolInfo;
}
