#include <visitor/semantic/semantic_private.hpp>

SymbolInfo Visitor::preVisit(TraitAST &traitAst) {
  SymbolInfo symbolInfo;
  symbolInfo.symbolName = traitAst.m_Name;
  symbolInfo.definedTypeName = traitAst.m_Name;
  symbolInfo.begin = traitAst.m_SemLoc.begin;
  symbolInfo.end = traitAst.m_SemLoc.end;
  symbolInfo.line = traitAst.m_SemLoc.line;
  symbolInfo.type = TypeSpecifier::SPEC_DEFINED;

  TraitInfo info;
  info.name = traitAst.m_Name;
  info.languageItem = traitAst.m_LanguageItem;
  std::set<std::string> names;
  for (const auto &requirement : traitAst.m_Requirements) {
    if (names.count(requirement.name) != 0) {
      SymbolInfo requirementSymbol = symbolInfo;
      requirementSymbol.symbolName = requirement.name;
      this->m_TypeErrorMessages.emplace_back(
          "Redefinition of trait requirement '" + requirement.name + "'",
          requirementSymbol);
      continue;
    }
    names.insert(requirement.name);
    info.requirements.push_back(requirement);
  }

  if (!info.languageItem.empty()) {
    for (const auto &entry : TraitTable) {
      if (entry.second.languageItem == info.languageItem &&
          entry.first != info.name) {
        this->m_TypeErrorMessages.emplace_back(
            "Language item '" + info.languageItem +
                "' is already bound to trait '" + entry.first + "'",
            symbolInfo);
      }
    }

    if (info.languageItem == "allocator") {
      if (names.count("alloc") == 0 || names.count("free") == 0) {
        this->m_TypeErrorMessages.emplace_back(
            "Language item 'allocator' requires trait methods 'alloc' and "
            "'free'",
            symbolInfo);
      }
    }
  }

  TraitTable[info.name] = std::move(info);
  return symbolInfo;
}
