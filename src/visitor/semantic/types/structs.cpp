#include <visitor/semantic/semantic_private.hpp>

SymbolInfo Visitor::preVisit(StructAST &structAst) {
  SymbolInfo symbolInfo;
  symbolInfo.symbolName = structAst.m_Name;
  symbolInfo.definedTypeName = structAst.m_Name;
  symbolInfo.begin = structAst.m_SemLoc.begin;
  symbolInfo.end = structAst.m_SemLoc.end;
  symbolInfo.line = structAst.m_SemLoc.line;
  symbolInfo.type = TypeSpecifier::SPEC_DEFINED;

  StructInfo info;
  info.name = structAst.m_Name;
  info.traits = structAst.m_Traits;
  info.isFromIncludedModule = structAst.m_IsFromIncludedSource;
  for (const auto &field : structAst.m_Fields) {
    if (info.fieldIndexes.count(field.name) != 0) {
      SymbolInfo fieldSymbol = symbolInfo;
      fieldSymbol.symbolName = field.name;
      this->m_TypeErrorMessages.emplace_back(
          "Redefinition of struct field '" + field.name + "'", fieldSymbol);
      continue;
    }

    if (field.type == TypeSpecifier::SPEC_DEFINED &&
        field.indirectionLevel == 0) {
      if (field.definedTypeName == structAst.m_Name) {
        SymbolInfo fieldSymbol = symbolInfo;
        fieldSymbol.symbolName = field.name;
        this->m_TypeErrorMessages.emplace_back(
            "Struct cannot contain itself by value", fieldSymbol);
      } else if (m_TypeChecking &&
                 this->m_TypeTable.count(field.definedTypeName) == 0) {
        SymbolInfo fieldSymbol = symbolInfo;
        fieldSymbol.symbolName = field.name;
        this->m_TypeErrorMessages.emplace_back(
            "Unknown type '" + field.definedTypeName + "'", fieldSymbol);
      }
    }

    info.fieldIndexes[field.name] = info.fields.size();
    info.fields.push_back(field);
  }

  StructTable[info.name] = std::move(info);

  if (m_TypeChecking) {
    const auto &registered = StructTable[structAst.m_Name];
    for (const auto &traitName : registered.traits) {
      auto traitIt = TraitTable.find(traitName);
      if (traitIt == TraitTable.end()) {
        SymbolInfo traitSymbol = symbolInfo;
        traitSymbol.symbolName = traitName;
        this->m_TypeErrorMessages.emplace_back(
            "Unknown trait '" + traitName + "'", traitSymbol);
        continue;
      }

      for (const auto &requirement : traitIt->second.requirements) {
        const auto methodName = structAst.m_Name + "." + requirement.name;
        if (FunctionTable.count(methodName) == 0) {
          SymbolInfo requirementSymbol = symbolInfo;
          requirementSymbol.symbolName = requirement.name;
          this->m_TypeErrorMessages.emplace_back(
              "Struct '" + structAst.m_Name + "' does not satisfy trait '" +
                  traitName + "': missing method '" + requirement.name + "'",
              requirementSymbol);
        }
      }
    }
  }

  return symbolInfo;
}
