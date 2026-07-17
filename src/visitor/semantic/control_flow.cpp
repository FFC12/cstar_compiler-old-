#include <visitor/semantic/semantic_private.hpp>

SymbolInfo Visitor::preVisit(IfStmtAST &ifStmtAst) {
  SymbolInfo symbolInfo;

  auto scopeLevel = this->m_ScopeLevel;

  for (auto &block : ifStmtAst.m_Cond) {
    // type checking for condition
    enterScope(false);
    std::string narrowedNullableSymbol;
    if (m_TypeChecking) {
      this->m_LastCondExpr = true;
      SymbolInfo conditionInfo = block.second.first->acceptBefore(*this);
      this->m_LastCondExpr = false;
      if (block.second.first->m_ExprKind == ExprKind::SymbolExpr) {
        auto *conditionSymbol =
            static_cast<SymbolAST *>(block.second.first.get());
        SymbolInfo lookupInfo;
        lookupInfo.symbolName = conditionSymbol->m_SymbolName;
        lookupInfo.begin = conditionSymbol->m_SemLoc.begin;
        lookupInfo.end = conditionSymbol->m_SemLoc.end;
        lookupInfo.line = conditionSymbol->m_SemLoc.line;
        SymbolInfo resolvedCondition;
        auto conditionName = conditionSymbol->m_SymbolName;
        if (symbolValidation(conditionName, lookupInfo, resolvedCondition,
                             true)) {
          conditionInfo = resolvedCondition;
        }
      }
      if (conditionInfo.isNullable && conditionInfo.indirectionLevel > 0 &&
          !conditionInfo.isRef && !conditionInfo.symbolName.empty()) {
        narrowedNullableSymbol = conditionInfo.symbolName;
        m_NonNullFlowSymbols.insert(narrowedNullableSymbol);
      }
    }

    // there is only one node actually..
    for (auto &node : block.second.second) {
      scopeHandler(node, SymbolScope::IfSt);
    }
    if (!narrowedNullableSymbol.empty()) {
      m_NonNullFlowSymbols.erase(narrowedNullableSymbol);
    }
    exitScope(false);
  }

  if (ifStmtAst.m_HasElif) {
    for (auto &entry : ifStmtAst.m_ElseIfs) {
      // type checking for condition
      enterScope(false);
      std::string narrowedNullableSymbol;
      if (m_TypeChecking) {
        this->m_LastCondExpr = true;
        SymbolInfo conditionInfo = entry.second.first->acceptBefore(*this);
        this->m_LastCondExpr = false;
        if (entry.second.first->m_ExprKind == ExprKind::SymbolExpr) {
          auto *conditionSymbol =
              static_cast<SymbolAST *>(entry.second.first.get());
          SymbolInfo lookupInfo;
          lookupInfo.symbolName = conditionSymbol->m_SymbolName;
          lookupInfo.begin = conditionSymbol->m_SemLoc.begin;
          lookupInfo.end = conditionSymbol->m_SemLoc.end;
          lookupInfo.line = conditionSymbol->m_SemLoc.line;
          SymbolInfo resolvedCondition;
          auto conditionName = conditionSymbol->m_SymbolName;
          if (symbolValidation(conditionName, lookupInfo, resolvedCondition,
                               true)) {
            conditionInfo = resolvedCondition;
          }
        }
        if (conditionInfo.isNullable && conditionInfo.indirectionLevel > 0 &&
            !conditionInfo.isRef && !conditionInfo.symbolName.empty()) {
          narrowedNullableSymbol = conditionInfo.symbolName;
          m_NonNullFlowSymbols.insert(narrowedNullableSymbol);
        }
      }
      auto &elseIfBlock = entry.second;
      // manually increasing
      this->m_SymbolId++;
      for (auto &node : elseIfBlock.second) {
        scopeHandler(node, SymbolScope::IfSt);
      }
      if (!narrowedNullableSymbol.empty()) {
        m_NonNullFlowSymbols.erase(narrowedNullableSymbol);
      }
      exitScope(false);
    }
  }

  enterScope(false);

  if (ifStmtAst.m_HasElse) {
    // manually increasing
    this->m_SymbolId++;
    for (auto &node : ifStmtAst.m_Else) {
      scopeHandler(node, SymbolScope::IfSt);
    }
  }

  exitScope(false);
  return symbolInfo;
}

SymbolInfo Visitor::preVisit(LoopStmtAST &loopStmtAst) {
  SymbolInfo symbolInfo;
  enterScope(false);

  const bool wasInsideLoop = this->m_LastLoop;
  this->m_LastLoop = true;
  auto makeLoopSymbol = [&](SymbolAST *symbolAst, TypeSpecifier type,
                            size_t indirectionLevel = 0) {
    SymbolInfo loopSymbol;
    loopSymbol.symbolName = symbolAst->m_SymbolName;
    loopSymbol.begin = symbolAst->m_SemLoc.begin;
    loopSymbol.end = symbolAst->m_SemLoc.end;
    loopSymbol.line = symbolAst->m_SemLoc.line;
    loopSymbol.type = type;
    loopSymbol.indirectionLevel = indirectionLevel;
    loopSymbol.isPrimitive = IsPrimitiveType(type);
    loopSymbol.isCastable = true;
    loopSymbol.symbolScope = SymbolScope::LoopSt;
    loopSymbol.scopeId = m_ScopeId;
    loopSymbol.scopeLevel = m_ScopeLevel;
    loopSymbol.symbolId = 0;
    if (!m_TypeChecking) {
      this->m_SymbolInfos.push_back(loopSymbol);
      this->m_LastScopeSymbols.emplace_back(loopSymbol.symbolName, loopSymbol);
    }
  };
  auto validateSymbolInCurrentScope = [&](std::string &name,
                                          SymbolInfo &lookupInfo,
                                          SymbolInfo &matchedSymbol,
                                          bool noError = false) {
    const auto previousLastSymbolInfo = m_LastSymbolInfo;
    m_LastSymbolInfo.symbolId = m_SymbolId;
    m_LastSymbolInfo.scopeId = m_ScopeId;
    m_LastSymbolInfo.scopeLevel = m_ScopeLevel;
    const bool found =
        symbolValidation(name, lookupInfo, matchedSymbol, noError);
    m_LastSymbolInfo = previousLastSymbolInfo;
    return found;
  };
  auto findKnownSymbol = [&](const std::string &name,
                             SymbolInfo &matchedSymbol) {
    SymbolInfo lookupInfo;
    auto lookupName = name;
    if (validateSymbolInCurrentScope(lookupName, lookupInfo, matchedSymbol,
                                     true)) {
      return true;
    }

    if (m_TypeChecking) {
      return false;
    }

    for (auto it = m_SymbolInfos.rbegin(); it != m_SymbolInfos.rend(); ++it) {
      if (it->symbolName == name) {
        matchedSymbol = *it;
        return true;
      }
    }

    for (auto &entry : GlobalSymbolTable) {
      if (entry.symbolName == name) {
        matchedSymbol = entry.symbolInfo;
        return true;
      }
    }

    return false;
  };

  if (m_TypeChecking) {
    if (loopStmtAst.m_RangeLoop) {
      if (loopStmtAst.m_Indexable) {
        auto *indexSymbol =
            static_cast<SymbolAST *>(loopStmtAst.m_IndexSymbol.get());
        makeLoopSymbol(indexSymbol, TypeSpecifier::SPEC_I64);
      }

      bool iterSymbolNotExist = false;
      if (loopStmtAst.m_HasNumericRange) {
        auto *dataSymbol =
            static_cast<SymbolAST *>(loopStmtAst.m_DataSymbol.get());
        makeLoopSymbol(dataSymbol, TypeSpecifier::SPEC_I64);

        // To checking symbols are valid and exists..
        // if they are symbols...
        this->m_LastCondExpr = true;
        loopStmtAst.m_Min->acceptBefore(*this);
        loopStmtAst.m_Max->acceptBefore(*this);
        this->m_LastCondExpr = false;
      } else {
        // TODO: Need to extra check for iterable data.
        this->m_LastLoopIter = true;
        if (loopStmtAst.m_IterSymbol->m_ExprKind == SymbolExpr) {
          // loopStmtAst.m_IterSymbol->acceptBefore(*this);
          this->m_LastLoopIter = false;
          auto iterSymbol = (SymbolAST *)loopStmtAst.m_IterSymbol.get();

          symbolInfo.begin = iterSymbol->m_SemLoc.begin;
          symbolInfo.end = iterSymbol->m_SemLoc.end;
          symbolInfo.line = iterSymbol->m_SemLoc.line;

          SymbolInfo matchedSymbol;
          auto iterName = iterSymbol->m_SymbolName;
          if (validateSymbolInCurrentScope(iterName, symbolInfo,
                                           matchedSymbol)) {
            if (!matchedSymbol.isSubscriptable) {
              symbolInfo.begin = iterSymbol->m_SemLoc.begin;
              symbolInfo.end = iterSymbol->m_SemLoc.end;
              symbolInfo.line = iterSymbol->m_SemLoc.line;
              this->m_TypeErrorMessages.emplace_back(
                  "Symbol must be iterable or provided a sequenceable "
                  "trait "
                  "(built-in trait)",
                  symbolInfo);
            }

            auto *dataSymbol =
                static_cast<SymbolAST *>(loopStmtAst.m_DataSymbol.get());
            makeLoopSymbol(dataSymbol, matchedSymbol.type);
          } else {
            iterSymbolNotExist = true;
            this->m_TypeErrorMessages.emplace_back(
                "Iterable symbol is not valid", symbolInfo);
          }
        } else {
          this->m_TypeErrorMessages.emplace_back(
              "Iterable version of loop needs an symbol which is also "
              "iterable",
              symbolInfo);
        }
      }

      (void)iterSymbolNotExist;
    } else {
      this->m_LastCondExpr = true;
      loopStmtAst.m_Cond->acceptBefore(*this);
      this->m_LastCondExpr = false;
    }
  } else if (loopStmtAst.m_RangeLoop) {
    if (loopStmtAst.m_Indexable) {
      auto *indexSymbol =
          static_cast<SymbolAST *>(loopStmtAst.m_IndexSymbol.get());
      makeLoopSymbol(indexSymbol, TypeSpecifier::SPEC_I64);
    }

    if (loopStmtAst.m_HasNumericRange) {
      auto *dataSymbol =
          static_cast<SymbolAST *>(loopStmtAst.m_DataSymbol.get());
      makeLoopSymbol(dataSymbol, TypeSpecifier::SPEC_I64);
      } else if (loopStmtAst.m_IterSymbol != nullptr &&
               loopStmtAst.m_IterSymbol->m_ExprKind == SymbolExpr) {
      auto *iterSymbol = static_cast<SymbolAST *>(loopStmtAst.m_IterSymbol.get());
      auto iterName = iterSymbol->m_SymbolName;
      SymbolInfo matchedSymbol;
      if (findKnownSymbol(iterName, matchedSymbol)) {
        auto *dataSymbol =
            static_cast<SymbolAST *>(loopStmtAst.m_DataSymbol.get());
        makeLoopSymbol(dataSymbol, matchedSymbol.type);
      }
    }
  }

  for (auto &node : loopStmtAst.m_Scope) {
    scopeHandler(node, SymbolScope::LoopSt);
  }

  this->m_LastLoop = wasInsideLoop;
  exitScope(false);

  return symbolInfo;
}

SymbolInfo Visitor::preVisit(OptionStmtAST &optionStmtAst) {
  SymbolInfo symbolInfo;
  symbolInfo.begin = optionStmtAst.m_SemLoc.begin;
  symbolInfo.end = optionStmtAst.m_SemLoc.end;
  symbolInfo.line = optionStmtAst.m_SemLoc.line;

  SymbolInfo matchedValue;
  if (m_TypeChecking) {
    this->m_LastCondExpr = true;
    matchedValue = optionStmtAst.m_Value->acceptBefore(*this);
    this->m_LastCondExpr = false;
    const bool valueIsEnum =
        matchedValue.type == TypeSpecifier::SPEC_DEFINED &&
        EnumTable.count(matchedValue.definedTypeName) != 0 &&
        matchedValue.indirectionLevel == 0;
    if (!valueIsEnum) {
      this->m_TypeErrorMessages.emplace_back(
          "option currently requires a scalar enum value", symbolInfo);
    }
  } else {
    optionStmtAst.m_Value->acceptBefore(*this);
  }

  std::set<std::string> coveredMembers;
  bool hasDefault = false;
  const EnumInfo *enumInfo = nullptr;
  if (!matchedValue.definedTypeName.empty() &&
      EnumTable.count(matchedValue.definedTypeName) != 0) {
    enumInfo = &EnumTable[matchedValue.definedTypeName];
  }

  for (auto &optionCase : optionStmtAst.m_Cases) {
    if (m_TypeChecking) {
      if (optionCase.isDefault) {
        if (hasDefault) {
          SymbolInfo caseInfo;
          caseInfo.begin = optionCase.loc.begin;
          caseInfo.end = optionCase.loc.end;
          caseInfo.line = optionCase.loc.line;
          this->m_TypeErrorMessages.emplace_back(
              "option can only declare one default '_' branch", caseInfo);
        }
        hasDefault = true;
      } else {
        auto patternInfo = optionCase.pattern->acceptBefore(*this);
        if (enumInfo != nullptr &&
            patternInfo.definedTypeName != enumInfo->name) {
          this->m_TypeErrorMessages.emplace_back(
              "option pattern enum type must match the option value enum",
              patternInfo);
        }
        if (patternInfo.type == TypeSpecifier::SPEC_DEFINED &&
            !patternInfo.definedTypeName.empty()) {
          const auto dot = patternInfo.symbolName.find('.');
          const auto memberName =
              dot == std::string::npos ? patternInfo.symbolName
                                       : patternInfo.symbolName.substr(dot + 1);
          if (coveredMembers.count(memberName) != 0) {
            this->m_TypeErrorMessages.emplace_back(
                "duplicate option branch for enum member '" + memberName + "'",
                patternInfo);
          }
          coveredMembers.insert(memberName);
        }
      }
    }

    enterScope(false);
    this->m_SymbolId++;
    for (auto &node : optionCase.scope) {
      scopeHandler(node, SymbolScope::IfSt);
    }
    exitScope(false);
  }

  if (m_TypeChecking && enumInfo != nullptr && !hasDefault) {
    for (const auto &member : enumInfo->members) {
      if (coveredMembers.count(member.name) == 0) {
        this->m_TypeErrorMessages.emplace_back(
            "non-exhaustive option over enum '" + enumInfo->name +
                "': missing member '" + member.name + "'",
            symbolInfo);
      }
    }
  }

  return symbolInfo;
}

SymbolInfo Visitor::preVisit(BreakStmtAST &breakStmtAst) {
  SymbolInfo symbolInfo;
  symbolInfo.begin = breakStmtAst.m_SemLoc.begin;
  symbolInfo.end = breakStmtAst.m_SemLoc.end;
  symbolInfo.line = breakStmtAst.m_SemLoc.line;

  if (m_TypeChecking && !m_LastLoop) {
    this->m_TypeErrorMessages.emplace_back(
        "`break` can only be used inside a loop", symbolInfo);
  }

  return symbolInfo;
}

SymbolInfo Visitor::preVisit(ContinueStmtAST &continueStmtAst) {
  SymbolInfo symbolInfo;
  symbolInfo.begin = continueStmtAst.m_SemLoc.begin;
  symbolInfo.end = continueStmtAst.m_SemLoc.end;
  symbolInfo.line = continueStmtAst.m_SemLoc.line;

  if (m_TypeChecking && !m_LastLoop) {
    this->m_TypeErrorMessages.emplace_back(
        "`continue` can only be used inside a loop", symbolInfo);
  }

  return symbolInfo;
}
