#include <visitor/semantic/semantic_private.hpp>

bool Visitor::symbolValidation(std::string &symbolName, SymbolInfo &symbolInfo,
                               SymbolInfo &matchedSymbol, bool noError) {
  bool isLocalSymbol = false;
  bool isGlobSymbol = false;

  size_t index = this->m_LastSymbolInfo.symbolId;
  size_t scopeId = this->m_LastSymbolInfo.scopeId;
  size_t scopeLevel = this->m_LastSymbolInfo.scopeLevel;

  for (auto &it : GlobalSymbolTable) {
    if (it.symbolName == symbolName) {
      if (it.symbolInfo.scopeLevel < scopeLevel) {
        // nothing
      } else {
        if (it.symbolInfo.scopeLevel == scopeLevel) {
          if (it.symbolInfo.symbolId > index ||
              it.symbolInfo.scopeId != scopeId) {
            goto pitfallGlob;
          } else {
            goto doneGlob;
          }
        } else {
          continue;
        }
      pitfallGlob:
        if (!noError) {
          this->m_TypeWarningMessages.emplace_back(
              "'" + symbolName + "' was not in a valid place", it.symbolInfo);
          this->m_TypeErrorMessages.emplace_back(
              "'" + symbolName +
                  "' was declared in the wrong place. Probably it used "
                  "before "
                  "declaration",
              symbolInfo);
        }
        break;
      }
    doneGlob:
      matchedSymbol = it.symbolInfo;
      isGlobSymbol = true;
      break;
    }
  }

  for (auto &it : this->m_LastScopeSymbols) {
    if (it.symbolName == symbolName) {
      if (it.symbolInfo.symbolScope == SymbolScope::LoopSt &&
          it.symbolInfo.scopeLevel <= scopeLevel) {
        goto done;
      }

      if (it.symbolInfo.scopeLevel < scopeLevel) {
        // nothing
      } else {
        if (it.symbolInfo.scopeLevel == scopeLevel) {
          if (scopeLevel == 1) {
            if (it.symbolInfo.symbolId > index) {
              goto pitfall;
            } else {
              goto done;
            }
          } else {
            if (it.symbolInfo.symbolId > index ||
                it.symbolInfo.scopeId != scopeId) {
              goto pitfall;
            } else {
              goto done;
            }
          }
        } else {
          continue;
        }
      pitfall:
        if (!noError) {
          this->m_TypeWarningMessages.emplace_back(
              "'" + symbolName + "' was not in a valid place", it.symbolInfo);
          this->m_TypeErrorMessages.emplace_back(
              "'" + symbolName +
                  "' was declared in the wrong place. Probably it used "
                  "before "
                  "declaration",
              symbolInfo);
        }
        break;
      }
    done:
      matchedSymbol = it.symbolInfo;
      isLocalSymbol = true;
      break;
    }
  }

  if (!isLocalSymbol && !isGlobSymbol && !noError) {
    this->m_TypeErrorMessages.emplace_back(
        "'" + symbolName + "' was not declared in this scope or global scope",
        symbolInfo);
    return false;
  }

  if (noError && !isLocalSymbol && !isGlobSymbol) {
    return false;
  }

  if (isLocalSymbol) {
    if (isGlobSymbol) {
      // variable shadowing
      this->m_TypeWarningMessages.emplace_back(
          "'" + symbolName + "' shadowing global variable", matchedSymbol);
    } else {
    }
  }

  if (m_TypeChecking && m_CurrentFunctionIsStatic && isGlobSymbol &&
      !matchedSymbol.isStatic && !matchedSymbol.isImported) {
    this->m_TypeErrorMessages.emplace_back(
        "static function cannot access non-static global symbol '" +
            symbolName + "'",
        symbolInfo);
  }

  if (m_TypeChecking &&
      m_DroppedSemanticSymbols.count(SymbolStateKey(matchedSymbol)) > 0 &&
      !noError) {
    this->m_TypeErrorMessages.emplace_back(
        "value '" + matchedSymbol.symbolName +
            "' was dropped and cannot be used before being reinitialized",
        symbolInfo, DiagnosticCode::SemanticOwnership);
  }

  return true;
}
