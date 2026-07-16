#include <visitor/semantic/semantic_private.hpp>

SymbolInfo Visitor::preVisit(SymbolAST &symbolAst) {
  auto symbolName = symbolAst.m_SymbolName;

  SymbolInfo symbolInfo;

  symbolInfo.symbolName = symbolName;
  symbolInfo.begin = symbolAst.m_SemLoc.begin;
  symbolInfo.end = symbolAst.m_SemLoc.end;
  symbolInfo.line = symbolAst.m_SemLoc.line;

  if (this->m_LastCondExpr || this->m_LastLoopDataSymbol ||
      this->m_LastLoopIndexSymbol) {
    this->m_LastSymbolInfo.symbolId = m_SymbolId;
    m_SymbolId += 1;
    this->m_LastSymbolInfo.scopeId = m_ScopeId;
    this->m_LastSymbolInfo.scopeLevel = m_ScopeLevel;
  }

  SymbolInfo matchedSymbol;
  if (this->m_TypeChecking) {
    if (!this->m_LastLoopDataSymbol && !this->m_LastLoopIndexSymbol &&
        !this->m_LastParamSymbol) {
      if (symbolValidation(symbolName, symbolInfo, matchedSymbol)) {
        this->m_MatchedSymbolType = matchedSymbol.type;
        if (matchedSymbol.indirectionLevel > 0 &&
            m_MovedUniqueSymbols.count(SymbolStateKey(matchedSymbol)) > 0) {
          this->m_TypeErrorMessages.emplace_back(
              "pointer '" + matchedSymbol.symbolName +
                  "' was moved and cannot be used before being reinitialized",
              symbolInfo, DiagnosticCode::SemanticOwnership);
        }

        if (!this->m_LastCondExpr && !this->m_LastFixExpr) {
          if (!matchedSymbol.isCastable &&
              matchedSymbol.type != this->m_LastSymbolInfo.type) {
            if (!matchedSymbol.isCastable) {
              this->m_TypeErrorMessages.emplace_back(
                  "Casting is not allowed for the symbol '" +
                      matchedSymbol.symbolName + "'",
                  symbolInfo);
            } else {
              this->m_TypeErrorMessages.emplace_back(
                  "Casting is not possible for the symbol '" +
                      matchedSymbol.symbolName + "'",
                  symbolInfo);
            }
          } else {
            if (matchedSymbol.type == this->m_LastSymbolInfo.type) {
              if (matchedSymbol.type == TypeSpecifier::SPEC_DEFINED &&
                  matchedSymbol.definedTypeName !=
                      this->m_LastSymbolInfo.definedTypeName) {
                this->m_TypeErrorMessages.emplace_back(
                    "Defined type mismatch. Expected '" +
                        this->m_LastSymbolInfo.definedTypeName +
                        "' but found '" + matchedSymbol.definedTypeName + "'",
                    symbolInfo);
              }

              if (m_LastSubscriptable) {
                if (m_LastSubscriptable == matchedSymbol.isSubscriptable) {
                  // all arrays has indirection level since nature of being
                  // array.
                  matchedSymbol.indirectionLevel =
                      matchedSymbol.arrayDimensions.size();
                  // already checking in BinaryOpAst visitor..
                  //              int f =
                  //              static_cast<int>(matchedSymbol.arrayDimensions.size());
                  //              int s =
                  //              static_cast<int>(m_LastArrayIndexCount); if(f
                  //              - s < 0) {
                  //              }
                  if (m_LastSymbolInfo.indirectionLevel !=
                          (matchedSymbol.indirectionLevel -
                           m_LastArrayIndexCount -
                           (this->m_LastDereferenced ? this->m_DereferenceLevel
                                                     : 0) +
                           (m_LastReferenced ? 1 : 0)) &&
                      m_LastBinOp) {
                    symbolInfo.typeCheckerInfo.isCompatibleSubs = false;
                  } else if (m_LastSymbolInfo.indirectionLevel !=
                                 (matchedSymbol.indirectionLevel -
                                  m_LastArrayIndexCount +
                                  (m_LastReferenced ? 1 : 0)) &&
                             !m_LastBinOp) {
                    accumulateIncompatiblePtrErrMesg(symbolInfo);
                  }
                } else {
                  this->m_TypeErrorMessages.emplace_back(
                      "Incompatible array index(es)", symbolInfo);
                  accumulateIncompatiblePtrErrMesg(symbolInfo);
                }
              }

              if ((this->m_LastBinOp && matchedSymbol.indirectionLevel > 0 &&
                   (matchedSymbol.indirectionLevel - m_LastArrayIndexCount -
                    (this->m_LastDereferenced ? this->m_DereferenceLevel : 0) +
                    (m_LastReferenced ? 1 : 0)) ==
                       this->m_LastSymbolInfo.indirectionLevel)) {
                this->m_LastBinOpHasAtLeastOnePtr = true;
              }
              if (matchedSymbol.indirectionLevel == 0 &&
                  m_LastSymbolInfo.indirectionLevel == 1 &&
                  m_LastSymbolInfo.isRef && m_LastReferenced &&
                  !this->m_LastBinOp) {
              } else {
                if (matchedSymbol.indirectionLevel +
                            (this->m_LastReferenced ? 1 : 0) -
                            (this->m_LastDereferenced ? this->m_DereferenceLevel
                                                      : 0) !=
                        this->m_LastSymbolInfo.indirectionLevel &&
                    this->m_LastBinOp) {
                  symbolInfo.typeCheckerInfo.isCompatiblePtr = false;
                } else if (matchedSymbol.indirectionLevel +
                                   (this->m_LastReferenced ? 1 : 0) -
                                   (this->m_LastDereferenced
                                        ? this->m_DereferenceLevel
                                        : 0) !=
                               this->m_LastSymbolInfo.indirectionLevel &&
                           !this->m_LastBinOp) {
                  accumulateIncompatiblePtrErrMesg(symbolInfo);
                }

                if ((this->m_LastBinOp && matchedSymbol.indirectionLevel > 0 &&
                     matchedSymbol.indirectionLevel +
                             (this->m_LastReferenced ? 1 : 0) -
                             (this->m_LastDereferenced
                                  ? this->m_DereferenceLevel
                                  : 0) ==
                         this->m_LastSymbolInfo.indirectionLevel)) {
                  this->m_LastBinOpHasAtLeastOnePtr = true;
                }
              }

            } else {
              if (IsPrimitiveType(this->m_ExpectedType) &&
                  IsPrimitiveType(this->m_MatchedSymbolType) &&
                  !this->m_LastReferenced) {
                // Plain type without ptr-level
                if (LosslessCasting(this->m_ExpectedType,
                                    this->m_MatchedSymbolType) &&
                    this->m_LastSymbolInfo.indirectionLevel == 0) {
                  this->m_TypeWarningMessages.emplace_back(
                      "A '" + GetTypeStr(this->m_MatchedSymbolType) +
                          "' type is casting to '" +
                          GetTypeStr(this->m_ExpectedType) +
                          "'. Potential data loss might be occured!",
                      symbolInfo);
                }
              } else {
                accumulateIncompatiblePtrErrMesg(symbolInfo);
              }
            }

            // type qualifier of the variable decl
            const bool readingConstValueThroughPointer =
                matchedSymbol.isConstVal && m_LastDereferenced;
            if ((matchedSymbol.isConstVal && !m_LastSymbolInfo.isConstVal) &&
                matchedSymbol.indirectionLevel != 0 &&
                !readingConstValueThroughPointer) {
              if (this->m_LastRetExpr) {
                this->m_TypeErrorMessages.emplace_back(
                    "Qualifier mismatch: cannot return value with "
                    "incompatible qualifier",
                    symbolInfo, DiagnosticCode::SemanticQualifierMismatch);
              } else {
                this->m_TypeErrorMessages.emplace_back(
                    "Qualifier mismatch: cannot initialize value with "
                    "incompatible qualifier",
                    symbolInfo, DiagnosticCode::SemanticQualifierMismatch);
              }
            }

            const bool bindingToConstRef =
                m_LastSymbolInfo.isConstRef && m_LastSymbolInfo.isRef;
            const bool readingConstRefAsValue =
                matchedSymbol.isConstRef && !m_LastSymbolInfo.isRef &&
                m_LastSymbolInfo.indirectionLevel == 0;
            if (matchedSymbol.isConstRef != m_LastSymbolInfo.isConstRef &&
                !bindingToConstRef && !readingConstRefAsValue) {
              if (this->m_LastRetExpr) {
                this->m_TypeErrorMessages.emplace_back(
                    "Qualifier mismatch: cannot return value with "
                    "incompatible qualifier",
                    symbolInfo, DiagnosticCode::SemanticQualifierMismatch);
              } else {
                this->m_TypeErrorMessages.emplace_back(
                    "Qualifier mismatch: cannot initialize value with "
                    "incompatible qualifier",
                    symbolInfo, DiagnosticCode::SemanticQualifierMismatch);
              }
            }

            const bool bindingToConstPtr =
                m_LastSymbolInfo.isConstPtr &&
                m_LastSymbolInfo.indirectionLevel > 0 && m_LastReferenced;
            const bool readingThroughConstPtr =
                matchedSymbol.isConstPtr && m_LastDereferenced;
            if (matchedSymbol.isConstPtr != m_LastSymbolInfo.isConstPtr &&
                !bindingToConstPtr && !readingThroughConstPtr &&
                !matchedSymbol.isConstRef && !matchedSymbol.isConstVal) {
              if (this->m_LastRetExpr) {
                this->m_TypeErrorMessages.emplace_back(
                    "Qualifier mismatch: cannot return value with "
                    "incompatible qualifier",
                    symbolInfo, DiagnosticCode::SemanticQualifierMismatch);
              } else {
                this->m_TypeErrorMessages.emplace_back(
                    "Qualifier mismatch: cannot initialize value with "
                    "incompatible qualifier",
                    symbolInfo, DiagnosticCode::SemanticQualifierMismatch);
              }
            }
          }
        } else {
          matchedSymbol.indirectionLevel =
              m_LastSymbolInfo.arrayDimensions.size();
          if (m_LastSymbolInfo.isSubscriptable) {
            if (0 !=
                    (matchedSymbol.indirectionLevel - m_LastArrayIndexCount -
                     (this->m_LastDereferenced ? this->m_DereferenceLevel : 0) +
                     (m_LastReferenced ? 1 : 0)) &&
                m_LastBinOp) {
              symbolInfo.typeCheckerInfo.isCompatibleSubsForBinOp = false;
            }
          }
        }
      } else {
        // Symbol could not validate obviously.
        // but it will be processed inside of the symbolValidation func.
      }
    } else {
      if (this->m_LastLoopDataSymbol || this->m_LastLoopIndexSymbol) {
        if (symbolValidation(symbolName, symbolInfo, matchedSymbol,
                             m_LastLoop)) {
          if (m_LastLoopIndexSymbol) {
            this->m_TypeWarningMessages.emplace_back(
                "The index symbol of the loop is shadowing a local or "
                "global variable",
                symbolInfo);
          } else {
            this->m_TypeWarningMessages.emplace_back(
                "The value symbol of the loop is shadowing a local or "
                "global variable",
                symbolInfo);
          }
        }
      }
    }
  } else {
    if (!this->m_LastLoopDataSymbol && !this->m_LastLoopIndexSymbol &&
        !this->m_LastParamSymbol) {
      symbolInfo.ptrAliases[this->m_LastSymbolInfo.indirectionLevel] =
          symbolName;
    }
  }

  if (this->m_TypeChecking && !matchedSymbol.symbolName.empty()) {
    const auto begin = symbolInfo.begin;
    const auto end = symbolInfo.end;
    const auto line = symbolInfo.line;
    const auto typeCheckerInfo = symbolInfo.typeCheckerInfo;
    symbolInfo = matchedSymbol;
    symbolInfo.begin = begin;
    symbolInfo.end = end;
    symbolInfo.line = line;
    symbolInfo.typeCheckerInfo = typeCheckerInfo;
  }

  return symbolInfo;
}
