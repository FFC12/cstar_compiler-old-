#include <visitor/semantic/semantic_private.hpp>

SymbolInfo Visitor::preVisit(AssignmentAST &assignmentAst) {
  SymbolInfo symbolInfo;
  const auto previousExpectedType = m_ExpectedType;
  const auto previousLastSymbolInfo = m_LastSymbolInfo;
  const auto previousDefinedTypeFlag = m_DefinedTypeFlag;
  const auto previousLastBinOpHasPtr = m_LastBinOpHasAtLeastOnePtr;
  const auto previousLastAssignment = m_LastAssignment;

  if (m_TypeChecking) {
    if (assignmentAst.m_LHS->m_ASTKind != ASTKind::Expr) {
      assert(false && "Assignment target must be an expression.");
    } else {
      SymbolInfo matchedSymbol;
      SymbolAST *lhs = nullptr;
      bool isStructFieldAssignment = false;
      std::string lhsName;
      this->m_LastAssignment = true;
      const size_t assignmentSymbolId = m_SymbolId;
      const size_t assignmentScopeId = m_ScopeId;
      const size_t assignmentScopeLevel = m_ScopeLevel;
      this->m_LastSymbolInfo.symbolId = assignmentSymbolId;
      this->m_LastSymbolInfo.scopeId = assignmentScopeId;
      this->m_LastSymbolInfo.scopeLevel = assignmentScopeLevel;

      if (assignmentAst.m_LHS->m_ExprKind == ExprKind::SymbolExpr) {
        lhs = static_cast<SymbolAST *>(assignmentAst.m_LHS.get());
        lhsName = lhs->m_SymbolName;
        symbolInfo.symbolName = lhs->m_SymbolName;
        symbolInfo.begin = lhs->m_SemLoc.begin;
        symbolInfo.end = lhs->m_SemLoc.end;
        symbolInfo.line = lhs->m_SemLoc.line;
      } else if (assignmentAst.m_LHS->m_ExprKind == ExprKind::BinOp) {
        auto resolveFieldAccess =
            [&](auto &self, IAST *node) -> SymbolInfo {
          SymbolInfo resolved;
          if (node == nullptr) {
            this->m_TypeErrorMessages.emplace_back(
                "Assignment target must be a symbol or struct field",
                symbolInfo);
            return resolved;
          }

          if (node->m_ExprKind == ExprKind::SymbolExpr) {
            auto *symbol = static_cast<SymbolAST *>(node);
            SymbolInfo lookup;
            lookup.symbolName = symbol->m_SymbolName;
            lookup.begin = symbol->m_SemLoc.begin;
            lookup.end = symbol->m_SemLoc.end;
            lookup.line = symbol->m_SemLoc.line;
            auto symbolName = symbol->m_SymbolName;
            if (!symbolValidation(symbolName, lookup, resolved, true)) {
              if (StructTable.count(symbol->m_SymbolName) != 0) {
                this->m_TypeErrorMessages.emplace_back(
                    "Struct type '" + symbol->m_SymbolName +
                        "' has no instance; use a value before accessing "
                        "fields",
                    lookup);
              } else {
                symbolValidation(symbolName, lookup, resolved);
              }
              return resolved;
            }
            if (resolved.type == TypeSpecifier::SPEC_DEFINED &&
                resolved.indirectionLevel > 0) {
              resolved.indirectionLevel = 0;
              resolved.isUnique = false;
              resolved.isRef = false;
            }
            return resolved;
          }

          if (node->m_ExprKind != ExprKind::BinOp) {
            this->m_TypeErrorMessages.emplace_back(
                "Assignment target must be a symbol or struct field",
                symbolInfo);
            return resolved;
          }

          auto *fieldAccess = static_cast<BinaryOpAST *>(node);
          if (fieldAccess->m_BinOpKind != B_DOT ||
              fieldAccess->m_RHS == nullptr ||
              fieldAccess->m_RHS->m_ExprKind != ExprKind::SymbolExpr) {
            this->m_TypeErrorMessages.emplace_back(
                "Assignment target must be a symbol or struct field",
                symbolInfo);
            return resolved;
          }

          auto base = self(self, fieldAccess->m_LHS.get());
          auto *fieldSymbol = static_cast<SymbolAST *>(fieldAccess->m_RHS.get());
          if (base.symbolName.empty() && base.type == TypeSpecifier::SPEC_VOID) {
            return resolved;
          }
          if (base.type != TypeSpecifier::SPEC_DEFINED) {
            this->m_TypeErrorMessages.emplace_back(
                "Field assignment requires a struct value", base);
            return resolved;
          }

          auto structIt = StructTable.find(base.definedTypeName);
          if (structIt == StructTable.end()) {
            this->m_TypeErrorMessages.emplace_back(
                "Unknown struct type '" + base.definedTypeName + "'", base);
            return resolved;
          }

          auto fieldIt =
              structIt->second.fieldIndexes.find(fieldSymbol->m_SymbolName);
          if (fieldIt == structIt->second.fieldIndexes.end()) {
            this->m_TypeErrorMessages.emplace_back(
                "Struct '" + base.definedTypeName + "' has no field '" +
                    fieldSymbol->m_SymbolName + "'",
                base);
            return resolved;
          }

          const auto &field = structIt->second.fields[fieldIt->second];
          if (structIt->second.isFromIncludedModule && !field.isPublic &&
              m_CurrentStructMethodOwner != base.definedTypeName) {
            SymbolInfo fieldInfo;
            fieldInfo.symbolName = fieldSymbol->m_SymbolName;
            fieldInfo.begin = fieldSymbol->m_SemLoc.begin;
            fieldInfo.end = fieldSymbol->m_SemLoc.end;
            fieldInfo.line = fieldSymbol->m_SemLoc.line;
            this->m_TypeErrorMessages.emplace_back(
                "Field '" + field.name + "' of module struct '" +
                    base.definedTypeName + "' is private",
                fieldInfo);
            return resolved;
          }
          resolved = base;
          resolved.symbolName = base.symbolName + "." + field.name;
          resolved.type = field.type;
          resolved.definedTypeName = field.definedTypeName;
          resolved.indirectionLevel = field.indirectionLevel;
          resolved.isUnique = field.isUnique;
          resolved.isRef = field.isRef;
          resolved.isNullable = field.isNullable;
          resolved.isSubscriptable = false;
          resolved.arrayDimensions.clear();
          resolved.isConstVal = false;
          resolved.isConstPtr = false;
          resolved.isConstRef = false;
          resolved.isReadOnly = false;
          resolved.begin = fieldSymbol->m_SemLoc.begin;
          resolved.end = fieldSymbol->m_SemLoc.end;
          resolved.line = fieldSymbol->m_SemLoc.line;
          return resolved;
        };

        matchedSymbol =
            resolveFieldAccess(resolveFieldAccess, assignmentAst.m_LHS.get());
        lhsName = matchedSymbol.symbolName;
        symbolInfo = matchedSymbol;
        isStructFieldAssignment = true;
      } else {
        this->m_TypeErrorMessages.emplace_back(
            "Assignment target must be a symbol or struct field", symbolInfo);
        return symbolInfo;
      }

      bool indirectable = false;
      if (isStructFieldAssignment ||
          symbolValidation(lhsName, symbolInfo, matchedSymbol)) {
        // TODO: for different scenerarios.

        if (assignmentAst.m_IsDereferenced) {
          int a = static_cast<int>(matchedSymbol.indirectionLevel);
          int b = static_cast<int>(assignmentAst.m_DerefLevel);
          if (a - b < 0) {
            m_TypeErrorMessages.emplace_back(
                "It's not a indirectable or not a valid indirection level",
                symbolInfo);
          } else {
            if (matchedSymbol.isNullable &&
                m_NonNullFlowSymbols.count(matchedSymbol.symbolName) == 0) {
              m_TypeErrorMessages.emplace_back(
                  "Nullable pointer must be proven non-null before "
                  "dereference assignment; guard it with `if (ptr)` first",
                  symbolInfo, DiagnosticCode::SemanticQualifierMismatch);
            }
            matchedSymbol.indirectionLevel -= assignmentAst.m_DerefLevel;
            if (matchedSymbol.indirectionLevel == 0) {
              matchedSymbol.isNullable = false;
            }

            indirectable = true;
          }
        }

        if (assignmentAst.m_Subscriptable) {
          if (matchedSymbol.isRuntimeSizedArray) {
            if (assignmentAst.m_SubscriptIndexes.size() != 1) {
              m_TypeErrorMessages.emplace_back(
                  "Runtime-sized array view `T[]` expects exactly one index",
                  symbolInfo);
            }
          } else if (assignmentAst.m_SubscriptIndexes.size() !=
              matchedSymbol.arrayDimensions.size()) {
            m_TypeErrorMessages.emplace_back(
                "Array type with index(es) is not assignable", symbolInfo);
          } else {
            for (int i = 0; i < matchedSymbol.arrayDimensions.size(); ++i) {
              auto *index = assignmentAst.m_SubscriptIndexes[i].get();
              int64_t indexVal = 0;
              if (!tryGetConstantIntegerLiteral(index, indexVal)) {
                continue;
              }
              if (IsConstantArrayIndexOutOfBounds(
                      indexVal, matchedSymbol.arrayDimensions[i])) {
                SymbolInfo indexInfo;
                indexInfo.begin = index->getSemLoc().begin;
                indexInfo.end = index->getSemLoc().end;
                indexInfo.line = index->getSemLoc().line;
                m_TypeErrorMessages.emplace_back(
                    "Array index '" + std::to_string(indexVal) +
                        "' is out of bounds for dimension " +
                        std::to_string(i) + " with size " +
                        std::to_string(matchedSymbol.arrayDimensions[i]),
                    indexInfo);
              }
            }
          }
        }

        //
        // type qualifier checking..
        //
        if (matchedSymbol.indirectionLevel == 0 &&
            (matchedSymbol.isConstRef || matchedSymbol.isConstVal)) {
          m_TypeErrorMessages.emplace_back(
              "const-qualified value is not assignable", symbolInfo,
              DiagnosticCode::SemanticConstAssignment);
        }

        if (matchedSymbol.isReadOnly) {
          m_TypeErrorMessages.emplace_back(
              "readonly-qualified symbol is neither assignable with a value "
              "nor pointer with a reference",
              symbolInfo, DiagnosticCode::SemanticReadonlyAssignment);
        }

        if (matchedSymbol.indirectionLevel > 0 && matchedSymbol.isConstPtr) {
          m_TypeErrorMessages.emplace_back(
              "constptr-qualified pointer is not assignable", symbolInfo,
              DiagnosticCode::SemanticConstPtrAssignment);
        } else {
          if (matchedSymbol.ptrAliases.count(assignmentAst.m_DerefLevel) > 0) {
            SymbolInfo indirectedSymbol;
            SymbolInfo &currentSymbol = matchedSymbol;
            size_t derefLevel = assignmentAst.m_DerefLevel;
            while (!currentSymbol.symbolName.empty()) {
              //              symbolValidation(currentSymbol.ptrAliases[derefLevel],symbolInfo,
              //              indirectedSymbol);
              if (currentSymbol.ptrAliases.count(derefLevel) > 0) {
                bool x = symbolValidation(currentSymbol.ptrAliases[derefLevel],
                                          symbolInfo, currentSymbol);
                derefLevel = currentSymbol.indirectionLevel;
              } else {
                break;
              }
            }

            if (currentSymbol.indirectionLevel == 0 &&
                (currentSymbol.isConstRef || currentSymbol.isConstVal)) {
              m_TypeErrorMessages.emplace_back(
                  "const-qualified value is not assignable", symbolInfo,
                  DiagnosticCode::SemanticConstAssignment);
            }

            if (currentSymbol.isReadOnly) {
              m_TypeErrorMessages.emplace_back(
                  "readonly-qualified symbol is neither assignable with a "
                  "value "
                  "nor pointer with a reference",
                  symbolInfo, DiagnosticCode::SemanticReadonlyAssignment);
            }

            if (currentSymbol.indirectionLevel > 0 &&
                currentSymbol.isConstPtr) {
              m_TypeErrorMessages.emplace_back(
                  "constptr-qualified pointer is not assignable", symbolInfo,
                  DiagnosticCode::SemanticConstPtrAssignment);
            }
          }
        }

        // ----------------------

        this->m_ExpectedType = matchedSymbol.type;

        this->m_DefinedTypeFlag =
            matchedSymbol.type == TypeSpecifier::SPEC_DEFINED;
        this->m_DefinedTypeName = matchedSymbol.definedTypeName;

        this->m_LastBinOpHasAtLeastOnePtr = false;
        this->m_LastSymbolInfo = matchedSymbol;
        this->m_LastSymbolInfo.symbolId = assignmentSymbolId;
        this->m_LastSymbolInfo.scopeId = assignmentScopeId;
        this->m_LastSymbolInfo.scopeLevel = assignmentScopeLevel;

        auto rhsAsSymbol = dynamic_cast<SymbolAST *>(assignmentAst.m_RHS.get());
        auto rhsAsUnary =
            dynamic_cast<UnaryOpAST *>(assignmentAst.m_RHS.get());
        const bool rhsIsMoveExpr =
            rhsAsUnary != nullptr && rhsAsUnary->m_UnaryOpKind == U_MOVE;

        auto getPointerSource = [&](SymbolInfo &source) -> bool {
          SymbolAST *sourceSymbol = rhsAsSymbol;
          if (rhsIsMoveExpr) {
            sourceSymbol = dynamic_cast<SymbolAST *>(rhsAsUnary->m_Node.get());
          }

          if (sourceSymbol == nullptr) {
            return false;
          }

          SymbolInfo lookup;
          auto sourceName = sourceSymbol->m_SymbolName;
          if (!symbolValidation(sourceName, lookup, source, true)) {
            return false;
          }

          return source.indirectionLevel > 0;
        };

        SymbolInfo uniqueMoveSource;
        bool markUniqueMoveSource = false;
        if (assignmentAst.m_ShortcutOp == ShortcutOp::S_MOV) {
          SymbolInfo source;
          if (matchedSymbol.indirectionLevel == 0 ||
              assignmentAst.m_IsDereferenced || assignmentAst.m_Subscriptable) {
            this->m_TypeErrorMessages.emplace_back(
                "':=' move assignment requires a direct pointer target",
                symbolInfo, DiagnosticCode::SemanticOwnership);
          } else if (!getPointerSource(source)) {
            this->m_TypeErrorMessages.emplace_back(
                "':=' move assignment requires a pointer source",
                symbolInfo, DiagnosticCode::SemanticOwnership);
          } else if (source.isNoMove) {
            this->m_TypeErrorMessages.emplace_back(
                "'nomove' pointer cannot be moved", symbolInfo,
                DiagnosticCode::SemanticOwnership);
          } else if (matchedSymbol.isUnique != source.isUnique) {
            this->m_TypeErrorMessages.emplace_back(
                "':=' move assignment requires matching pointer ownership kind",
                symbolInfo, DiagnosticCode::SemanticOwnership);
          } else {
            uniqueMoveSource = source;
            markUniqueMoveSource = true;
          }
        } else if (matchedSymbol.isUnique && rhsAsSymbol != nullptr) {
          SymbolInfo source;
          if (getPointerSource(source) && source.isUnique) {
            this->m_TypeErrorMessages.emplace_back(
                "unique pointer cannot be copied; use ':=' or 'move' to "
                "transfer ownership",
                symbolInfo, DiagnosticCode::SemanticOwnership);
          }
        }

        //      this->m_LastIde
        //        if(indirectable)
        auto rhs = assignmentAst.m_RHS->acceptBefore(*this);
        if (rhs.isNullable && !matchedSymbol.isNullable &&
            rhs.indirectionLevel > 0 &&
            m_NonNullFlowSymbols.count(rhs.symbolName) == 0) {
          this->m_TypeErrorMessages.emplace_back(
              "Nullable pointer cannot be assigned to a non-null pointer "
              "without an `if (ptr)` non-null proof",
              symbolInfo, DiagnosticCode::SemanticQualifierMismatch);
        }
        if (markUniqueMoveSource) {
          m_MovedUniqueSymbols.insert(SymbolStateKey(uniqueMoveSource));
          m_MovedUniqueSymbols.erase(SymbolStateKey(matchedSymbol));
        } else if (matchedSymbol.indirectionLevel > 0 &&
                   !assignmentAst.m_IsDereferenced &&
                   !assignmentAst.m_Subscriptable) {
          m_MovedUniqueSymbols.erase(SymbolStateKey(matchedSymbol));
        }

        // rhs symbol need to check

        this->m_LastReferenced = false;
        this->m_DereferenceLevel = 1;
      }

      this->m_LastAssignment = previousLastAssignment;
    }
  }

  this->m_ExpectedType = previousExpectedType;
  this->m_LastSymbolInfo = previousLastSymbolInfo;
  this->m_DefinedTypeFlag = previousDefinedTypeFlag;
  this->m_LastBinOpHasAtLeastOnePtr = previousLastBinOpHasPtr;
  this->m_LastAssignment = previousLastAssignment;

  return symbolInfo;
}
