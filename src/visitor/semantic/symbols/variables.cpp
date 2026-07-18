#include <visitor/semantic/semantic_private.hpp>

SymbolInfo Visitor::preVisit(VarAST &varAst) {
  SymbolInfo symbolInfo;
  const auto previousExpectedType = m_ExpectedType;
  const auto previousLastSymbolInfo = m_LastSymbolInfo;
  const auto previousDefinedTypeFlag = m_DefinedTypeFlag;
  const auto previousDefinedTypeName = m_DefinedTypeName;
  const auto previousLastBinOpHasPtr = m_LastBinOpHasAtLeastOnePtr;
  const auto previousLastVarDecl = m_LastVarDecl;

  symbolInfo.symbolName = varAst.m_Name;
  symbolInfo.begin = varAst.m_SemLoc.begin;
  symbolInfo.end = varAst.m_SemLoc.end;
  symbolInfo.line = varAst.m_SemLoc.line;

  this->m_LastVarDecl = true;
  // symbolInfo.symbolId = Visitor::SymbolId;
  // Visitor::SymbolIdList[symbolInfo.symbolName] = symbolInfo.symbolId;

  if (varAst.m_IsLocal) {
    // will be evualated later
    // symbolInfo = varAst.m_RHS->acceptBefore(*this);
    symbolInfo.isGlob = false;

    symbolInfo.symbolId = this->m_SymbolId;
    symbolInfo.scopeId = this->m_ScopeId;
    symbolInfo.scopeLevel = this->m_ScopeLevel;
    this->m_SymbolId += 1;
  } else {
    symbolInfo.isGlob = true;
    symbolInfo.scopeLevel = 0;
    symbolInfo.scopeId = 0;

    symbolInfo.symbolId = this->m_SymbolId;
    symbolInfo.scopeId = this->m_ScopeId;
    symbolInfo.scopeLevel = this->m_ScopeLevel;
  }

  symbolInfo.type = varAst.m_TypeSpec;
  if (varAst.m_TypeSpec == TypeSpecifier::SPEC_DEFINED &&
      varAst.m_Typename != nullptr &&
      varAst.m_Typename->m_ExprKind == ExprKind::SymbolExpr) {
    auto *typeSymbol = static_cast<SymbolAST *>(varAst.m_Typename.get());
    symbolInfo.definedTypeName = typeSymbol->m_SymbolName;
  }
  symbolInfo.isSubscriptable = varAst.m_IsInitializerList;
  symbolInfo.indirectionLevel = varAst.m_IndirectLevel;
  symbolInfo.isConstRef = varAst.m_TypeQualifier == Q_CONSTREF;
  symbolInfo.isConstPtr = varAst.m_TypeQualifier == Q_CONSTPTR;
  symbolInfo.isReadOnly = varAst.m_TypeQualifier == Q_READONLY;
  symbolInfo.isConstVal = varAst.m_TypeQualifier == Q_CONST;
  symbolInfo.isRef = varAst.m_IsRef;
  symbolInfo.isUnique = varAst.m_IsUniquePtr;
  symbolInfo.isNullable = varAst.m_IsNullable;
  symbolInfo.isDynamicTraitObject = varAst.m_IsDynamicTraitObject;
  if (!varAst.m_StateQualifiers.empty()) {
    for (const auto& state : varAst.m_StateQualifiers) {
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
        this->m_TypeErrorMessages.emplace_back(
            "Unknown protocol state qualifier '" + state + "' for type '" +
                symbolInfo.definedTypeName + "'",
            symbolInfo);
      }
    }
  }
  symbolInfo.isCastable = true;
  symbolInfo.isPublic = varAst.m_AccessSpec == ACCESS_PUBLIC;
  symbolInfo.isStatic = varAst.m_IsStatic ||
                        varAst.m_VisibilitySpec == VisibilitySpecifier::VIS_STATIC;
  symbolInfo.isExported =
      varAst.m_VisibilitySpec == VisibilitySpecifier::VIS_EXPORT;
  symbolInfo.isImported =
      varAst.m_VisibilitySpec == VisibilitySpecifier::VIS_IMPORT;
  symbolInfo.qualifierLevels =
      BuildQualifierLevels(varAst.m_TypeQualifier, symbolInfo.indirectionLevel,
                           symbolInfo.isRef);

  if (symbolInfo.indirectionLevel > 1 &&
      (varAst.m_TypeQualifier == TypeQualifier::Q_CONST ||
       varAst.m_TypeQualifier == TypeQualifier::Q_CONSTPTR ||
       varAst.m_TypeQualifier == TypeQualifier::Q_CONSTREF)) {
    this->m_TypeErrorMessages.emplace_back(
        "Multi-level pointer qualifiers require explicit per-level qualifier "
        "syntax; this declaration would be ambiguous in the current MVP",
        symbolInfo, DiagnosticCode::SemanticQualifierMismatch);
  }

  if (m_TypeChecking && symbolInfo.isNullable &&
      (symbolInfo.indirectionLevel == 0 || symbolInfo.isRef)) {
    this->m_TypeErrorMessages.emplace_back(
        "Nullable marker '?' is valid only for pointer types (`T*?` or `T^?`)",
        symbolInfo, DiagnosticCode::SemanticInvalidQualifier);
  }

  if (m_TypeChecking && symbolInfo.isDynamicTraitObject) {
    if (TraitTable.count(symbolInfo.definedTypeName) == 0) {
      this->m_TypeErrorMessages.emplace_back(
          "dynamic trait object target '" + symbolInfo.definedTypeName +
              "' must name a trait",
          symbolInfo);
    } else if (!symbolInfo.isRef && !symbolInfo.isUnique) {
      this->m_TypeErrorMessages.emplace_back(
          "shared dynamic trait object lowering is not implemented yet; use "
          "`dynamic Trait&` or `dynamic Trait^`",
          symbolInfo, DiagnosticCode::SemanticOwnership);
    }
  }

  symbolInfo.isNeededEval = true;

  if (!varAst.m_RHS) {
    // Well,the symbol it's not initialized.
    symbolInfo.isNeededEval = false;
  }

  if (varAst.m_TypeSpec == TypeSpecifier::SPEC_DEFINED &&
      varAst.m_Typename != nullptr &&
      varAst.m_Typename->m_ExprKind == ExprKind::SymbolExpr) {
    auto typeName =
        dynamic_cast<SymbolAST *>(varAst.m_Typename.get())->m_SymbolName;
    this->m_DefinedTypeName = typeName;
    this->m_DefinedTypeFlag = true;
    if (m_TypeChecking && !symbolInfo.isDynamicTraitObject &&
        this->m_TypeTable.count(typeName) == 0) {
      this->m_TypeErrorMessages.emplace_back(
          "Unknown type '" + typeName + "'", symbolInfo);
    }
  }

  if (symbolInfo.isSubscriptable) {
    for (auto &v : varAst.m_ArrDim) {
      uint64_t dimension = 0;
      if (!TryGetNonNegativeIntegerLiteral(v.get(), dimension)) {
        this->m_TypeErrorMessages.emplace_back(
            "Array dimensions must be compile-time positive integer literals "
            "in the current MVP",
            symbolInfo);
        continue;
      }

      if (dimension == 0) {
        this->m_TypeErrorMessages.emplace_back(
            "Array dimensions must be greater than zero", symbolInfo);
        continue;
      }

      if (dimension > std::numeric_limits<size_t>::max()) {
        this->m_TypeErrorMessages.emplace_back(
            "Array dimension is too large for this target", symbolInfo);
        continue;
      }

      symbolInfo.arrayDimensions.push_back(static_cast<size_t>(dimension));
    }
    m_LastArrayDims = symbolInfo.arrayDimensions;

    const bool allDimensionsValid =
        symbolInfo.arrayDimensions.size() == varAst.m_ArrDim.size();
    const auto elementCount =
        allDimensionsValid ? ProductOfDimensions(symbolInfo.arrayDimensions) : 0;
    if (allDimensionsValid && elementCount == 0) {
      this->m_TypeErrorMessages.emplace_back(
          "Array dimensions overflow the addressable element count",
          symbolInfo);
    } else if (allDimensionsValid && varAst.m_IsLocal) {
      const uint64_t elementBytes =
          symbolInfo.indirectionLevel > 0
              ? static_cast<uint64_t>(sizeof(void *))
              : PrimitiveTypeStorageBytes(symbolInfo.type);
      if (elementBytes > 0 &&
          elementCount >
              std::numeric_limits<uint64_t>::max() / elementBytes) {
        this->m_TypeErrorMessages.emplace_back(
            "Array storage size overflows the addressable byte count",
            symbolInfo);
      } else if (elementBytes > 0 &&
                 elementCount * elementBytes > kMaxLocalStackArrayBytes) {
        this->m_TypeErrorMessages.emplace_back(
            "Local array requires " +
                std::to_string(elementCount * elementBytes) +
                " bytes of stack storage; use heap/allocator-backed storage "
                "for large buffers",
            symbolInfo);
      }
    }
  }

  // Do not need to look up the symbol table
  // for the declared name. since we can directly
  // take the information from decl.
  if (this->m_TypeChecking && symbolInfo.isNeededEval) {
    this->m_ExpectedType = varAst.m_TypeSpec;

    // Array intializer
    if (symbolInfo.isSubscriptable) {
      bool constant = varAst.m_ArrDim.size() == 1 &&
                      (varAst.m_RHS->m_ExprKind == ExprKind::ScalarExpr ||
                       varAst.m_RHS->m_ExprKind == ExprKind::SymbolExpr);

      if (varAst.m_RHS->m_ExprKind != ExprKind::BinOp &&
          varAst.m_RHS->m_ASTKind == ASTKind::Expr && !constant) {
        this->m_TypeErrorMessages.emplace_back(
            "Array initializer must be an initilizer list", symbolInfo);
      } else {
        if (constant) {
          // `(value)` is scalar-fill syntax for fixed-size arrays.
          // It initializes every element to the same scalar/symbol value.
        } else {
          std::vector<BinOpOrVal> vec;
          getElementsOfArray(*varAst.m_RHS, vec);
          const auto expectedElements =
              ProductOfDimensions(symbolInfo.arrayDimensions);
          if (expectedElements != 0 && vec.size() != expectedElements) {
            this->m_TypeErrorMessages.emplace_back(
                "Array initializer has " + std::to_string(vec.size()) +
                    " element(s), but array storage requires " +
                    std::to_string(expectedElements),
                symbolInfo);
          }
        }
        m_LastArrayDims.clear();
      }
    }
    // --

    if (((symbolInfo.isConstPtr && !symbolInfo.isSubscriptable) &&
         (symbolInfo.indirectionLevel == 0 || symbolInfo.isRef)) ||
        (symbolInfo.isConstRef && !symbolInfo.isRef)) {
      // error
      this->m_TypeErrorMessages.emplace_back(
          "Invalid qualifier/type combination",
          symbolInfo, DiagnosticCode::SemanticInvalidQualifier);
    }

    // reset all state flags.
    this->m_LastBinOpHasAtLeastOnePtr = false;

    this->m_LastSymbolInfo = symbolInfo;
    auto rhsAsSymbol = dynamic_cast<SymbolAST *>(varAst.m_RHS.get());
    auto rhsAsUnary = dynamic_cast<UnaryOpAST *>(varAst.m_RHS.get());
    auto rhsAsCast = dynamic_cast<CastOpAST *>(varAst.m_RHS.get());
    const bool rhsIsMoveExpr =
        rhsAsUnary != nullptr && rhsAsUnary->m_UnaryOpKind == U_MOVE;
    std::string constructorName;
    const bool isConstructorInitializer =
        constructorInitializer(varAst, constructorName);

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
    auto dynamicMoveSource = [&]() -> SymbolAST * {
      if (rhsAsCast == nullptr ||
          rhsAsCast->m_CastOpKind != CastOpKind::C_DYNAMIC_MOVE_AS ||
          rhsAsCast->m_Node == nullptr ||
          rhsAsCast->m_Node->m_ExprKind != ExprKind::SymbolExpr) {
        return nullptr;
      }
      return static_cast<SymbolAST *>(rhsAsCast->m_Node.get());
    };

    if (varAst.m_IsMoveInit) {
      SymbolInfo source;
      if (symbolInfo.indirectionLevel == 0) {
        this->m_TypeErrorMessages.emplace_back(
            "':=' move initializer requires a pointer target",
            symbolInfo, DiagnosticCode::SemanticOwnership);
      } else if (!getPointerSource(source)) {
        this->m_TypeErrorMessages.emplace_back(
            "':=' move initializer requires a pointer source",
            symbolInfo, DiagnosticCode::SemanticOwnership);
      } else if (source.isNoMove) {
        this->m_TypeErrorMessages.emplace_back(
            "'nomove' pointer cannot be moved", symbolInfo,
            DiagnosticCode::SemanticOwnership);
      } else if (symbolInfo.isUnique != source.isUnique) {
        this->m_TypeErrorMessages.emplace_back(
            "':=' move initializer requires matching pointer ownership kind",
            symbolInfo, DiagnosticCode::SemanticOwnership);
      } else {
        uniqueMoveSource = source;
        markUniqueMoveSource = true;
      }
    } else if (symbolInfo.isUnique && rhsAsSymbol != nullptr) {
      SymbolInfo source;
      if (getPointerSource(source) && source.isUnique) {
        this->m_TypeErrorMessages.emplace_back(
            "unique pointer cannot be copied; use ':=' or 'move' to transfer "
            "ownership",
            symbolInfo, DiagnosticCode::SemanticOwnership);
      }
    }

    if (isConstructorInitializer) {
      if (!varAst.m_IsLocal) {
        this->m_TypeErrorMessages.emplace_back(
            "Constructor initializer is only supported for local struct "
            "variables",
            symbolInfo);
      }

      auto signatureIt = FunctionTable.find(constructorName);
      if (signatureIt == FunctionTable.end()) {
        this->m_TypeErrorMessages.emplace_back(
            "Struct '" + symbolInfo.definedTypeName +
                "' does not declare a constructor",
            symbolInfo);
      } else {
        auto *call = static_cast<FuncCallAST *>(varAst.m_RHS.get());
        std::vector<IAST *> argNodes;
        auto collectArgs = [&](auto &self, IAST *node) -> void {
          if (node == nullptr) {
            return;
          }

          if (node->m_ExprKind == ExprKind::BinOp) {
            auto *binOp = static_cast<BinaryOpAST *>(node);
            if (binOp->m_BinOpKind == BinOpKind::B_COMM) {
              self(self, binOp->m_LHS.get());
              self(self, binOp->m_RHS.get());
              return;
            }
          }

          argNodes.push_back(node);
        };
        collectArgs(collectArgs, call->m_Args.get());

        const auto expectedArgs =
            signatureIt->second.params.empty()
                ? 0
                : signatureIt->second.params.size() - 1;
        if (argNodes.size() != expectedArgs) {
          this->m_TypeErrorMessages.emplace_back(
              "Constructor for '" + symbolInfo.definedTypeName + "' expects " +
                  std::to_string(expectedArgs) + " argument(s), but " +
                  std::to_string(argNodes.size()) + " provided",
              symbolInfo);
        } else {
          for (size_t i = 0; i < argNodes.size(); ++i) {
            const auto &param = signatureIt->second.params[i + 1];
            m_LastSymbolInfo = param;
            if (m_LastSymbolInfo.isRef) {
              m_LastSymbolInfo.indirectionLevel += 1;
            }
            m_LastSymbolInfo.symbolId = m_SymbolId;
            m_LastSymbolInfo.scopeId = m_ScopeId;
            m_LastSymbolInfo.scopeLevel = m_ScopeLevel;
            m_ExpectedType = param.type;
            m_DefinedTypeFlag = param.type == TypeSpecifier::SPEC_DEFINED;
            m_DefinedTypeName = param.definedTypeName;
            argNodes[i]->acceptBefore(*this);
          }
        }
      }
    } else {
      auto tempSymbolInfo = varAst.m_RHS->acceptBefore(*this);
      if (tempSymbolInfo.isNullable && !symbolInfo.isNullable &&
          tempSymbolInfo.indirectionLevel > 0 &&
          m_NonNullFlowSymbols.count(tempSymbolInfo.symbolName) == 0) {
        this->m_TypeErrorMessages.emplace_back(
            "Nullable pointer cannot initialize a non-null pointer without an "
            "`if (ptr)` non-null proof",
            symbolInfo, DiagnosticCode::SemanticQualifierMismatch);
      }
      symbolInfo.ptrAliases = std::move(tempSymbolInfo.ptrAliases);
      if (symbolInfo.protocolStates.empty() &&
          !tempSymbolInfo.protocolStates.empty()) {
        symbolInfo.protocolStates = tempSymbolInfo.protocolStates;
      }
      if (auto *moveSource = dynamicMoveSource()) {
        SymbolInfo source;
        SymbolInfo lookup;
        lookup.symbolName = moveSource->m_SymbolName;
        lookup.begin = moveSource->m_SemLoc.begin;
        lookup.end = moveSource->m_SemLoc.end;
        lookup.line = moveSource->m_SemLoc.line;
        auto sourceName = moveSource->m_SymbolName;
        if (symbolValidation(sourceName, lookup, source, true)) {
          m_MovedUniqueSymbols.insert(SymbolStateKey(source));
          m_MovedUniqueSymbols.erase(SymbolStateKey(symbolInfo));
        }
      } else if (markUniqueMoveSource) {
        m_MovedUniqueSymbols.insert(SymbolStateKey(uniqueMoveSource));
        m_MovedUniqueSymbols.erase(SymbolStateKey(symbolInfo));
      } else if (symbolInfo.indirectionLevel > 0) {
        m_MovedUniqueSymbols.erase(SymbolStateKey(symbolInfo));
      }
    }
  } else {
    if (varAst.m_RHS != nullptr) {
      this->m_LastSymbolInfo = symbolInfo;
      auto tempSmbolInfo = varAst.m_RHS->acceptBefore(*this);
      if (tempSmbolInfo.isNullable && !symbolInfo.isNullable &&
          tempSmbolInfo.indirectionLevel > 0 &&
          m_NonNullFlowSymbols.count(tempSmbolInfo.symbolName) == 0) {
        this->m_TypeErrorMessages.emplace_back(
            "Nullable pointer cannot initialize a non-null pointer without an "
            "`if (ptr)` non-null proof",
            symbolInfo, DiagnosticCode::SemanticQualifierMismatch);
      }
      symbolInfo.ptrAliases = std::move(tempSmbolInfo.ptrAliases);
      if (symbolInfo.protocolStates.empty() &&
          !tempSmbolInfo.protocolStates.empty()) {
        symbolInfo.protocolStates = tempSmbolInfo.protocolStates;
      }
    }
  }

  if (m_TypeChecking && symbolInfo.type == TypeSpecifier::SPEC_DEFINED &&
      !symbolInfo.definedTypeName.empty()) {
    if (symbolInfo.protocolStates.empty()) {
      if (const auto* protocol = protocolForType(symbolInfo.definedTypeName)) {
        if (!protocol->defaultState.empty()) {
          symbolInfo.protocolStates[protocol->name] = protocol->defaultState;
        }
      }
    }
    if (!symbolInfo.protocolStates.empty()) {
      m_LocalProtocolStates[symbolInfo.symbolName] = symbolInfo.protocolStates;
    }
  }

  this->m_LastVarDecl = previousLastVarDecl;
  this->m_ExpectedType = previousExpectedType;
  this->m_LastSymbolInfo = previousLastSymbolInfo;
  this->m_DefinedTypeFlag = previousDefinedTypeFlag;
  this->m_DefinedTypeName = previousDefinedTypeName;
  this->m_LastBinOpHasAtLeastOnePtr = previousLastBinOpHasPtr;

  return symbolInfo;
}
