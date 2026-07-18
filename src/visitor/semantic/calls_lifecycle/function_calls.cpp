#include <visitor/semantic/semantic_private.hpp>

static bool ParseDynamicDispatchName(const std::string &name,
                                     std::string &traitName,
                                     std::string &methodName) {
  const std::string prefix = "$dynamic.";
  if (name.rfind(prefix, 0) != 0) {
    return false;
  }

  const auto rest = name.substr(prefix.size());
  const auto split = rest.rfind('.');
  if (split == std::string::npos || split == 0 || split + 1 >= rest.size()) {
    return false;
  }

  traitName = rest.substr(0, split);
  methodName = rest.substr(split + 1);
  return true;
}

static const FunctionSignature *FindDynamicDispatchSignature(
    const std::string &traitName, const std::string &methodName) {
  for (const auto &structEntry : Visitor::StructTable) {
    const auto &traits = structEntry.second.traits;
    if (std::find(traits.begin(), traits.end(), traitName) == traits.end()) {
      continue;
    }

    const auto candidate = structEntry.first + "." + methodName;
    auto signatureIt = Visitor::FunctionTable.find(candidate);
    if (signatureIt != Visitor::FunctionTable.end()) {
      return &signatureIt->second;
    }
  }

  return nullptr;
}

static std::string UnqualifiedDefinedTypeName(const std::string &name) {
  const auto dot = name.rfind('.');
  return dot == std::string::npos ? name : name.substr(dot + 1);
}

static bool SameDefinedTypeName(const std::string &actual,
                                const std::string &expected) {
  return actual == expected ||
         (!actual.empty() && !expected.empty() &&
          UnqualifiedDefinedTypeName(actual) ==
              UnqualifiedDefinedTypeName(expected));
}

SymbolInfo Visitor::preVisit(FuncCallAST &funcCallAst) {
  SymbolInfo symbolInfo;

  symbolInfo.begin = funcCallAst.m_SemLoc.begin;
  symbolInfo.end = funcCallAst.m_SemLoc.end;
  symbolInfo.line = funcCallAst.m_SemLoc.line;

  if (!m_TypeChecking) {
    return symbolInfo;
  }

  auto funcName =
      resolveFunctionCallName(funcCallAst.m_FuncSymbol.get(), symbolInfo, true);
  if (funcName.empty()) {
    return symbolInfo;
  }

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
  collectArgs(collectArgs, funcCallAst.m_Args.get());
  const bool hasImplicitMethodReceiver =
      methodCallReceiver(funcCallAst.m_FuncSymbol.get()) != nullptr;
  if (auto *receiver = methodCallReceiver(funcCallAst.m_FuncSymbol.get())) {
    argNodes.insert(argNodes.begin(), receiver);
  }

  if (funcName == "print") {
    if (argNodes.empty()) {
      this->m_TypeErrorMessages.emplace_back(
          "Builtin 'print' expects at least 1 argument", symbolInfo);
    }
    symbolInfo.type = TypeSpecifier::SPEC_VOID;
    return symbolInfo;
  }

  if (funcName == "input_int") {
    if (!argNodes.empty()) {
      this->m_TypeErrorMessages.emplace_back(
          "Builtin 'input_int' does not accept arguments", symbolInfo);
    }
    symbolInfo.type = TypeSpecifier::SPEC_I64;
    symbolInfo.indirectionLevel = 0;
    return symbolInfo;
  }

  if (funcName == "input_string") {
    if (!argNodes.empty()) {
      this->m_TypeErrorMessages.emplace_back(
          "Builtin 'input_string' does not accept arguments", symbolInfo);
    }
    symbolInfo.type = TypeSpecifier::SPEC_CHAR;
    symbolInfo.indirectionLevel = 1;
    return symbolInfo;
  }

  if (funcName == "clear_screen") {
    if (!argNodes.empty()) {
      this->m_TypeErrorMessages.emplace_back(
          "Builtin 'clear_screen' does not accept arguments", symbolInfo);
    }
    symbolInfo.type = TypeSpecifier::SPEC_VOID;
    return symbolInfo;
  }

  if (funcName == "flush_output") {
    if (!argNodes.empty()) {
      this->m_TypeErrorMessages.emplace_back(
          "Builtin 'flush_output' does not accept arguments", symbolInfo);
    }
    symbolInfo.type = TypeSpecifier::SPEC_VOID;
    return symbolInfo;
  }

  if (funcName == "sleep_ms") {
    if (argNodes.size() != 1) {
      this->m_TypeErrorMessages.emplace_back(
          "Builtin 'sleep_ms' expects exactly 1 argument", symbolInfo);
    }
    symbolInfo.type = TypeSpecifier::SPEC_VOID;
    return symbolInfo;
  }

  if (funcName == "enable_raw_input") {
    if (!argNodes.empty()) {
      this->m_TypeErrorMessages.emplace_back(
          "Builtin 'enable_raw_input' does not accept arguments", symbolInfo);
    }
    symbolInfo.type = TypeSpecifier::SPEC_VOID;
    return symbolInfo;
  }

  if (funcName == "disable_raw_input") {
    if (!argNodes.empty()) {
      this->m_TypeErrorMessages.emplace_back(
          "Builtin 'disable_raw_input' does not accept arguments", symbolInfo);
    }
    symbolInfo.type = TypeSpecifier::SPEC_VOID;
    return symbolInfo;
  }

  if (funcName == "read_key") {
    if (!argNodes.empty()) {
      this->m_TypeErrorMessages.emplace_back(
          "Builtin 'read_key' does not accept arguments", symbolInfo);
    }
    symbolInfo.type = TypeSpecifier::SPEC_I32;
    symbolInfo.indirectionLevel = 0;
    return symbolInfo;
  }

  if (funcName == "unsafe_span") {
    if (!m_LastSymbolInfo.isRuntimeSizedArray) {
      this->m_TypeErrorMessages.emplace_back(
          "`unsafe_span(ptr, len)` can only be used where a `T[]` span "
          "parameter is expected",
          symbolInfo, DiagnosticCode::SemanticInvalidQualifier);
      return symbolInfo;
    }

    if (argNodes.size() != 2) {
      this->m_TypeErrorMessages.emplace_back(
          "`unsafe_span` expects exactly 2 arguments: pointer and length",
          symbolInfo);
      return symbolInfo;
    }

    SymbolInfo pointerInfo;
    if (argNodes[0]->m_ExprKind == ExprKind::SymbolExpr) {
      auto *pointerSymbol = static_cast<SymbolAST *>(argNodes[0]);
      SymbolInfo lookup;
      auto pointerName = pointerSymbol->m_SymbolName;
      if (!symbolValidation(pointerName, lookup, pointerInfo)) {
        return symbolInfo;
      }
    } else {
      auto previousLastSymbol = m_LastSymbolInfo;
      m_LastSymbolInfo = {};
      pointerInfo = argNodes[0]->acceptBefore(*this);
      m_LastSymbolInfo = previousLastSymbol;
    }

    if (pointerInfo.indirectionLevel == 0 || pointerInfo.isRef) {
      this->m_TypeErrorMessages.emplace_back(
          "`unsafe_span` first argument must be a pointer", pointerInfo);
    }

    const auto expectedType = m_LastSymbolInfo.type;
    const auto expectedDefinedType = m_LastSymbolInfo.definedTypeName;
    const bool definedMismatch =
        expectedType == TypeSpecifier::SPEC_DEFINED &&
        !SameDefinedTypeName(pointerInfo.definedTypeName, expectedDefinedType);
    if (pointerInfo.type != expectedType || definedMismatch) {
      this->m_TypeErrorMessages.emplace_back(
          "`unsafe_span` pointer element type is incompatible with expected "
          "`T[]` parameter",
          pointerInfo);
    }

    auto previousLastSymbol = m_LastSymbolInfo;
    auto previousExpectedType = m_ExpectedType;
    m_LastSymbolInfo = {};
    m_LastSymbolInfo.type = TypeSpecifier::SPEC_I64;
    m_ExpectedType = TypeSpecifier::SPEC_I64;
    SymbolInfo lengthInfo;
    if (argNodes[1]->m_ExprKind == ExprKind::ScalarExpr) {
      lengthInfo =
          InferScalarLiteralType(static_cast<ScalarOrLiteralAST *>(argNodes[1]));
    } else {
      lengthInfo = argNodes[1]->acceptBefore(*this);
    }
    m_LastSymbolInfo = previousLastSymbol;
    m_ExpectedType = previousExpectedType;
    if (!IsIntegerType(lengthInfo.type) || lengthInfo.indirectionLevel > 0) {
      this->m_TypeErrorMessages.emplace_back(
          "`unsafe_span` length must be an integer value", lengthInfo);
    }

    symbolInfo = m_LastSymbolInfo;
    symbolInfo.isSubscriptable = true;
    symbolInfo.isRuntimeSizedArray = true;
    symbolInfo.arrayDimensions.clear();
    return symbolInfo;
  }

  if (funcName == "strong_count") {
    if (argNodes.size() != 1) {
      this->m_TypeErrorMessages.emplace_back(
          "Builtin 'strong_count' expects exactly 1 argument", symbolInfo);
    } else if (argNodes[0]->m_ExprKind != ExprKind::SymbolExpr) {
      this->m_TypeErrorMessages.emplace_back(
          "Builtin 'strong_count' expects a shared pointer symbol",
          symbolInfo);
    } else {
      auto *argSymbol = static_cast<SymbolAST *>(argNodes[0]);
      SymbolInfo lookupInfo;
      SymbolInfo matchedSymbol;
      auto argName = argSymbol->m_SymbolName;
      if (symbolValidation(argName, lookupInfo, matchedSymbol)) {
        if (matchedSymbol.indirectionLevel == 0 || matchedSymbol.isUnique) {
          this->m_TypeErrorMessages.emplace_back(
              "Builtin 'strong_count' expects a shared pointer symbol",
              symbolInfo, DiagnosticCode::SemanticOwnership);
        }
      }
    }
    symbolInfo.type = TypeSpecifier::SPEC_I64;
    symbolInfo.indirectionLevel = 0;
    return symbolInfo;
  }

  std::string dynamicTraitName;
  std::string dynamicMethodName;
  if (ParseDynamicDispatchName(funcName, dynamicTraitName,
                               dynamicMethodName)) {
    const auto *signature =
        FindDynamicDispatchSignature(dynamicTraitName, dynamicMethodName);
    if (signature == nullptr) {
      this->m_TypeErrorMessages.emplace_back(
          "Dynamic trait method '" + dynamicTraitName + "." +
              dynamicMethodName +
              "' has no concrete implementation signature to dispatch",
          symbolInfo);
      return symbolInfo;
    }

    if (argNodes.empty()) {
      this->m_TypeErrorMessages.emplace_back(
          "Dynamic trait method call requires a receiver", symbolInfo);
      return symbolInfo;
    }

    const auto explicitArgCount = argNodes.size() - 1;
    const auto requiredExplicitArgCount =
        signature->params.empty() ? 0 : signature->params.size() - 1;
    if (explicitArgCount != requiredExplicitArgCount) {
      this->m_TypeErrorMessages.emplace_back(
          "Dynamic trait method '" + dynamicTraitName + "." +
              dynamicMethodName + "' expects " +
              std::to_string(requiredExplicitArgCount) +
              " argument(s), but " + std::to_string(explicitArgCount) +
              " provided",
          symbolInfo);
      return signature->returnType;
    }

    for (size_t i = 1; i < argNodes.size() && i < signature->params.size();
         ++i) {
      m_LastSymbolInfo = signature->params[i];
      m_LastSymbolInfo.symbolId = m_SymbolId;
      m_LastSymbolInfo.scopeId = m_ScopeId;
      m_LastSymbolInfo.scopeLevel = m_ScopeLevel;
      if (m_LastSymbolInfo.isRef &&
          !m_LastSymbolInfo.isDynamicTraitObject &&
          m_LastSymbolInfo.indirectionLevel > 0) {
        m_LastSymbolInfo.indirectionLevel -= 1;
      }
      m_ExpectedType = signature->params[i].type;
      m_DefinedTypeFlag =
          signature->params[i].type == TypeSpecifier::SPEC_DEFINED;
      m_DefinedTypeName = signature->params[i].definedTypeName;
      argNodes[i]->acceptBefore(*this);
    }

    symbolInfo = signature->returnType;
    symbolInfo.symbolName = funcName;
    return symbolInfo;
  }

  auto signatureIt = FunctionTable.find(funcName);
  if (signatureIt == FunctionTable.end()) {
    this->m_TypeErrorMessages.emplace_back(
        "Function '" + funcName + "' was not declared",
        symbolInfo);
    return symbolInfo;
  }

  const auto &signature = signatureIt->second;
  if (signature.isFromIncludedSource && !signature.returnType.isPublic &&
      !signature.returnType.isImported &&
      !m_CurrentFunctionFromIncludedSource) {
    this->m_TypeErrorMessages.emplace_back(
        "Function '" + funcName +
            "' is private to its included module and cannot be called here",
        symbolInfo);
    return signature.returnType;
  }
  if (signature.canThrow && !m_CurrentFunctionCanThrow) {
    this->m_TypeErrorMessages.emplace_back(
        "fallible function '" + funcName +
            "' can only be called from an `except` function or an explicit "
            "result-handling form",
        symbolInfo);
  }
  if (m_TypeChecking && m_CurrentFunctionIsStatic &&
      !signature.returnType.isStatic) {
    this->m_TypeErrorMessages.emplace_back(
        "static function cannot call non-static function '" + funcName + "'",
        symbolInfo);
  }
  if ((!signature.isVariadic && signature.params.size() != argNodes.size()) ||
      (signature.isVariadic && argNodes.size() < signature.params.size())) {
    this->m_TypeErrorMessages.emplace_back(
        "Function '" + funcName + "' expects " +
            (signature.isVariadic
                 ? "at least " + std::to_string(signature.params.size())
                 : std::to_string(signature.params.size())) +
            " argument(s), but " +
            std::to_string(argNodes.size()) + " provided",
        symbolInfo);
    return signature.returnType;
  }

  const bool callValueIsChecked =
      m_LastVarDecl || m_LastAssignment || m_LastRetExpr || m_LastBinOp;
  const auto expectedCallResultType =
      m_LastRetExpr ? m_LastFuncRetTypeInfo.type : m_ExpectedType;
  auto previousExpectedType = m_ExpectedType;
  auto previousLastSymbolInfo = m_LastSymbolInfo;
  auto previousDefinedTypeFlag = m_DefinedTypeFlag;
  auto previousLastBinOpHasPtr = m_LastBinOpHasAtLeastOnePtr;

  auto moveSourceSymbol = [](IAST *node) -> SymbolAST * {
    if (node == nullptr || node->m_ExprKind != ExprKind::UnaryOp) {
      return nullptr;
    }

    auto *unary = static_cast<UnaryOpAST *>(node);
    if (unary->m_UnaryOpKind != U_MOVE ||
        unary->m_Node->m_ExprKind != ExprKind::SymbolExpr) {
      return nullptr;
    }

    return static_cast<SymbolAST *>(unary->m_Node.get());
  };

  auto copySourceSymbol = [](IAST *node) -> SymbolAST * {
    if (node == nullptr || node->m_ExprKind != ExprKind::UnaryOp) {
      return nullptr;
    }

    auto *unary = static_cast<UnaryOpAST *>(node);
    if (unary->m_UnaryOpKind != U_COPY ||
        unary->m_Node->m_ExprKind != ExprKind::SymbolExpr) {
      return nullptr;
    }

    return static_cast<SymbolAST *>(unary->m_Node.get());
  };

  for (size_t i = 0; i < argNodes.size(); ++i) {
    const bool isVariadicArg = i >= signature.params.size();
    bool argumentAlreadyChecked = false;
    if (hasImplicitMethodReceiver && i == 0 && signature.params[i].isRef) {
      continue;
    }

    if (isVariadicArg) {
      if (argNodes[i]->m_ExprKind == ExprKind::SymbolExpr) {
        auto *argSymbol = static_cast<SymbolAST *>(argNodes[i]);
        SymbolInfo argInfo;
        SymbolInfo matchedSymbol;
        argInfo.symbolName = argSymbol->m_SymbolName;
        argInfo.begin = argSymbol->m_SemLoc.begin;
        argInfo.end = argSymbol->m_SemLoc.end;
        argInfo.line = argSymbol->m_SemLoc.line;
        symbolValidation(argSymbol->m_SymbolName, argInfo, matchedSymbol);
      }
      continue;
    }

    m_LastSymbolInfo = signature.params[i];
    m_LastSymbolInfo.symbolId = m_SymbolId;
    m_LastSymbolInfo.scopeId = m_ScopeId;
    m_LastSymbolInfo.scopeLevel = m_ScopeLevel;
    m_ExpectedType = signature.params[i].type;
    m_DefinedTypeFlag = signature.params[i].type == TypeSpecifier::SPEC_DEFINED;
    m_DefinedTypeName = signature.params[i].definedTypeName;
    m_LastBinOpHasAtLeastOnePtr = false;

    const bool parameterIsOwnedPointer =
        signature.params[i].indirectionLevel > 0 &&
        !signature.params[i].isRef && !signature.params[i].isSubscriptable;
    auto *argMoveSource = moveSourceSymbol(argNodes[i]);
    SymbolInfo movedCallSource;
    bool markMovedCallSource = false;
    if (parameterIsOwnedPointer && argMoveSource != nullptr) {
      SymbolInfo lookupInfo;
      SymbolInfo movedSource;
      auto sourceName = argMoveSource->m_SymbolName;
      if (symbolValidation(sourceName, lookupInfo, movedSource, true)) {
        if (movedSource.isNoMove) {
          SymbolInfo argInfo;
          argInfo.begin = argMoveSource->m_SemLoc.begin;
          argInfo.end = argMoveSource->m_SemLoc.end;
          argInfo.line = argMoveSource->m_SemLoc.line;
          this->m_TypeErrorMessages.emplace_back(
              "'nomove' pointer cannot be moved", argInfo,
              DiagnosticCode::SemanticOwnership);
        } else if (movedSource.indirectionLevel == 0 ||
            movedSource.isUnique != signature.params[i].isUnique) {
          SymbolInfo argInfo;
          argInfo.begin = argMoveSource->m_SemLoc.begin;
          argInfo.end = argMoveSource->m_SemLoc.end;
          argInfo.line = argMoveSource->m_SemLoc.line;
          this->m_TypeErrorMessages.emplace_back(
              "'move' argument requires matching pointer ownership kind",
              argInfo, DiagnosticCode::SemanticOwnership);
        } else {
          movedCallSource = movedSource;
          markMovedCallSource = true;
        }
      }
    }

	    if (signature.params[i].isRuntimeSizedArray) {
	      bool isExistingSpanValue = false;
	      SymbolInfo existingSpanInfo;
      if (argNodes[i]->m_ExprKind == ExprKind::SymbolExpr) {
        auto *argSymbol = static_cast<SymbolAST *>(argNodes[i]);
        SymbolInfo lookup;
        auto argName = argSymbol->m_SymbolName;
        isExistingSpanValue =
            symbolValidation(argName, lookup, existingSpanInfo, true) &&
            existingSpanInfo.isRuntimeSizedArray;
      }

      const bool isUnsafeSpanCall =
          argNodes[i]->m_ExprKind == ExprKind::FuncCallExpr &&
          static_cast<FuncCallAST *>(argNodes[i])->m_FuncSymbol != nullptr &&
          static_cast<FuncCallAST *>(argNodes[i])
                  ->m_FuncSymbol->m_ExprKind == ExprKind::SymbolExpr &&
          static_cast<SymbolAST *>(
              static_cast<FuncCallAST *>(argNodes[i])->m_FuncSymbol.get())
                  ->m_SymbolName == "unsafe_span";

      if (!isExistingSpanValue &&
          argNodes[i]->m_ExprKind != ExprKind::SpanExpr &&
          !isUnsafeSpanCall) {
        SymbolInfo argInfo;
        argInfo.begin = argNodes[i]->getSemLoc().begin;
        argInfo.end = argNodes[i]->getSemLoc().end;
        argInfo.line = argNodes[i]->getSemLoc().line;
        this->m_TypeErrorMessages.emplace_back(
            "Function argument " + std::to_string(i + 1) +
                " must use `span value`, `span value[begin..end]`, or "
                "`unsafe_span(ptr, len)` for `T[]` parameters",
            argInfo);
        continue;
      }

      auto spanInfo = isExistingSpanValue ? existingSpanInfo
                                          : argNodes[i]->acceptBefore(*this);
      const bool definedMismatch =
          signature.params[i].type == TypeSpecifier::SPEC_DEFINED &&
          !SameDefinedTypeName(spanInfo.definedTypeName,
                               signature.params[i].definedTypeName);
      if (!spanInfo.isRuntimeSizedArray || spanInfo.type != signature.params[i].type ||
          definedMismatch ||
          spanInfo.indirectionLevel != signature.params[i].indirectionLevel) {
        this->m_TypeErrorMessages.emplace_back(
            "Function argument " + std::to_string(i + 1) +
                " is incompatible with runtime-sized array parameter",
            spanInfo);
      }
      argumentAlreadyChecked = true;
	      continue;
	    }

	    if (signature.params[i].requiresExplicitRefArgument &&
	        !signature.params[i].isDynamicTraitObject) {
	      const bool isExplicitRefArgument =
	          argNodes[i]->m_ExprKind == ExprKind::UnaryOp &&
	          static_cast<UnaryOpAST *>(argNodes[i])->m_UnaryOpKind == U_REF;
	      if (!isExplicitRefArgument) {
	        SymbolInfo argInfo;
	        argInfo.begin = argNodes[i]->getSemLoc().begin;
	        argInfo.end = argNodes[i]->getSemLoc().end;
	        argInfo.line = argNodes[i]->getSemLoc().line;
	        this->m_TypeErrorMessages.emplace_back(
	            "Function '" + funcName + "' argument " +
	                std::to_string(i + 1) +
	                " must use `ref value` for reference parameters",
	            argInfo);
	        continue;
	      }
	    }

	    if (argNodes[i]->m_ExprKind == ExprKind::ScalarExpr) {
	      auto *scalar = static_cast<ScalarOrLiteralAST *>(argNodes[i]);
      const auto expectedType = signature.params[i].type;
      const bool expectedInteger = IsIntegerType(expectedType);

      if ((expectedInteger && scalar->m_IsBoolean) ||
          (expectedType == TypeSpecifier::SPEC_BOOL && !scalar->m_IsBoolean)) {
        SymbolInfo argInfo;
        argInfo.begin = scalar->m_SemLoc.begin;
        argInfo.end = scalar->m_SemLoc.end;
        argInfo.line = scalar->m_SemLoc.line;
        this->m_TypeErrorMessages.emplace_back(
            "Function argument " + std::to_string(i + 1) +
                " is incompatible with parameter type '" +
                GetTypeStr(expectedType) + "'",
            argInfo);
        continue;
      }
    } else if (argNodes[i]->m_ExprKind == ExprKind::SymbolExpr ||
               copySourceSymbol(argNodes[i]) != nullptr) {
      auto *argSymbol = copySourceSymbol(argNodes[i]) != nullptr
                            ? copySourceSymbol(argNodes[i])
                            : static_cast<SymbolAST *>(argNodes[i]);
      SymbolInfo argInfo;
      SymbolInfo matchedSymbol;
      argInfo.symbolName = argSymbol->m_SymbolName;
      argInfo.begin = argSymbol->m_SemLoc.begin;
      argInfo.end = argSymbol->m_SemLoc.end;
      argInfo.line = argSymbol->m_SemLoc.line;

      auto argSymbolName = argSymbol->m_SymbolName;
      if (!symbolValidation(argSymbolName, argInfo, matchedSymbol)) {
        continue;
      }

      const auto expectedType = signature.params[i].type;
      const auto actualType = matchedSymbol.type;
      if (signature.params[i].isRef &&
          !signature.params[i].isDynamicTraitObject) {
        const bool definedMismatch =
            expectedType == TypeSpecifier::SPEC_DEFINED &&
            !SameDefinedTypeName(matchedSymbol.definedTypeName,
                                 signature.params[i].definedTypeName);
        const bool refLevelCompatible =
            matchedSymbol.indirectionLevel ==
            signature.params[i].indirectionLevel;

        if (actualType != expectedType || definedMismatch ||
            !refLevelCompatible) {
          this->m_TypeErrorMessages.emplace_back(
              "Function argument " + std::to_string(i + 1) +
                  " is incompatible with reference parameter",
              argInfo);
        }
        argumentAlreadyChecked = true;
      }

      if (signature.params[i].indirectionLevel > 0 &&
          !signature.params[i].isRef && signature.params[i].isUnique &&
          matchedSymbol.indirectionLevel > 0 && matchedSymbol.isUnique) {
        this->m_TypeErrorMessages.emplace_back(
            "unique pointer function argument cannot be copied; use 'move' "
            "to transfer ownership",
            argInfo, DiagnosticCode::SemanticOwnership);
        continue;
      }

      if (signature.params[i].isSubscriptable != matchedSymbol.isSubscriptable) {
        this->m_TypeErrorMessages.emplace_back(
            "Function argument " + std::to_string(i + 1) +
                " is incompatible with array parameter",
            argInfo);
        continue;
      }

      if (signature.params[i].isSubscriptable) {
        if (signature.params[i].arrayDimensions.size() !=
            matchedSymbol.arrayDimensions.size()) {
          this->m_TypeErrorMessages.emplace_back(
              "Function argument " + std::to_string(i + 1) +
                  " has incompatible array dimension count",
              argInfo);
          continue;
        }

        bool dimensionMismatch = false;
        for (size_t dim = 0; dim < signature.params[i].arrayDimensions.size();
             ++dim) {
          if (signature.params[i].arrayDimensions[dim] !=
              matchedSymbol.arrayDimensions[dim]) {
            dimensionMismatch = true;
            break;
          }
        }

        if (dimensionMismatch) {
          this->m_TypeErrorMessages.emplace_back(
              "Function argument " + std::to_string(i + 1) +
                  " has incompatible array size",
              argInfo);
          continue;
        }

        argumentAlreadyChecked = true;
      }

      const bool boolMismatch =
          (IsIntegerType(expectedType) && actualType == TypeSpecifier::SPEC_BOOL) ||
          (expectedType == TypeSpecifier::SPEC_BOOL && IsIntegerType(actualType));

      if (boolMismatch) {
        this->m_TypeErrorMessages.emplace_back(
            "Function argument " + std::to_string(i + 1) +
                " is incompatible with parameter type '" +
                GetTypeStr(expectedType) + "'",
            argInfo);
        continue;
      }
    }

    if (!argumentAlreadyChecked &&
        !(hasImplicitMethodReceiver && i == 0 && signature.params[i].isRef)) {
      argNodes[i]->acceptBefore(*this);
    }
    if (markMovedCallSource) {
      m_MovedUniqueSymbols.insert(SymbolStateKey(movedCallSource));
    }
  }

  m_ExpectedType = previousExpectedType;
  m_LastSymbolInfo = previousLastSymbolInfo;
  m_DefinedTypeFlag = previousDefinedTypeFlag;
  m_LastBinOpHasAtLeastOnePtr = previousLastBinOpHasPtr;

  symbolInfo = signature.returnType;
  symbolInfo.symbolName = funcName;
  symbolInfo.begin = funcCallAst.m_SemLoc.begin;
  symbolInfo.end = funcCallAst.m_SemLoc.end;
  symbolInfo.line = funcCallAst.m_SemLoc.line;

  if (callValueIsChecked && IsPrimitiveType(expectedCallResultType) &&
      IsPrimitiveType(symbolInfo.type) &&
      expectedCallResultType != symbolInfo.type) {
    const bool expectedInteger = IsIntegerType(expectedCallResultType);
    const bool actualInteger = IsIntegerType(symbolInfo.type);
    const bool boolMismatch =
        expectedCallResultType == TypeSpecifier::SPEC_BOOL ||
        symbolInfo.type == TypeSpecifier::SPEC_BOOL;

    if (boolMismatch || expectedCallResultType == TypeSpecifier::SPEC_VOID ||
        symbolInfo.type == TypeSpecifier::SPEC_VOID) {
      this->m_TypeErrorMessages.emplace_back(
          "Function '" + funcName + "' returns '" +
              GetTypeStr(symbolInfo.type) + "', but '" +
              GetTypeStr(expectedCallResultType) + "' is expected",
          symbolInfo);
    } else if (expectedInteger && actualInteger &&
               LosslessCasting(expectedCallResultType, symbolInfo.type)) {
      this->m_TypeWarningMessages.emplace_back(
          "Function '" + funcName + "' returns '" +
              GetTypeStr(symbolInfo.type) + "', which is casting to '" +
              GetTypeStr(expectedCallResultType) +
              "'. Potential data loss might be occured!",
          symbolInfo);
    }
  }

  applyProtocolMethodCall(funcName, funcCallAst, symbolInfo);
  return symbolInfo;
}
