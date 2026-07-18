#include <visitor/semantic/semantic_private.hpp>

SymbolInfo Visitor::preVisit(FuncAST &funcAst) {
  SymbolInfo symbolInfo;

  if (funcAst.m_RetType != nullptr) {
    auto retType = dynamic_cast<TypeAST *>(funcAst.m_RetType.get());

    symbolInfo.type = retType->m_TypeSpec;
    if (retType->m_TypeSpec == TypeSpecifier::SPEC_DEFINED &&
        retType->m_Symbol != nullptr &&
        retType->m_Symbol->m_ExprKind == ExprKind::SymbolExpr) {
      auto *typeSymbol = static_cast<SymbolAST *>(retType->m_Symbol.get());
      symbolInfo.definedTypeName = typeSymbol->m_SymbolName;
    }
    symbolInfo.indirectionLevel = retType->m_IndirectLevel;
    symbolInfo.isConstRef = funcAst.m_RetTypeQualifier == Q_CONSTREF;
    symbolInfo.isConstPtr = funcAst.m_RetTypeQualifier == Q_CONSTPTR;
    symbolInfo.isReadOnly = funcAst.m_RetTypeQualifier == Q_READONLY;
    symbolInfo.isConstVal = funcAst.m_RetTypeQualifier == Q_CONST;
    symbolInfo.isRef = retType->m_IsRef;
    symbolInfo.isUnique = retType->m_IsUniquePtr;
    symbolInfo.isNullable = retType->m_IsNullable;
    symbolInfo.isDynamicTraitObject = retType->m_IsDynamicTraitObject;
    symbolInfo.protocolStates = retType->acceptBefore(*this).protocolStates;
    symbolInfo.qualifierLevels = BuildQualifierLevels(
        funcAst.m_RetTypeQualifier, symbolInfo.indirectionLevel,
        symbolInfo.isRef);

    m_LastFuncRetTypeInfo = symbolInfo;
  }

  symbolInfo.assocFuncName = funcAst.m_FuncName;
  symbolInfo.begin = funcAst.m_SemLoc.begin;
  symbolInfo.end = funcAst.m_SemLoc.end;
  symbolInfo.line = funcAst.m_SemLoc.line;
  symbolInfo.isPublic = funcAst.m_Access == ACCESS_PUBLIC;
  symbolInfo.isStatic = funcAst.m_IsStatic;
  symbolInfo.isExported = funcAst.m_IsExported;
  symbolInfo.isImported = funcAst.m_IsForwardDecl && !funcAst.m_IsExported;

  if (funcAst.m_IsVariadic && !funcAst.m_IsForwardDecl) {
    this->m_TypeErrorMessages.emplace_back(
        "Variadic parameter marker '...' is currently supported only for "
        "native import/export declarations",
        symbolInfo);
  }

  if (symbolInfo.isDynamicTraitObject) {
    if (TraitTable.count(symbolInfo.definedTypeName) == 0) {
      this->m_TypeErrorMessages.emplace_back(
          "dynamic trait object return target '" + symbolInfo.definedTypeName +
              "' must name a trait",
          symbolInfo);
    } else if (!symbolInfo.isRef) {
      this->m_TypeErrorMessages.emplace_back(
          "owned dynamic trait object lowering is not implemented yet; use "
          "`dynamic Trait&` for borrowed dispatch",
          symbolInfo, DiagnosticCode::SemanticOwnership);
    }
  }

  std::string methodOwner;
  std::string methodName;
  if (SplitStructMethodName(funcAst.m_FuncName, methodOwner, methodName)) {
    if (methodName == "constructor" || methodName == "destructor") {
      if (funcAst.m_HasExplicitReturnType) {
        this->m_TypeErrorMessages.emplace_back(
            "Lifecycle method '" + methodName +
                "' does not accept an explicit return type; it is always void",
            symbolInfo);
      }

      if (methodName == "destructor") {
        const size_t userParamCount =
            funcAst.m_Params.empty() ? 0 : funcAst.m_Params.size() - 1;
        if (userParamCount != 0) {
          this->m_TypeErrorMessages.emplace_back(
              "Lifecycle method 'destructor' cannot accept user parameters",
              symbolInfo);
        }
      }
    } else if (methodName == "new") {
      if (!funcAst.m_IsStatic) {
        this->m_TypeErrorMessages.emplace_back(
            "struct 'new' allocation entry must be static and called as '" +
                methodOwner + "::new(...)'",
            symbolInfo);
      }

      if (symbolInfo.type != TypeSpecifier::SPEC_DEFINED ||
          symbolInfo.definedTypeName != methodOwner) {
        this->m_TypeErrorMessages.emplace_back(
            "struct 'new' allocation entry must return '" + methodOwner + "^' "
            "or '" + methodOwner + "*'",
            symbolInfo);
      } else if (symbolInfo.indirectionLevel == 0) {
        this->m_TypeErrorMessages.emplace_back(
            "by-value construction uses the constructor syntax '" +
                methodOwner + "(...)'; 'new' is reserved for allocation "
                "entries",
            symbolInfo);
      } else {
        this->m_TypeErrorMessages.emplace_back(
            "struct 'new' allocation lowering requires allocator/control-block "
            "support and is not implemented yet",
            symbolInfo);
      }
    }
  }

  if (m_TypeChecking) {
    const bool previousStaticFunction = m_CurrentFunctionIsStatic;
    const bool previousCanThrow = m_CurrentFunctionCanThrow;
    const bool previousFromIncludedSource = m_CurrentFunctionFromIncludedSource;
    const auto previousErrorTypeName = m_CurrentFunctionErrorTypeName;
    const auto previousStructMethodOwner = m_CurrentStructMethodOwner;
    m_DroppedSemanticSymbols.clear();
    m_LocalProtocolStates.clear();
    m_CurrentFunctionIsStatic = funcAst.m_IsStatic;
    m_CurrentFunctionCanThrow = funcAst.m_CanThrow;
    m_CurrentFunctionFromIncludedSource = funcAst.isFromIncludedSource();
    m_CurrentFunctionErrorTypeName = funcAst.m_ErrorTypeName;
    if (m_CurrentFunctionCanThrow && m_CurrentFunctionErrorTypeName.empty()) {
      this->m_TypeErrorMessages.emplace_back(
          "except function should declare an error type: `except ErrorType :: "
          "ReturnType`",
          symbolInfo);
    } else if (m_CurrentFunctionCanThrow &&
               EnumTable.count(m_CurrentFunctionErrorTypeName) == 0 &&
               this->m_TypeTable.count(m_CurrentFunctionErrorTypeName) == 0) {
      this->m_TypeErrorMessages.emplace_back(
          "Unknown except error type '" + m_CurrentFunctionErrorTypeName + "'",
          symbolInfo);
    }
    std::string currentMethodOwner;
    std::string currentMethodName;
    if (SplitStructMethodName(funcAst.m_FuncName, currentMethodOwner,
                              currentMethodName)) {
      m_CurrentStructMethodOwner = currentMethodOwner;
    } else {
      m_CurrentStructMethodOwner.clear();
    }
    this->m_LastScopeSymbols.clear();
    for (auto &param : funcAst.m_Params) {
      auto symbol = param->acceptBefore(*this);
      symbol.symbolScope = SymbolScope::Func;
      this->m_LastScopeSymbols.emplace_back(symbol.symbolName, symbol);
    }

    enterScope(false);
    for (auto &node : funcAst.m_Scope) {
      typeCheckerScopeHandler(node);
    }
    m_CurrentFunctionIsStatic = previousStaticFunction;
    m_CurrentFunctionCanThrow = previousCanThrow;
    m_CurrentFunctionFromIncludedSource = previousFromIncludedSource;
    m_CurrentFunctionErrorTypeName = previousErrorTypeName;
    m_CurrentStructMethodOwner = previousStructMethodOwner;
  } else {
    for (auto &param : funcAst.m_Params) {
      auto symbol = param->acceptBefore(*this);

      if (symbol.isNeededTypeCheck) {
        //      if(m_TypeTable.count(symbol.ty))
        bool isLeftOne = false, isRightOne = false;
        for (auto &type : this->m_TypeTable) {
          if (type.first == symbol.definedTypenamePair.first) {
            isLeftOne = true;
          }

          if (type.first == symbol.definedTypenamePair.second) {
            isRightOne = true;
          }
        }

        if (!isLeftOne && !isRightOne) {
          this->m_TypeErrorMessages.emplace_back(
              "Unknown type '" + symbol.definedTypenamePair.first + "' or '" +
                  symbol.definedTypenamePair.second + "'",
              symbol);
        } else {
          if (isRightOne && isLeftOne) {
            auto firstSymbol = symbol.definedTypenamePair.first;
            this->m_TypeErrorMessages.emplace_back(
                "Unexpected token '" + firstSymbol + "' after '" + firstSymbol +
                    "'",
                symbol);
          } else if (isLeftOne) {
            symbol.symbolName = symbol.definedTypenamePair.second;
            this->m_SymbolInfos.push_back(symbol);
          } else {
            symbol.symbolName = symbol.definedTypenamePair.first;
            this->m_SymbolInfos.push_back(symbol);
          }
        }
      } else {
        symbol.symbolScope = SymbolScope::Func;
        this->m_SymbolInfos.push_back(symbol);
      }
    }

    enterScope(false);
    for (auto &node : funcAst.m_Scope) {
      scopeHandler(node, SymbolScope::Func);
    }
  }

  exitScope(false);
  return symbolInfo;
}
