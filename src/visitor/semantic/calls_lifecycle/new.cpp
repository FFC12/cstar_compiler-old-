#include <visitor/semantic/semantic_private.hpp>

SymbolInfo Visitor::preVisit(NewAST &newAst) {
  SymbolInfo symbolInfo;
  symbolInfo.begin = newAst.m_SemLoc.begin;
  symbolInfo.end = newAst.m_SemLoc.end;
  symbolInfo.line = newAst.m_SemLoc.line;
  symbolInfo.symbolName = "new " + newAst.m_TypeName;
  symbolInfo.type = newAst.m_TypeSpec;
  if (newAst.m_TypeSpec == TypeSpecifier::SPEC_DEFINED) {
    symbolInfo.definedTypeName = newAst.m_TypeName;
  }
  symbolInfo.indirectionLevel = 1;
  symbolInfo.isUnique = !newAst.m_IsShared;
  symbolInfo.isNullable = newAst.m_IsFallible;

  if (!m_TypeChecking) {
    return symbolInfo;
  }

  const bool targetIsDefined = newAst.m_TypeSpec == TypeSpecifier::SPEC_DEFINED;
  const bool targetIsStruct =
      targetIsDefined && StructTable.count(newAst.m_TypeName) != 0;
  const bool targetIsPrimitive =
      !targetIsDefined && newAst.m_TypeSpec != TypeSpecifier::SPEC_VOID &&
      newAst.m_TypeSpec != TypeSpecifier::SPEC_NIL;

  if (targetIsDefined &&
      (m_TypeTable.count(newAst.m_TypeName) == 0 || !targetIsStruct)) {
    this->m_TypeErrorMessages.emplace_back(
        "`new` expects a known struct type or sized primitive type",
        symbolInfo);
    return symbolInfo;
  }

  if (!targetIsDefined && !targetIsPrimitive) {
    this->m_TypeErrorMessages.emplace_back(
        "`new` expects a sized primitive or struct type", symbolInfo);
    return symbolInfo;
  }

  if (newAst.m_Allocator != nullptr) {
    if (newAst.m_Allocator->m_ExprKind != ExprKind::SymbolExpr) {
      this->m_TypeErrorMessages.emplace_back(
          "`new(allocator)` expects a named allocator value", symbolInfo);
    }

    SymbolInfo allocatorInfo;
    if (newAst.m_Allocator->m_ExprKind == ExprKind::SymbolExpr) {
      auto *allocatorSymbol =
          static_cast<SymbolAST *>(newAst.m_Allocator.get());
      SymbolInfo lookupInfo;
      lookupInfo.symbolName = allocatorSymbol->m_SymbolName;
      lookupInfo.begin = allocatorSymbol->m_SemLoc.begin;
      lookupInfo.end = allocatorSymbol->m_SemLoc.end;
      lookupInfo.line = allocatorSymbol->m_SemLoc.line;
      symbolValidation(allocatorSymbol->m_SymbolName, lookupInfo,
                       allocatorInfo, true);
    } else {
      allocatorInfo = newAst.m_Allocator->acceptBefore(*this);
    }
    if (allocatorInfo.type != TypeSpecifier::SPEC_DEFINED) {
      this->m_TypeErrorMessages.emplace_back(
          "`new(allocator)` expects a struct value implementing Allocator",
          symbolInfo);
    } else {
      std::string allocatorTraitName;
      for (const auto &entry : TraitTable) {
        if (entry.second.languageItem == "allocator") {
          allocatorTraitName = entry.first;
          break;
        }
      }

      bool satisfiesAllocator = false;
      auto structIt = StructTable.find(allocatorInfo.definedTypeName);
      if (!allocatorTraitName.empty() && structIt != StructTable.end()) {
        satisfiesAllocator =
            std::find(structIt->second.traits.begin(),
                      structIt->second.traits.end(), allocatorTraitName) !=
            structIt->second.traits.end();
      }

      if (!satisfiesAllocator) {
        this->m_TypeErrorMessages.emplace_back(
            "allocator type '" + allocatorInfo.definedTypeName +
                "' must implement the #[lang(allocator)] trait with "
                "alloc(bytes, align) and free(ptr, bytes, align)",
            symbolInfo);
      }
    }
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
  collectArgs(collectArgs, newAst.m_Args.get());

  if (targetIsPrimitive) {
    if (argNodes.size() != 1) {
      this->m_TypeErrorMessages.emplace_back(
          "Primitive `new` expects exactly one initializer expression",
          symbolInfo);
      return symbolInfo;
    }

    m_LastSymbolInfo = symbolInfo;
    m_LastSymbolInfo.indirectionLevel = 0;
    m_LastSymbolInfo.isUnique = false;
    m_LastSymbolInfo.isNullable = false;
    m_ExpectedType = newAst.m_TypeSpec;
    m_DefinedTypeFlag = false;
    m_DefinedTypeName.clear();
    argNodes[0]->acceptBefore(*this);
    return symbolInfo;
  }

  const auto constructorName = newAst.m_TypeName + ".constructor";
  auto signatureIt = FunctionTable.find(constructorName);
  if (signatureIt == FunctionTable.end()) {
    if (!argNodes.empty()) {
      this->m_TypeErrorMessages.emplace_back(
          "Struct '" + newAst.m_TypeName +
              "' does not declare a constructor for `new` arguments",
          symbolInfo);
    }
    return symbolInfo;
  }

  const auto expectedArgs =
      signatureIt->second.params.empty() ? 0 : signatureIt->second.params.size() - 1;
  if (argNodes.size() != expectedArgs) {
    this->m_TypeErrorMessages.emplace_back(
        "Constructor for '" + newAst.m_TypeName + "' expects " +
            std::to_string(expectedArgs) + " argument(s), but " +
            std::to_string(argNodes.size()) + " provided",
        symbolInfo);
    return symbolInfo;
  }

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

  return symbolInfo;
}
