#include <visitor/semantic/semantic_private.hpp>

std::string Visitor::resolveFunctionCallName(IAST *node, SymbolInfo &symbolInfo,
                                             bool emitDiagnostics) {
  if (node == nullptr) {
    return {};
  }

  if (node->m_ExprKind == ExprKind::SymbolExpr) {
    auto *symbol = static_cast<SymbolAST *>(node);
    symbolInfo.symbolName = symbol->m_SymbolName;
    symbolInfo.begin = symbol->m_SemLoc.begin;
    symbolInfo.end = symbol->m_SemLoc.end;
    symbolInfo.line = symbol->m_SemLoc.line;
    return symbol->m_SymbolName;
  }

  if (node->m_ExprKind == ExprKind::BinOp) {
    auto *binOp = static_cast<BinaryOpAST *>(node);
    if ((binOp->m_BinOpKind == BinOpKind::B_DOT ||
         binOp->m_BinOpKind == BinOpKind::B_CCOL) &&
        binOp->m_LHS != nullptr && binOp->m_RHS != nullptr &&
        binOp->m_LHS->m_ExprKind == ExprKind::SymbolExpr &&
        binOp->m_RHS->m_ExprKind == ExprKind::SymbolExpr) {
      auto *alias = static_cast<SymbolAST *>(binOp->m_LHS.get());
      auto *member = static_cast<SymbolAST *>(binOp->m_RHS.get());
      symbolInfo.symbolName = member->m_SymbolName;
      symbolInfo.begin = member->m_SemLoc.begin;
      symbolInfo.end = member->m_SemLoc.end;
      symbolInfo.line = member->m_SemLoc.line;

      if (binOp->m_BinOpKind == BinOpKind::B_CCOL) {
        auto methodName = alias->m_SymbolName + "." + member->m_SymbolName;
        if (emitDiagnostics) {
          if (m_TypeTable.count(alias->m_SymbolName) == 0) {
            this->m_TypeErrorMessages.emplace_back(
                "`::` method calls require a type name on the left side",
                symbolInfo);
            return {};
          }

          auto signatureIt = FunctionTable.find(methodName);
          if (signatureIt == FunctionTable.end()) {
            this->m_TypeErrorMessages.emplace_back(
                "Struct '" + alias->m_SymbolName +
                    "' has no static method '" + member->m_SymbolName + "'",
                symbolInfo);
            return {};
          }

          if (!signatureIt->second.returnType.isStatic) {
            this->m_TypeErrorMessages.emplace_back(
                "`::` can only call static struct methods; use '.' for "
                "instance methods",
                symbolInfo);
            return {};
          }
        }

        return methodName;
      }

      if (ModuleAliases.count(alias->m_SymbolName) != 0) {
        return member->m_SymbolName;
      }

      if (StructTable.count(alias->m_SymbolName) != 0) {
        if (emitDiagnostics) {
          this->m_TypeErrorMessages.emplace_back(
              "Instance method call requires a struct value; use '" +
                  alias->m_SymbolName + "::" + member->m_SymbolName +
                  "(...)' only for static methods",
              symbolInfo);
        }
        return {};
      }

      if (emitDiagnostics) {
        SymbolInfo receiverInfo;
        SymbolInfo matchedReceiver;
        receiverInfo.symbolName = alias->m_SymbolName;
        receiverInfo.begin = alias->m_SemLoc.begin;
        receiverInfo.end = alias->m_SemLoc.end;
        receiverInfo.line = alias->m_SemLoc.line;
        auto receiverName = alias->m_SymbolName;
        m_LastSymbolInfo.symbolId = m_SymbolId;
        m_LastSymbolInfo.scopeId = m_ScopeId;
        m_LastSymbolInfo.scopeLevel = m_ScopeLevel;
        if (!symbolValidation(receiverName, receiverInfo, matchedReceiver)) {
          return {};
        }

        if (matchedReceiver.type != TypeSpecifier::SPEC_DEFINED) {
          this->m_TypeErrorMessages.emplace_back(
              "Method call receiver must be a struct value or pointer",
              receiverInfo);
          return {};
        }

        auto methodName =
            matchedReceiver.definedTypeName + "." + member->m_SymbolName;
        if (FunctionTable.count(methodName) == 0) {
          this->m_TypeErrorMessages.emplace_back(
              "Struct '" + matchedReceiver.definedTypeName +
                  "' has no method '" + member->m_SymbolName + "'",
              symbolInfo);
          return {};
        }

        if (member->m_SymbolName == "destructor") {
          this->m_TypeErrorMessages.emplace_back(
              "destructor cannot be called directly; use `drop " +
                  alias->m_SymbolName + ";` for explicit early release",
              symbolInfo, DiagnosticCode::SemanticOwnership);
          return {};
        }

        return methodName;
      }

      auto receiverInfo = getSymbolInfo(alias->m_SymbolName);
      if (receiverInfo.type == TypeSpecifier::SPEC_DEFINED) {
        return receiverInfo.definedTypeName + "." + member->m_SymbolName;
      }

      if (emitDiagnostics) {
        if (emitDiagnostics) {
          this->m_TypeErrorMessages.emplace_back(
              "Module alias '" + alias->m_SymbolName + "' was not declared",
              symbolInfo);
        }
        return {};
      }
    }
  }

  if (emitDiagnostics) {
    this->m_TypeErrorMessages.emplace_back(
        "Function call target must be a symbol or module alias member",
        symbolInfo);
  }
  return {};
}
