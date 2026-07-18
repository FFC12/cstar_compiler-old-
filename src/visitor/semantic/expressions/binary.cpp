#include <visitor/semantic/semantic_private.hpp>

SymbolInfo Visitor::preVisit(BinaryOpAST &binaryOpAst) {
  SymbolInfo symbolInfo;
  if (this->m_TypeChecking) {
    ASTNode &lhs = binaryOpAst.m_LHS, &rhs = binaryOpAst.m_RHS;

    if (binaryOpAst.m_BinOpKind == B_DOT) {
      SymbolInfo accessInfo;
      accessInfo.begin = binaryOpAst.m_SemLoc.begin;
      accessInfo.end = binaryOpAst.m_SemLoc.end;
      accessInfo.line = binaryOpAst.m_SemLoc.line;

      if (lhs != nullptr && rhs != nullptr &&
          lhs->m_ExprKind == ExprKind::SymbolExpr &&
          rhs->m_ExprKind == ExprKind::SymbolExpr) {
        auto *enumSymbol = static_cast<SymbolAST *>(lhs.get());
        auto *memberSymbol = static_cast<SymbolAST *>(rhs.get());
        if (Visitor::EnumTable.count(enumSymbol->m_SymbolName) != 0) {
          auto *member = SemFindEnumMember(enumSymbol->m_SymbolName,
                                        memberSymbol->m_SymbolName);
          if (member == nullptr) {
            this->m_TypeErrorMessages.emplace_back(
                "Enum '" + enumSymbol->m_SymbolName + "' has no member '" +
                    memberSymbol->m_SymbolName + "'",
                accessInfo);
            return accessInfo;
          }

          if (m_DefinedTypeFlag && !m_DefinedTypeName.empty() &&
              m_DefinedTypeName != enumSymbol->m_SymbolName) {
            this->m_TypeErrorMessages.emplace_back(
                "Enum value '" + enumSymbol->m_SymbolName + "." +
                    memberSymbol->m_SymbolName +
                    "' is incompatible with enum type '" +
                    m_DefinedTypeName + "'",
                accessInfo);
          }
          accessInfo.type = TypeSpecifier::SPEC_DEFINED;
          accessInfo.definedTypeName = enumSymbol->m_SymbolName;
          accessInfo.symbolName = enumSymbol->m_SymbolName + "." +
                                  memberSymbol->m_SymbolName;
          accessInfo.value = std::to_string(member->value);
          return accessInfo;
        }
      }

      auto resolveFieldAccess =
          [&](auto &self, IAST *node) -> SymbolInfo {
        SymbolInfo resolved;
        resolved.begin = accessInfo.begin;
        resolved.end = accessInfo.end;
        resolved.line = accessInfo.line;

        if (node == nullptr) {
          this->m_TypeErrorMessages.emplace_back(
              "Struct field access expects `value.field`", accessInfo);
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
          if (m_DroppedSemanticSymbols.count(SymbolStateKey(resolved)) > 0) {
            this->m_TypeErrorMessages.emplace_back(
                "value '" + resolved.symbolName +
                    "' was dropped and cannot be used before being "
                    "reinitialized",
                lookup, DiagnosticCode::SemanticOwnership);
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
              "Struct field access expects `value.field`", accessInfo);
          return resolved;
        }

        auto *fieldAccess = static_cast<BinaryOpAST *>(node);
        if (fieldAccess->m_BinOpKind != B_DOT ||
            fieldAccess->m_RHS == nullptr ||
            fieldAccess->m_RHS->m_ExprKind != ExprKind::SymbolExpr) {
          this->m_TypeErrorMessages.emplace_back(
              "Struct field access expects `value.field`", accessInfo);
          return resolved;
        }

        auto base = self(self, fieldAccess->m_LHS.get());
        auto *fieldSymbol = static_cast<SymbolAST *>(fieldAccess->m_RHS.get());
        if (base.symbolName.empty() && base.type == TypeSpecifier::SPEC_VOID) {
          return resolved;
        }
        if (base.type != TypeSpecifier::SPEC_DEFINED) {
          this->m_TypeErrorMessages.emplace_back(
              "Field access requires a struct value", base);
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
              accessInfo);
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
        resolved.begin = fieldSymbol->m_SemLoc.begin;
        resolved.end = fieldSymbol->m_SemLoc.end;
        resolved.line = fieldSymbol->m_SemLoc.line;
        return resolved;
      };

      return resolveFieldAccess(resolveFieldAccess, &binaryOpAst);
    }

    if (binaryOpAst.m_BinOpKind == B_TER) {
      symbolInfo.begin = binaryOpAst.m_SemLoc.begin;
      symbolInfo.end = binaryOpAst.m_SemLoc.end;
      symbolInfo.line = binaryOpAst.m_SemLoc.line;

      if (lhs == nullptr || rhs == nullptr || binaryOpAst.m_Extra == nullptr) {
        this->m_TypeErrorMessages.emplace_back(
            "Ternary expression requires condition, true branch and false "
            "branch",
            symbolInfo);
        return symbolInfo;
      }

      auto containsSelectSideEffect =
          [&](auto &self, IAST *node) -> bool {
        if (node == nullptr || ContainsTernarySelectSideEffect(node)) {
          return node != nullptr;
        }

        if (node->getExprKind() == ExprKind::BinOp) {
          auto *binary = static_cast<BinaryOpAST *>(node);
          return self(self, binary->m_LHS.get()) ||
                 self(self, binary->m_RHS.get()) ||
                 self(self, binary->m_Extra.get());
        }

        if (node->getExprKind() == ExprKind::UnaryOp) {
          auto *unary = static_cast<UnaryOpAST *>(node);
          return self(self, unary->m_Node.get());
        }

        if (node->getExprKind() == ExprKind::CastExpr) {
          auto *cast = static_cast<CastOpAST *>(node);
          return self(self, cast->m_Node.get());
        }

        return false;
      };

      auto visitCondition = [&]() {
        const auto previousExpectedType = m_ExpectedType;
        const auto previousLastSymbolInfo = m_LastSymbolInfo;
        const auto previousDefinedTypeFlag = m_DefinedTypeFlag;
        const auto previousDefinedTypeName = m_DefinedTypeName;
        const auto previousLastCondExpr = m_LastCondExpr;

        m_LastCondExpr = true;
        m_ExpectedType = TypeSpecifier::SPEC_BOOL;
        m_LastSymbolInfo.type = TypeSpecifier::SPEC_BOOL;
        m_LastSymbolInfo.indirectionLevel = 0;
        m_LastSymbolInfo.isRef = false;
        m_DefinedTypeFlag = false;
        m_DefinedTypeName.clear();
        lhs->acceptBefore(*this);

        m_ExpectedType = previousExpectedType;
        m_LastSymbolInfo = previousLastSymbolInfo;
        m_DefinedTypeFlag = previousDefinedTypeFlag;
        m_DefinedTypeName = previousDefinedTypeName;
        m_LastCondExpr = previousLastCondExpr;
      };

      auto visitBranch = [&](IAST *node) -> SymbolInfo {
        const auto previousExpectedType = m_ExpectedType;
        const auto previousLastSymbolInfo = m_LastSymbolInfo;
        const auto previousDefinedTypeFlag = m_DefinedTypeFlag;
        const auto previousDefinedTypeName = m_DefinedTypeName;
        const auto previousLastBinOp = m_LastBinOp;
        const auto previousLastBinOpHasPtr = m_LastBinOpHasAtLeastOnePtr;

        m_LastBinOp = true;
        m_LastBinOpHasAtLeastOnePtr = false;
        SymbolInfo branchInfo = node->acceptBefore(*this);

        if (auto *scalar = dynamic_cast<ScalarOrLiteralAST *>(node)) {
          branchInfo = InferScalarLiteralType(scalar);
        } else if (IsVoidValue(branchInfo) &&
                   node->getExprKind() == ExprKind::BinOp &&
                   m_LastSymbolInfo.type != TypeSpecifier::SPEC_VOID) {
          branchInfo = m_LastSymbolInfo;
        }

        m_ExpectedType = previousExpectedType;
        m_LastSymbolInfo = previousLastSymbolInfo;
        m_DefinedTypeFlag = previousDefinedTypeFlag;
        m_DefinedTypeName = previousDefinedTypeName;
        m_LastBinOp = previousLastBinOp;
        m_LastBinOpHasAtLeastOnePtr = previousLastBinOpHasPtr;
        return branchInfo;
      };

      auto branchFitsTarget = [](const SymbolInfo &branch,
                                 const SymbolInfo &target) {
        if (IsVoidValue(branch)) {
          return false;
        }

        if (IsPointerLike(target)) {
          return branch.type == target.type &&
                 branch.definedTypeName == target.definedTypeName &&
                 branch.indirectionLevel == target.indirectionLevel &&
                 branch.isUnique == target.isUnique &&
                 branch.isRef == target.isRef &&
                 SameQualifierShape(branch, target);
        }

        if (target.type == TypeSpecifier::SPEC_BOOL) {
          return branch.type == TypeSpecifier::SPEC_BOOL &&
                 branch.indirectionLevel == 0;
        }

        if (IsNumericPrimitiveType(target.type)) {
          return IsNumericPrimitiveType(branch.type) &&
                 branch.type != TypeSpecifier::SPEC_BOOL &&
                 branch.indirectionLevel == 0;
        }

        if (target.type == TypeSpecifier::SPEC_DEFINED) {
          return branch.type == TypeSpecifier::SPEC_DEFINED &&
                 branch.definedTypeName == target.definedTypeName &&
                 branch.indirectionLevel == target.indirectionLevel;
        }

        return SameTypeShape(branch, target);
      };

      auto branchesAreCompatible = [](const SymbolInfo &trueInfo,
                                      const SymbolInfo &falseInfo) {
        if (IsVoidValue(trueInfo) || IsVoidValue(falseInfo)) {
          return false;
        }

        if (SameTypeShape(trueInfo, falseInfo)) {
          return true;
        }

        const bool bothNumeric =
            IsNumericPrimitiveType(trueInfo.type) &&
            IsNumericPrimitiveType(falseInfo.type) &&
            trueInfo.type != TypeSpecifier::SPEC_BOOL &&
            falseInfo.type != TypeSpecifier::SPEC_BOOL &&
            trueInfo.indirectionLevel == 0 && falseInfo.indirectionLevel == 0;
        if (bothNumeric) {
          return true;
        }

        return false;
      };

      visitCondition();

      if (containsSelectSideEffect(containsSelectSideEffect, rhs.get()) ||
          containsSelectSideEffect(containsSelectSideEffect,
                                   binaryOpAst.m_Extra.get())) {
        this->m_TypeErrorMessages.emplace_back(
            "Ternary branch expressions must be side-effect-free in the "
            "current MVP; use if/else for calls, allocation or assignment",
            symbolInfo);
      }

      auto trueInfo = visitBranch(rhs.get());
      auto falseInfo = visitBranch(binaryOpAst.m_Extra.get());

      if (IsVoidValue(trueInfo) || IsVoidValue(falseInfo)) {
        this->m_TypeErrorMessages.emplace_back(
            "Ternary branches must produce a value", symbolInfo);
      } else if (m_LastSymbolInfo.type != TypeSpecifier::SPEC_VOID ||
                 IsPointerLike(m_LastSymbolInfo)) {
        if (!branchFitsTarget(trueInfo, m_LastSymbolInfo) ||
            !branchFitsTarget(falseInfo, m_LastSymbolInfo)) {
          this->m_TypeErrorMessages.emplace_back(
              "Ternary branch types are incompatible with the expected "
              "expression type",
              symbolInfo);
        }
        symbolInfo = m_LastSymbolInfo;
      } else if (!branchesAreCompatible(trueInfo, falseInfo)) {
        this->m_TypeErrorMessages.emplace_back(
            "Ternary branches must have compatible value types", symbolInfo);
      } else {
        symbolInfo = trueInfo;
      }

      symbolInfo.begin = binaryOpAst.m_SemLoc.begin;
      symbolInfo.end = binaryOpAst.m_SemLoc.end;
      symbolInfo.line = binaryOpAst.m_SemLoc.line;
      return symbolInfo;
    }

    auto resolveOperandInfo = [&](IAST *node) -> SymbolInfo {
      SymbolInfo resolved;
      if (node == nullptr) {
        return resolved;
      }

      if (auto *scalar = dynamic_cast<ScalarOrLiteralAST *>(node)) {
        return InferScalarLiteralType(scalar);
      }

      if (auto *symbol = dynamic_cast<SymbolAST *>(node)) {
        const auto previousExpectedType = m_ExpectedType;
        const auto previousLastSymbolInfo = m_LastSymbolInfo;
        const auto previousDefinedTypeFlag = m_DefinedTypeFlag;
        const auto previousDefinedTypeName = m_DefinedTypeName;
        const auto previousLastCondExpr = m_LastCondExpr;

        m_LastCondExpr = true;
        m_ExpectedType = TypeSpecifier::SPEC_BOOL;
        m_LastSymbolInfo.type = TypeSpecifier::SPEC_BOOL;
        m_LastSymbolInfo.indirectionLevel = 0;
        m_LastSymbolInfo.isRef = false;
        m_DefinedTypeFlag = false;
        m_DefinedTypeName.clear();

        resolved = symbol->acceptBefore(*this);

        m_ExpectedType = previousExpectedType;
        m_LastSymbolInfo = previousLastSymbolInfo;
        m_DefinedTypeFlag = previousDefinedTypeFlag;
        m_DefinedTypeName = previousDefinedTypeName;
        m_LastCondExpr = previousLastCondExpr;
        return resolved;
      }

      const auto previousExpectedType = m_ExpectedType;
      const auto previousLastSymbolInfo = m_LastSymbolInfo;
      const auto previousDefinedTypeFlag = m_DefinedTypeFlag;
      const auto previousDefinedTypeName = m_DefinedTypeName;
      const auto previousLastBinOp = m_LastBinOp;
      const auto previousLastBinOpHasPtr = m_LastBinOpHasAtLeastOnePtr;
      const auto previousLastCondExpr = m_LastCondExpr;

      m_LastBinOp = true;
      m_LastCondExpr = false;
      m_LastBinOpHasAtLeastOnePtr = false;
      m_ExpectedType = TypeSpecifier::SPEC_I64;
      m_LastSymbolInfo.type = TypeSpecifier::SPEC_I64;
      m_LastSymbolInfo.indirectionLevel = 0;
      m_LastSymbolInfo.isRef = false;
      m_LastSymbolInfo.isUnique = false;
      m_DefinedTypeFlag = false;
      m_DefinedTypeName.clear();

      resolved = node->acceptBefore(*this);
      if (auto *scalar = dynamic_cast<ScalarOrLiteralAST *>(node)) {
        resolved = InferScalarLiteralType(scalar);
      }

      m_ExpectedType = previousExpectedType;
      m_LastSymbolInfo = previousLastSymbolInfo;
      m_DefinedTypeFlag = previousDefinedTypeFlag;
      m_DefinedTypeName = previousDefinedTypeName;
      m_LastBinOp = previousLastBinOp;
      m_LastBinOpHasAtLeastOnePtr = previousLastBinOpHasPtr;
      m_LastCondExpr = previousLastCondExpr;
      return resolved;
    };

    if (IsLogicalOperator(binaryOpAst.m_BinOpKind)) {
      auto visitConditionOperand = [&](IAST *node) {
        const auto previousExpectedType = m_ExpectedType;
        const auto previousLastSymbolInfo = m_LastSymbolInfo;
        const auto previousDefinedTypeFlag = m_DefinedTypeFlag;
        const auto previousDefinedTypeName = m_DefinedTypeName;
        const auto previousLastCondExpr = m_LastCondExpr;

        m_LastCondExpr = true;
        m_ExpectedType = TypeSpecifier::SPEC_BOOL;
        m_LastSymbolInfo.type = TypeSpecifier::SPEC_BOOL;
        m_LastSymbolInfo.indirectionLevel = 0;
        m_LastSymbolInfo.isRef = false;
        m_DefinedTypeFlag = false;
        m_DefinedTypeName.clear();

        if (node != nullptr) {
          node->acceptBefore(*this);
        }

        m_ExpectedType = previousExpectedType;
        m_LastSymbolInfo = previousLastSymbolInfo;
        m_DefinedTypeFlag = previousDefinedTypeFlag;
        m_DefinedTypeName = previousDefinedTypeName;
        m_LastCondExpr = previousLastCondExpr;
      };

      visitConditionOperand(lhs.get());
      visitConditionOperand(rhs.get());
      return MakeBoolInfo(binaryOpAst.m_SemLoc);
    }

    if (IsComparisonOperator(binaryOpAst.m_BinOpKind)) {
      auto lhsSymbol = resolveOperandInfo(lhs.get());
      auto rhsSymbol = resolveOperandInfo(rhs.get());

      if (lhsSymbol.type == TypeSpecifier::SPEC_DEFINED &&
          lhsSymbol.indirectionLevel == 0) {
        const auto overloadMethod =
            SemValueOperatorMethodName(binaryOpAst.m_BinOpKind);
        const auto functionName =
            lhsSymbol.definedTypeName + "." + overloadMethod;
        auto signatureIt = FunctionTable.find(functionName);
        if (signatureIt != FunctionTable.end()) {
          const auto &signature = signatureIt->second;
          if (signature.params.size() != 2) {
            this->m_TypeErrorMessages.emplace_back(
                "Value operator '" + functionName +
                    "' must have exactly one explicit parameter",
                lhsSymbol);
          } else {
            const auto &rhsParam = signature.params[1];
            if (rhsParam.type != rhsSymbol.type ||
                rhsParam.definedTypeName != rhsSymbol.definedTypeName ||
                rhsParam.indirectionLevel != rhsSymbol.indirectionLevel) {
              this->m_TypeErrorMessages.emplace_back(
                  "Right operand does not match value operator parameter",
                  rhsSymbol);
            }
          }

          if (signature.returnType.type != TypeSpecifier::SPEC_BOOL ||
              signature.returnType.indirectionLevel != 0) {
            this->m_TypeErrorMessages.emplace_back(
                "Comparison value operators must return bool", lhsSymbol);
          }

          return MakeBoolInfo(binaryOpAst.m_SemLoc);
        }
      }

      const bool lhsIsPointer = IsPointerLike(lhsSymbol);
      const bool rhsIsPointer = IsPointerLike(rhsSymbol);
      const bool lhsIsEnum =
          lhsSymbol.type == TypeSpecifier::SPEC_DEFINED &&
          EnumTable.count(lhsSymbol.definedTypeName) != 0;
      const bool rhsIsEnum =
          rhsSymbol.type == TypeSpecifier::SPEC_DEFINED &&
          EnumTable.count(rhsSymbol.definedTypeName) != 0;

      if (lhsIsPointer || rhsIsPointer) {
        if (IsOrderedComparisonOperator(binaryOpAst.m_BinOpKind)) {
          this->m_TypeErrorMessages.emplace_back(
              "Pointer values only support equality comparisons",
              lhsIsPointer ? lhsSymbol : rhsSymbol);
        } else if (!lhsIsPointer || !rhsIsPointer ||
                   lhsSymbol.type != rhsSymbol.type ||
                   lhsSymbol.definedTypeName != rhsSymbol.definedTypeName ||
                   lhsSymbol.indirectionLevel != rhsSymbol.indirectionLevel ||
                   lhsSymbol.isRef != rhsSymbol.isRef ||
                   lhsSymbol.isUnique != rhsSymbol.isUnique) {
          this->m_TypeErrorMessages.emplace_back(
              "Pointer comparison requires matching pointer types",
              lhsIsPointer ? rhsSymbol : lhsSymbol);
        }
      } else if (lhsIsEnum || rhsIsEnum) {
        if (!lhsIsEnum || !rhsIsEnum ||
            lhsSymbol.definedTypeName != rhsSymbol.definedTypeName) {
          this->m_TypeErrorMessages.emplace_back(
              "Enum comparison requires matching enum operands",
              lhsIsEnum ? rhsSymbol : lhsSymbol);
        } else if (IsOrderedComparisonOperator(binaryOpAst.m_BinOpKind)) {
          this->m_TypeErrorMessages.emplace_back(
              "Enum values only support equality comparisons", lhsSymbol);
        }
      } else {
        const bool lhsNumeric = IsNumericPrimitiveType(lhsSymbol.type) ||
                                lhsSymbol.type == TypeSpecifier::SPEC_BOOL;
        const bool rhsNumeric = IsNumericPrimitiveType(rhsSymbol.type) ||
                                rhsSymbol.type == TypeSpecifier::SPEC_BOOL;
        if (!lhsNumeric || !rhsNumeric) {
          this->m_TypeErrorMessages.emplace_back(
              "Comparison operands must be numeric, bool, enum or matching "
              "pointer values",
              !lhsNumeric ? lhsSymbol : rhsSymbol);
        } else if (IsOrderedComparisonOperator(binaryOpAst.m_BinOpKind) &&
                   (lhsSymbol.type == TypeSpecifier::SPEC_BOOL ||
                    rhsSymbol.type == TypeSpecifier::SPEC_BOOL)) {
          this->m_TypeErrorMessages.emplace_back(
              "Ordered comparisons require numeric operands, not bool",
              lhsSymbol.type == TypeSpecifier::SPEC_BOOL ? lhsSymbol
                                                         : rhsSymbol);
        }
      }

      return MakeBoolInfo(binaryOpAst.m_SemLoc);
    }

    const auto overloadMethod = SemValueOperatorMethodName(binaryOpAst.m_BinOpKind);
    if (!overloadMethod.empty()) {
      auto previousExpectedType = m_ExpectedType;
      auto previousLastSymbolInfo = m_LastSymbolInfo;
      auto previousDefinedTypeFlag = m_DefinedTypeFlag;
      auto previousLastBinOpHasPtr = m_LastBinOpHasAtLeastOnePtr;

      auto lhsSymbol = lhs->acceptBefore(*this);
      auto rhsSymbol = rhs->acceptBefore(*this);

      m_ExpectedType = previousExpectedType;
      m_LastSymbolInfo = previousLastSymbolInfo;
      m_DefinedTypeFlag = previousDefinedTypeFlag;
      m_LastBinOpHasAtLeastOnePtr = previousLastBinOpHasPtr;

      if (lhsSymbol.type == TypeSpecifier::SPEC_DEFINED &&
          lhsSymbol.indirectionLevel == 0) {
        const auto functionName =
            lhsSymbol.definedTypeName + "." + overloadMethod;
        auto signatureIt = FunctionTable.find(functionName);
        if (signatureIt != FunctionTable.end()) {
          const auto &signature = signatureIt->second;
          if (signature.params.size() != 2) {
            this->m_TypeErrorMessages.emplace_back(
                "Value operator '" + functionName +
                    "' must have exactly one explicit parameter",
                lhsSymbol);
          } else {
            const auto &rhsParam = signature.params[1];
            if (rhsParam.type != rhsSymbol.type ||
                rhsParam.definedTypeName != rhsSymbol.definedTypeName ||
                rhsParam.indirectionLevel != rhsSymbol.indirectionLevel) {
              this->m_TypeErrorMessages.emplace_back(
                  "Right operand does not match value operator parameter",
                  rhsSymbol);
            }
          }
          return signature.returnType;
        }
      }
    }

    bool isPtrType = false;
    bool errorFlag = false;

    if (this->m_LastSymbolInfo.indirectionLevel != 0) {
      isPtrType = true;
    }

    m_LastBinOp = true;

    SymbolInfo rhsSymbol;
    if (binaryOpAst.m_BinOpKind == B_MARRS) {
      rhsSymbol.begin = binaryOpAst.m_SemLoc.begin;
      rhsSymbol.end = binaryOpAst.m_SemLoc.end;
      rhsSymbol.line = binaryOpAst.m_SemLoc.line;
      return rhsSymbol;
    } else {
      size_t indexCount = 0;
      size_t hasIndexes = false;
      if (binaryOpAst.m_BinOpKind == B_ARRS) {
        indexCount = this->getIndexesOfArray(*binaryOpAst.m_RHS.get());
        hasIndexes = true;
      } else {
        this->m_BinOpTermCount += 1;
        rhsSymbol = rhs->acceptBefore(*this);
        this->m_BinOpTermCount -= 1;
      }

      if (isPtrType) {
        if (!rhsSymbol.typeCheckerInfo.isCompatiblePtr &&
            (this->m_LastBinOp && !this->m_LastBinOpHasAtLeastOnePtr) &&
            this->m_BinOpTermCount == 0) {
          this->m_TypeErrorMessages.emplace_back(
              "Invalid operand '" + rhsSymbol.symbolName + "'" +
                  " of binary operation",
              rhsSymbol);
          rhsSymbol.typeCheckerInfo.isCompatiblePtr = true;
          errorFlag = true;
        }
      }

      // Array index op...
      if (this->m_LastBinOp && binaryOpAst.m_BinOpKind == B_ARRS) {
        SymbolInfo matchedSymbol;
        auto symbolName = ((SymbolAST *)binaryOpAst.m_LHS.get())->m_SymbolName;

        size_t arraySize = m_LastVarDecl
                               ? matchedSymbol.arrayDimensions.size()
                               : getIndexesOfArray(*binaryOpAst.m_RHS.get());

        if (symbolValidation(symbolName, rhsSymbol, matchedSymbol)) {
          if (matchedSymbol.isRuntimeSizedArray) {
            if (indexCount != 1) {
              this->m_TypeErrorMessages.emplace_back(
                  "Runtime-sized array view `T[]` expects exactly one index",
                  rhsSymbol);
            }

            auto elementInfo = matchedSymbol;
            elementInfo.begin = binaryOpAst.m_SemLoc.begin;
            elementInfo.end = binaryOpAst.m_SemLoc.end;
            elementInfo.line = binaryOpAst.m_SemLoc.line;
            elementInfo.isSubscriptable = false;
            elementInfo.isRuntimeSizedArray = false;
            elementInfo.arrayDimensions.clear();
            m_LastArrayIndexCount = 0;
            m_LastSubscriptable = false;
            return elementInfo;
          }

          if (matchedSymbol.isSubscriptable && indexCount <= arraySize) {
            m_LastArrayIndexCount = indexCount;
            m_LastSubscriptable = true;
            std::vector<IAST *> indexes;
            auto collectArrayIndexLiterals =
                [&](auto &self, IAST *expr,
                    std::vector<IAST *> &indexes) -> void {
              if (expr == nullptr) {
                return;
              }

              if (expr->m_ExprKind != ExprKind::BinOp) {
                indexes.push_back(expr);
                return;
              }

              auto *binaryExpr = static_cast<BinaryOpAST *>(expr);
              if (binaryExpr->m_BinOpKind != B_MARRS) {
                indexes.push_back(expr);
                return;
              }

              self(self, binaryExpr->m_LHS.get(), indexes);
              self(self, binaryExpr->m_RHS.get(), indexes);
            };
            collectArrayIndexLiterals(collectArrayIndexLiterals,
                                      binaryOpAst.m_RHS.get(), indexes);
            const size_t checkCount =
                std::min(indexes.size(), matchedSymbol.arrayDimensions.size());
            for (size_t i = 0; i < checkCount; ++i) {
              if (indexes[i] == nullptr) {
                continue;
              }

              int64_t indexVal = 0;
              if (!tryGetConstantIntegerLiteral(indexes[i], indexVal)) {
                continue;
              }
              if (IsConstantArrayIndexOutOfBounds(
                      indexVal, matchedSymbol.arrayDimensions[i])) {
                SymbolInfo indexInfo;
                indexInfo.begin = indexes[i]->getSemLoc().begin;
                indexInfo.end = indexes[i]->getSemLoc().end;
                indexInfo.line = indexes[i]->getSemLoc().line;
                m_TypeErrorMessages.emplace_back(
                    "Array index '" + std::to_string(indexVal) +
                        "' is out of bounds for dimension " +
                        std::to_string(i) + " with size " +
                        std::to_string(matchedSymbol.arrayDimensions[i]),
                    indexInfo);
              }
            }

            auto elementInfo = matchedSymbol;
            elementInfo.begin = binaryOpAst.m_SemLoc.begin;
            elementInfo.end = binaryOpAst.m_SemLoc.end;
            elementInfo.line = binaryOpAst.m_SemLoc.line;
            elementInfo.indirectionLevel = 0;
            elementInfo.isSubscriptable = false;
            elementInfo.arrayDimensions.clear();
            if (indexCount < matchedSymbol.arrayDimensions.size()) {
              elementInfo.isSubscriptable = true;
              elementInfo.arrayDimensions.assign(
                  matchedSymbol.arrayDimensions.begin() + indexCount,
                  matchedSymbol.arrayDimensions.end());
              elementInfo.indirectionLevel = elementInfo.arrayDimensions.size();
            }
            m_LastArrayIndexCount = 0;
            m_LastSubscriptable = false;
            return elementInfo;
          } else {
            if (indexCount > matchedSymbol.arrayDimensions.size()) {
              if (m_LastVarDecl) {
                this->m_TypeErrorMessages.emplace_back(
                    "Invalid array index(es)", m_LastSymbolInfo);
              } else {
                this->m_TypeErrorMessages.emplace_back(
                    "Invalid array index(es)", rhsSymbol);
              }
            }
          }
        }
      }

      this->m_BinOpTermCount += 1;
      auto lhsSymbol = lhs->acceptBefore(*this);
      this->m_BinOpTermCount -= 1;

      const bool lhsIsEnum =
          lhsSymbol.type == TypeSpecifier::SPEC_DEFINED &&
          EnumTable.count(lhsSymbol.definedTypeName) != 0;
      const bool rhsIsEnum =
          rhsSymbol.type == TypeSpecifier::SPEC_DEFINED &&
          EnumTable.count(rhsSymbol.definedTypeName) != 0;
      if (lhsIsEnum || rhsIsEnum) {
        if (!lhsIsEnum || !rhsIsEnum ||
            lhsSymbol.definedTypeName != rhsSymbol.definedTypeName ||
            lhsSymbol.indirectionLevel != 0 || rhsSymbol.indirectionLevel != 0) {
          this->m_TypeErrorMessages.emplace_back(
              "Enum binary operation requires matching enum operands",
              lhsIsEnum ? rhsSymbol : lhsSymbol);
        } else {
          const auto &enumInfo = EnumTable[lhsSymbol.definedTypeName];
          if (IsEnumBitwiseOperator(binaryOpAst.m_BinOpKind)) {
            if (!enumInfo.isFlags) {
              this->m_TypeErrorMessages.emplace_back(
                  "Bitwise enum operators require a 'flags enum'; enum '" +
                      enumInfo.name + "' is scalar",
                  lhsSymbol);
            }
          } else if (!IsEnumEqualityOperator(binaryOpAst.m_BinOpKind)) {
            this->m_TypeErrorMessages.emplace_back(
                "Enum values only support equality; flags enums also support "
                "bitwise &, | and ^",
                lhsSymbol);
          }
        }
      }

      if (isPtrType && !errorFlag) {
        if (!lhsSymbol.typeCheckerInfo.isCompatiblePtr &&
            (this->m_LastBinOp && !this->m_LastBinOpHasAtLeastOnePtr) &&
            this->m_BinOpTermCount == 0) {
          this->m_TypeErrorMessages.emplace_back(
              "Invalid operand '" + lhsSymbol.symbolName + "'" +
                  " of binary operation",
              lhsSymbol);
          lhsSymbol.typeCheckerInfo.isCompatiblePtr = true;
        } else if (!lhsSymbol.typeCheckerInfo.isCompatibleSubs &&
                   (this->m_LastBinOp && !this->m_LastBinOpHasAtLeastOnePtr) &&
                   this->m_BinOpTermCount == 0) {
          this->m_TypeErrorMessages.emplace_back(
              "Invalid operand '" + lhsSymbol.symbolName + "'" +
                  " of binary operation",
              lhsSymbol);
        }
      }

      if (!lhsSymbol.typeCheckerInfo.isCompatibleSubs) {
        this->m_TypeErrorMessages.emplace_back("Invalid array index(es)",
                                               symbolInfo);
      } else if (!lhsSymbol.typeCheckerInfo.isCompatibleSubsForBinOp) {
        this->m_TypeErrorMessages.emplace_back(
            "Invalid array index(es). Subscripted value is not array",
            lhsSymbol);
        this->m_TypeWarningMessages.emplace_back(
            "You exceeded to the maximum dimension count of the array",
            lhsSymbol);
      }

      if (IsValueOperator(binaryOpAst.m_BinOpKind)) {
        if (!IsVoidValue(lhsSymbol)) {
          symbolInfo = lhsSymbol;
        } else {
          symbolInfo = rhsSymbol;
        }
        symbolInfo.begin = binaryOpAst.m_SemLoc.begin;
        symbolInfo.end = binaryOpAst.m_SemLoc.end;
        symbolInfo.line = binaryOpAst.m_SemLoc.line;
      }

      this->m_LastBinOp = false;
      this->m_LastArrayIndexCount = 0;
      this->m_LastSubscriptable = false;
    }
  } else {
    // nothing to do
  }

  return symbolInfo;
}
