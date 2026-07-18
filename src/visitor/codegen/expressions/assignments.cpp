#include <visitor/codegen/codegen_private.hpp>

ValuePtr Visitor::visit(AssignmentAST &assignmentAst) {
  llvm::Value *storage = nullptr;
  SymbolInfo symbolInfo;
  std::string targetName;
  AssignmentTarget target;

  if (assignmentAst.m_LHS->m_ExprKind == ExprKind::SymbolExpr) {
    auto *lhs = static_cast<SymbolAST *>(assignmentAst.m_LHS.get());
    targetName = lhs->m_SymbolName;
    if (m_LocalVarsOnScope.count(lhs->m_SymbolName) > 0) {
      storage = m_LocalVarsOnScope[lhs->m_SymbolName];
    } else if (m_GlobalVars.count(lhs->m_SymbolName) > 0) {
      storage = m_GlobalVars[lhs->m_SymbolName];
    } else {
      assert(false && "Assignment target was not found.");
    }

    symbolInfo = getSymbolInfo(lhs->m_SymbolName);
    auto referenceParam = m_ReferenceParamValueTypes.find(lhs->m_SymbolName);
    if (referenceParam != m_ReferenceParamValueTypes.end() &&
        !assignmentAst.m_IsDereferenced) {
      auto *referenceSlotType = GetPointeeType(storage);
      target.address = CreateLoad(storage, referenceSlotType,
                                  lhs->m_SymbolName + ".ref.addr");
      target.valueType = referenceParam->second;
    } else {
      target = ResolveAssignmentTarget(
          storage, symbolInfo,
          assignmentAst.m_IsDereferenced ? assignmentAst.m_DerefLevel : 0);
    }
  } else if (assignmentAst.m_LHS->m_ExprKind == ExprKind::BinOp) {
    struct FieldAddress {
      llvm::Value *address = nullptr;
      llvm::Type *valueType = nullptr;
      SymbolInfo symbolInfo;
      std::string name;
    };

    auto resolveFieldAddress =
        [&](auto &self, IAST *node) -> FieldAddress {
      if (node == nullptr) {
        assert(false && "Unsupported assignment target.");
      }

      if (node->m_ExprKind == ExprKind::SymbolExpr) {
        auto *symbol = static_cast<SymbolAST *>(node);
        auto info = getSymbolInfo(symbol->m_SymbolName);
        auto *symbolStorage = FindStorage(m_LocalVarsOnScope, m_GlobalVars,
                                          symbol->m_SymbolName);
        if (symbolStorage == nullptr) {
          assert(false && "Struct assignment target storage was not found.");
        }
        if (info.isRef &&
            m_ReferenceParamValueTypes.count(symbol->m_SymbolName) > 0) {
          auto *slotType = GetPointeeType(symbolStorage);
          symbolStorage = CreateLoad(symbolStorage, slotType,
                                     symbol->m_SymbolName + ".ref.field");
        }
        if (info.type == TypeSpecifier::SPEC_DEFINED && !info.isRef &&
            info.indirectionLevel > 0) {
          if (IsSharedPointerSymbol(info)) {
            auto *handle = Builder->CreateLoad(GetSharedPointerTy(),
                                               symbolStorage,
                                               symbol->m_SymbolName + ".sp");
            symbolStorage = ExtractSharedPointerData(handle);
          } else {
            auto *pointerType = GetType(info.type, info.indirectionLevel);
            symbolStorage = Builder->CreateLoad(
                pointerType, symbolStorage, symbol->m_SymbolName + ".ptr");
          }
          info.indirectionLevel = 0;
          info.isUnique = false;
          info.isRef = false;
        }
        return {symbolStorage, GetSymbolLLVMType(info), info,
                symbol->m_SymbolName};
      }

      if (node->m_ExprKind != ExprKind::BinOp) {
        assert(false && "Unsupported assignment target.");
      }

      auto *fieldAccess = static_cast<BinaryOpAST *>(node);
      if (fieldAccess->m_BinOpKind != B_DOT ||
          fieldAccess->m_RHS == nullptr ||
          fieldAccess->m_RHS->m_ExprKind != ExprKind::SymbolExpr) {
        assert(false && "Unsupported assignment target.");
      }

      auto base = self(self, fieldAccess->m_LHS.get());
      auto *fieldSymbol = static_cast<SymbolAST *>(fieldAccess->m_RHS.get());
      auto structIt = StructTable.find(base.symbolInfo.definedTypeName);
      if (structIt == StructTable.end()) {
        assert(false && "Struct type was not registered.");
      }
      auto fieldIt =
          structIt->second.fieldIndexes.find(fieldSymbol->m_SymbolName);
      if (fieldIt == structIt->second.fieldIndexes.end()) {
        assert(false && "Struct field was not registered.");
      }

      auto *structType = GetDefinedStructTy(base.symbolInfo.definedTypeName);
      const auto fieldIndex = fieldIt->second;
      const auto &field = structIt->second.fields[fieldIndex];
      auto fieldName = base.name + "." + field.name;
      auto *fieldAddress = Builder->CreateStructGEP(
          structType, base.address, static_cast<unsigned>(fieldIndex),
          fieldName);
      auto fieldInfo = base.symbolInfo;
      fieldInfo.symbolName = fieldName;
      fieldInfo.type = field.type;
      fieldInfo.definedTypeName = field.definedTypeName;
      fieldInfo.indirectionLevel = field.indirectionLevel;
      fieldInfo.isUnique = field.isUnique;
      fieldInfo.isRef = field.isRef;
      fieldInfo.arrayDimensions = field.arrayDimensions;
      fieldInfo.isSubscriptable = !field.arrayDimensions.empty();
      return {fieldAddress, GetStructFieldLLVMType(field), fieldInfo,
              fieldName};
    };

    auto field = resolveFieldAddress(resolveFieldAddress,
                                     assignmentAst.m_LHS.get());
    targetName = field.name;
    target.address = field.address;
    target.valueType = field.valueType;
    symbolInfo = field.symbolInfo;
  } else {
    assert(false && "Unsupported assignment target.");
  }

  llvm::Type *previousType = m_LastType;
  bool previousSigned = m_LastSigned;
  bool previousVarDecl = m_LastVarDecl;

  if (assignmentAst.m_Subscriptable) {
    if (symbolInfo.isRuntimeSizedArray) {
      auto *spanValue = Builder->CreateLoad(GetSpanTy(), target.address,
                                            targetName + ".span");
      auto *data = ExtractSpanData(spanValue);
      auto *elementType = GetStorageType(symbolInfo.type,
                                         symbolInfo.indirectionLevel,
                                         symbolInfo.isUnique,
                                         false,
                                         symbolInfo.definedTypeName);

      m_LastType = Builder->getInt64Ty();
      m_LastSigned = true;
      m_LastVarDecl = false;

      auto *index = assignmentAst.m_SubscriptIndexes.empty()
                        ? llvm::ConstantInt::get(Builder->getInt64Ty(), 0)
                        : assignmentAst.m_SubscriptIndexes.front()->accept(*this);
      index = CastArrayIndex(index);

      auto *typedData =
          Builder->CreatePointerCast(data, llvm::PointerType::get(
                                              Builder->getContext(), 0));
      target.address = Builder->CreateInBoundsGEP(
          elementType, typedData, index, targetName + ".span.element");
      target.valueType = elementType;
      goto subscript_target_ready;
    }

    llvm::Type *arrayType = target.valueType != nullptr &&
                                    target.valueType->isArrayTy()
                                ? target.valueType
                                : GetPointeeType(target.address);
    auto arrayParamType = m_ArrayParamValueTypes.find(targetName);
    if (arrayParamType != m_ArrayParamValueTypes.end()) {
      auto *slotType = GetPointeeType(target.address);
      target.address =
          CreateLoad(target.address, slotType, targetName + ".array.addr");
      arrayType = arrayParamType->second;
    }
    if (!arrayType->isArrayTy()) {
      assert(false && "Subscript assignment target must be an array.");
    }

    m_LastType = Builder->getInt64Ty();
    m_LastSigned = true;
    m_LastVarDecl = false;

    std::vector<llvm::Value *> indexes;
    indexes.reserve(assignmentAst.m_SubscriptIndexes.size());
    for (auto &indexAst : assignmentAst.m_SubscriptIndexes) {
      auto *index = indexAst->accept(*this);
      indexes.push_back(CastArrayIndex(index));
    }

    std::vector<llvm::Value *> idxList = {
        llvm::ConstantInt::get(Builder->getInt64Ty(), 0),
        CreateLinearArrayIndex(indexes, symbolInfo.arrayDimensions)};
    target.address =
        Builder->CreateInBoundsGEP(arrayType, target.address, idxList,
                                   targetName + ".element");
    target.valueType = arrayType->getArrayElementType();
  }

subscript_target_ready:
  m_LastType = target.valueType;
  m_LastSigned = IsSigned(symbolInfo.type);
  m_LastVarDecl = false;

  const bool targetIsShared = IsSharedPointerTy(target.valueType);
  auto *rhsSymbol = symbolFromMoveSource(assignmentAst.m_RHS.get());
  auto *rhsUnary = dynamic_cast<UnaryOpAST *>(assignmentAst.m_RHS.get());
  const bool rhsIsMove =
      rhsUnary != nullptr && rhsUnary->m_UnaryOpKind == U_MOVE;
  if (targetIsShared) {
    auto *oldHandle =
        Visitor::Builder->CreateLoad(GetSharedPointerTy(), target.address,
                                     targetName + ".old.sp");
    ReleaseSharedPointer(oldHandle);
  }

  llvm::Value *rhs = assignmentAst.m_RHS->accept(*this);
  rhs = CastValueToType(rhs, target.valueType, m_LastSigned);
  if (targetIsShared && rhsSymbol != nullptr &&
      assignmentAst.m_ShortcutOp != ShortcutOp::S_MOV && !rhsIsMove) {
    RetainSharedPointer(rhs);
  }

  llvm::Value *result = rhs;
  if (assignmentAst.m_ShortcutOp != ShortcutOp::S_NONE &&
      assignmentAst.m_ShortcutOp != ShortcutOp::S_MOV) {
    llvm::Value *currentValue = Visitor::Builder->CreateLoad(
        target.valueType, target.address, targetName);

    result = CreateShortcutAssignmentValue(currentValue, rhs, target.valueType,
                                           assignmentAst.m_ShortcutOp,
                                           m_LastSigned);
    result = CastValueToType(result, target.valueType, m_LastSigned);
  }

  Visitor::Builder->CreateStore(result, target.address);
  if (targetIsShared && rhsSymbol != nullptr &&
      (assignmentAst.m_ShortcutOp == ShortcutOp::S_MOV || rhsIsMove)) {
    auto *sourceStorage = FindStorage(m_LocalVarsOnScope, m_GlobalVars,
                                      rhsSymbol->m_SymbolName);
    Builder->CreateStore(CreateNullSharedPointerHandle(), sourceStorage);
  }

  m_LastType = previousType;
  m_LastSigned = previousSigned;
  m_LastVarDecl = previousVarDecl;

  return result;
}
