#include <visitor/codegen/codegen_private.hpp>

ValuePtr Visitor::visit(UnaryOpAST &unaryOpAst) {
  llvm::Value *value = nullptr;

  if (unaryOpAst.m_UnaryOpKind == UnaryOpKind::U_NEGATIVE) {
    value = unaryOpAst.m_Node->accept(*this);
    if (value == nullptr) {
      return nullptr;
    }
    if (value->getType()->isFloatingPointTy()) {
      value = Builder->CreateFNeg(value, "fnegtmp");
    } else if (value->getType()->isIntegerTy()) {
      value = Builder->CreateNeg(value, "negtmp");
    } else {
      assert(false && "Unary '-' requires a numeric value.");
    }
    m_LastType = value->getType();
    return value;
  }

  if (unaryOpAst.m_UnaryOpKind == UnaryOpKind::U_POSITIVE) {
    value = unaryOpAst.m_Node->accept(*this);
    if (value == nullptr) {
      return nullptr;
    }
    if (!value->getType()->isIntegerTy() &&
        !value->getType()->isFloatingPointTy()) {
      assert(false && "Unary '+' requires a numeric value.");
    }
    m_LastType = value->getType();
    return value;
  }

  if (unaryOpAst.m_UnaryOpKind == UnaryOpKind::U_NOT) {
    auto *targetType = m_LastType;
    value = unaryOpAst.m_Node->accept(*this);
    if (value == nullptr) {
      return nullptr;
    }

    value = CastValueToBranchCondition(value, m_LastSigned);
    value = Builder->CreateNot(value, "nottmp");
    if (targetType != nullptr && targetType != value->getType()) {
      value = CastValueToType(value, targetType, false);
    }
    m_LastType = value->getType();
    m_LastSigned = false;
    return value;
  }

  if (unaryOpAst.m_UnaryOpKind == UnaryOpKind::U_MOVE) {
    return unaryOpAst.m_Node->accept(*this);
  }

  if (unaryOpAst.m_UnaryOpKind == UnaryOpKind::U_BINNEG) {
    value = unaryOpAst.m_Node->accept(*this);
    if (value == nullptr) {
      return nullptr;
    }
    if (!value->getType()->isIntegerTy()) {
      assert(false && "Bitwise '~' requires an integer-like value.");
    }
    value = Builder->CreateNot(value, "bnot");
    m_LastType = value->getType();
    return value;
  }

  if (unaryOpAst.m_UnaryOpKind == UnaryOpKind::U_REF) {
    if (unaryOpAst.m_Node != nullptr &&
        unaryOpAst.m_Node->m_ExprKind == ExprKind::BinOp) {
      auto *binary = static_cast<BinaryOpAST *>(unaryOpAst.m_Node.get());
      if (binary->m_BinOpKind == B_ARRS &&
          binary->m_LHS->m_ExprKind == ExprKind::SymbolExpr) {
        auto *arraySymbol = static_cast<SymbolAST *>(binary->m_LHS.get());
        auto *arrayStorage = FindStorage(m_LocalVarsOnScope, m_GlobalVars,
                                         arraySymbol->m_SymbolName);
        if (arrayStorage == nullptr) {
          assert(false && "'ref' array operand was not found.");
        }

        m_LastArrayIndex = true;
        auto *baseAddress = binary->m_LHS->accept(*this);
        m_LastArrayIndex = false;

        auto *previousType = m_LastType;
        bool previousSigned = m_LastSigned;
        m_LastType = Builder->getInt64Ty();
        m_LastSigned = true;
        auto *rawIndex = binary->m_RHS->accept(*this);
        m_LastType = previousType;
        m_LastSigned = previousSigned;

        std::vector<llvm::Value *> indexes = {CastArrayIndex(rawIndex)};
        auto symbolInfo = getSymbolInfo(arraySymbol->m_SymbolName);
        auto *arrayType = GetPointeeType(baseAddress);
        if (!arrayType->isArrayTy()) {
          auto arrayParamType =
              m_ArrayParamValueTypes.find(arraySymbol->m_SymbolName);
          if (arrayParamType != m_ArrayParamValueTypes.end()) {
            arrayType = arrayParamType->second;
          }
        }
        std::vector<llvm::Value *> idxList = {
            llvm::ConstantInt::get(Builder->getInt64Ty(), 0),
            CreateLinearArrayIndex(indexes, symbolInfo.arrayDimensions)};
        value = Builder->CreateInBoundsGEP(arrayType, baseAddress, idxList,
                                           arraySymbol->m_SymbolName + ".ref");
        m_LastType = value->getType();
        m_LastSigned = IsSigned(symbolInfo.type);
        return value;
      }

      if (binary->m_BinOpKind == B_DOT) {
        struct FieldAddress {
          llvm::Value *address = nullptr;
          llvm::Type *valueType = nullptr;
          SymbolInfo symbolInfo;
          std::string name;
        };

        auto resolveFieldAddress =
            [&](auto &self, IAST *node) -> FieldAddress {
          if (node == nullptr) {
            assert(false && "'ref' field operand was empty.");
          }

          if (node->m_ExprKind == ExprKind::SymbolExpr) {
            auto *symbol = static_cast<SymbolAST *>(node);
            auto info = getSymbolInfo(symbol->m_SymbolName);
            auto *symbolStorage = FindStorage(m_LocalVarsOnScope, m_GlobalVars,
                                              symbol->m_SymbolName);
            if (symbolStorage == nullptr) {
              assert(false && "'ref' field base storage was not found.");
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
                auto *handle = Builder->CreateLoad(
                    GetSharedPointerTy(), symbolStorage,
                    symbol->m_SymbolName + ".sp.field");
                symbolStorage = ExtractSharedPointerData(handle);
              } else {
                auto *pointerType =
                    GetType(info.type, info.indirectionLevel);
                symbolStorage = Builder->CreateLoad(
                    pointerType, symbolStorage,
                    symbol->m_SymbolName + ".ptr.field");
              }
              info.indirectionLevel = 0;
              info.isUnique = false;
              info.isRef = false;
            }

            return {symbolStorage, GetSymbolLLVMType(info), info,
                    symbol->m_SymbolName};
          }

          if (node->m_ExprKind != ExprKind::BinOp) {
            assert(false && "'ref' field operand must be symbol.field.");
          }

          auto *fieldAccess = static_cast<BinaryOpAST *>(node);
          if (fieldAccess->m_BinOpKind != B_DOT ||
              fieldAccess->m_RHS == nullptr ||
              fieldAccess->m_RHS->m_ExprKind != ExprKind::SymbolExpr) {
            assert(false && "'ref' field operand must be symbol.field.");
          }

          auto base = self(self, fieldAccess->m_LHS.get());
          auto *fieldSymbol = static_cast<SymbolAST *>(fieldAccess->m_RHS.get());
          auto structIt = StructTable.find(base.symbolInfo.definedTypeName);
          if (structIt == StructTable.end()) {
            assert(false && "'ref' field base struct type was not registered.");
          }

          auto fieldIt =
              structIt->second.fieldIndexes.find(fieldSymbol->m_SymbolName);
          if (fieldIt == structIt->second.fieldIndexes.end()) {
            assert(false && "'ref' target struct field was not registered.");
          }

          auto *structType = GetDefinedStructTy(base.symbolInfo.definedTypeName);
          const auto fieldIndex = fieldIt->second;
          const auto &field = structIt->second.fields[fieldIndex];
          auto fieldName = base.name + "." + field.name;
          auto *fieldAddress = Builder->CreateStructGEP(
              structType, base.address, static_cast<unsigned>(fieldIndex),
              fieldName + ".ref");

          auto fieldInfo = base.symbolInfo;
          fieldInfo.symbolName = fieldName;
          fieldInfo.type = field.type;
          fieldInfo.definedTypeName = field.definedTypeName;
          fieldInfo.indirectionLevel = field.indirectionLevel;
          fieldInfo.isUnique = field.isUnique;
          fieldInfo.isRef = field.isRef;
          fieldInfo.isNullable = field.isNullable;
          fieldInfo.isSubscriptable = false;
          fieldInfo.arrayDimensions.clear();

          return {fieldAddress, GetStructFieldLLVMType(field), fieldInfo,
                  fieldName};
        };

        auto field = resolveFieldAddress(resolveFieldAddress,
                                         unaryOpAst.m_Node.get());
        m_LastType = field.address->getType();
        m_LastSigned = IsSigned(GetEffectiveStorageType(
            field.symbolInfo.type, field.symbolInfo.definedTypeName));
        return field.address;
      }
    }

    if (unaryOpAst.m_Node == nullptr ||
        unaryOpAst.m_Node->m_ExprKind != ExprKind::SymbolExpr) {
      assert(false && "'ref' currently expects a symbol operand.");
    }

    auto *symbol = static_cast<SymbolAST *>(unaryOpAst.m_Node.get());
    if (m_LocalVarsOnScope.count(symbol->m_SymbolName) > 0) {
      value = m_LocalVarsOnScope[symbol->m_SymbolName];
      auto referenceParam = m_ReferenceParamValueTypes.find(symbol->m_SymbolName);
      if (referenceParam != m_ReferenceParamValueTypes.end()) {
        auto *referenceSlotType = GetPointeeType(value);
        value = CreateLoad(value, referenceSlotType,
                           symbol->m_SymbolName + ".ref.addr");
      }
    } else if (m_GlobalVars.count(symbol->m_SymbolName) > 0) {
      value = m_GlobalVars[symbol->m_SymbolName];
    } else {
      assert(false && "'ref' operand was not found in codegen symbol tables.");
    }

    if (IsSharedPointerTy(m_LastType)) {
      auto *counter = CreateSharedPointerCounter("sp.ref", 1);
      value = CreateSharedPointerHandle(value, counter);
    } else {
      m_LastType = value->getType();
    }
  }

  if (unaryOpAst.m_UnaryOpKind == UnaryOpKind::U_DEREF) {
    size_t derefLevel = 0;
    IAST *operand = &unaryOpAst;
    while (operand != nullptr && operand->getASTKind() == ASTKind::Expr &&
           operand->getExprKind() == ExprKind::UnaryOp) {
      auto *unaryOperand = static_cast<UnaryOpAST *>(operand);
      if (unaryOperand->m_UnaryOpKind != UnaryOpKind::U_DEREF) {
        break;
      }

      derefLevel += 1;
      operand = unaryOperand->m_Node.get();
    }

    if (operand != nullptr && operand->getASTKind() == ASTKind::Expr &&
        operand->getExprKind() == ExprKind::SymbolExpr) {
      auto *symbol = static_cast<SymbolAST *>(operand);
      llvm::Value *storage = nullptr;
      if (m_LocalVarsOnScope.count(symbol->m_SymbolName) > 0) {
        storage = m_LocalVarsOnScope[symbol->m_SymbolName];
      } else if (m_GlobalVars.count(symbol->m_SymbolName) > 0) {
        storage = m_GlobalVars[symbol->m_SymbolName];
      } else {
        assert(false && "'deref' operand was not found in codegen tables.");
      }

      const auto symbolInfo = getSymbolInfo(symbol->m_SymbolName);
      if (derefLevel == 0 || derefLevel > symbolInfo.indirectionLevel) {
        assert(false && "Invalid dereference level.");
      }

      if (IsSharedPointerSymbol(symbolInfo)) {
        value = Builder->CreateLoad(GetSharedPointerTy(), storage, "deref.sp");
        for (size_t level = 1; level <= derefLevel; ++level) {
          auto *data = ExtractSharedPointerData(value);
          const size_t remaining = symbolInfo.indirectionLevel - level;
          if (remaining > 0) {
            value = Builder->CreateLoad(GetSharedPointerTy(), data, "deref.sp");
          } else {
            m_LastType = GetType(symbolInfo.type, 0);
            value = Builder->CreateLoad(m_LastType, data, "deref.value");
            return value;
          }
        }
        m_LastType = GetSharedPointerTy();
        return value;
      }

      value = Builder->CreateLoad(
          GetType(symbolInfo.type, symbolInfo.indirectionLevel), storage,
          "deref");
      for (size_t level = 1; level < derefLevel; ++level) {
        value = Builder->CreateLoad(
            GetType(symbolInfo.type, symbolInfo.indirectionLevel - level),
            value, "deref");
      }

      m_LastType =
          GetType(symbolInfo.type, symbolInfo.indirectionLevel - derefLevel);
      value = Builder->CreateLoad(m_LastType, value, "deref.value");
      return value;
    }

    llvm::Type *loadType = m_LastType;
    value = unaryOpAst.m_Node->accept(*this);

    if (value == nullptr) {
      return nullptr;
    }

    if (loadType == nullptr || loadType->isVoidTy()) {
      assert(false && "'deref' requires a concrete expected value type.");
    }

    value = Builder->CreateLoad(loadType, value, "deref");
    m_LastType = loadType;
  }

  return value;
}
ValuePtr Visitor::visit(TypeAST &typeAst) { return nullptr; }

ValuePtr Visitor::visit(FixAST &fixAst) {
  if (fixAst.m_Symbol == nullptr ||
      fixAst.m_Symbol->m_ExprKind != ExprKind::SymbolExpr) {
    assert(false && "increment/decrement target must be a symbol.");
  }

  auto *symbol = static_cast<SymbolAST *>(fixAst.m_Symbol.get());
  llvm::Value *storage =
      FindStorage(m_LocalVarsOnScope, m_GlobalVars, symbol->m_SymbolName);
  if (storage == nullptr) {
    assert(false && "increment/decrement target was not found.");
  }

  auto symbolInfo = getSymbolInfo(symbol->m_SymbolName);
  llvm::Value *targetAddress = storage;
  llvm::Type *targetType = GetSymbolLLVMType(symbolInfo);

  auto referenceParam = m_ReferenceParamValueTypes.find(symbol->m_SymbolName);
  if (referenceParam != m_ReferenceParamValueTypes.end()) {
    auto *slotType = GetPointeeType(storage);
    targetAddress =
        CreateLoad(storage, slotType, symbol->m_SymbolName + ".fix.ref.addr");
    targetType = referenceParam->second;
  }

  auto *currentValue =
      Builder->CreateLoad(targetType, targetAddress, symbol->m_SymbolName);
  auto *one = CreateOneValue(targetType);
  llvm::Value *nextValue = nullptr;

  if (fixAst.m_IsIncrement) {
    nextValue = targetType->isFloatingPointTy()
                    ? Builder->CreateFAdd(currentValue, one, "inctmp")
                    : (IsSigned(symbolInfo.type)
                           ? Builder->CreateNSWAdd(currentValue, one, "inctmp")
                           : Builder->CreateNUWAdd(currentValue, one,
                                                   "inctmp"));
  } else if (fixAst.m_IsDecrement) {
    nextValue = targetType->isFloatingPointTy()
                    ? Builder->CreateFSub(currentValue, one, "dectmp")
                    : (IsSigned(symbolInfo.type)
                           ? Builder->CreateNSWSub(currentValue, one, "dectmp")
                           : Builder->CreateNUWSub(currentValue, one,
                                                   "dectmp"));
  } else {
    assert(false && "increment/decrement expression is missing an operator.");
  }

  Builder->CreateStore(nextValue, targetAddress);
  m_LastType = targetType;
  m_LastSigned = IsSigned(symbolInfo.type);
  return fixAst.m_IsPostfix ? currentValue : nextValue;
}

ValuePtr Visitor::visit(StructAST &structAst) {
  (void)GetDefinedStructTy(structAst.m_Name);
  return nullptr;
}

ValuePtr Visitor::visit(TraitAST &traitAst) { return nullptr; }

ValuePtr Visitor::visit(EnumAST &enumAst) { return nullptr; }

void Visitor::finalizeCodegen() {
  if (this->m_GlobaInitVarFunc.size() > 0) {
    auto function = CreateGlobalFuncSubToMain(this->m_GlobaInitVarFunc);
    llvm::verifyFunction(*function);

    std::vector<llvm::Type *> types;
    types.push_back(Builder->getInt32Ty());
    types.push_back(function->getType());
    types.push_back(GetI8PtrTy());

    auto structType = llvm::StructType::get(Builder->getContext(), types);
    auto type = llvm::ArrayType::get(structType, 1);
    Visitor::Module->getOrInsertGlobal(llvm::StringRef("llvm.global_ctors"),
                                       type);
    auto globVar =
        Visitor::Module->getNamedGlobal(llvm::StringRef("llvm.global_ctors"));
    globVar->setLinkage(llvm::GlobalVariable::AppendingLinkage);
    //  globVar->setSection(llvm::StringRef(".ctor"));
    std::vector<llvm::Constant *> values;
    values.push_back(llvm::ConstantInt::get(Builder->getInt32Ty(), 65535));
    values.push_back(function);
    values.push_back(llvm::ConstantPointerNull::get(GetI8PtrTy()));
    auto *ctorVal = llvm::ConstantStruct::get(structType, values);

    std::vector<llvm::Constant *> ctorValues;
    ctorValues.push_back(ctorVal);
    auto init = llvm::ConstantArray::get(type, ctorValues);
    globVar->setInitializer(init);
  }
}
