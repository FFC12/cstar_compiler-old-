#include <visitor/codegen/codegen_private.hpp>

ValuePtr Visitor::emitDropForSymbol(const std::string &symbolName,
                                    bool markDropped) {
  auto *storage = FindStorage(m_LocalVarsOnScope, m_GlobalVars, symbolName);
  if (storage == nullptr) {
    assert(false && "drop target storage was not found.");
  }

  auto symbolInfo = getSymbolInfo(symbolName);
  if (symbolInfo.type != TypeSpecifier::SPEC_DEFINED) {
    return nullptr;
  }

  auto heapIt = m_HeapAllocations.find(symbolName);
  if (heapIt != m_HeapAllocations.end()) {
    const auto &heapInfo = heapIt->second;
    auto *structType = GetDefinedStructTy(heapInfo.typeName);
    auto allocationSize = Module->getDataLayout().getTypeAllocSize(structType);
    auto allocationAlign = Module->getDataLayout().getPrefTypeAlign(structType)
                               .value();
    auto *sizeValue = llvm::ConstantInt::get(Builder->getInt64Ty(),
                                             allocationSize);
    auto *alignValue = llvm::ConstantInt::get(Builder->getInt64Ty(),
                                              allocationAlign);
    auto freeHeapData = [&](llvm::Value *data) {
      if (!heapInfo.allocatorSymbol.empty() && !heapInfo.allocatorTypeName.empty()) {
        auto *freeFunction = Module->getFunction(heapInfo.allocatorTypeName +
                                                 ".free");
        auto *allocatorStorage =
            FindStorage(m_LocalVarsOnScope, m_GlobalVars,
                        heapInfo.allocatorSymbol);
        if (freeFunction != nullptr && allocatorStorage != nullptr) {
          auto allocatorInfo = getSymbolInfo(heapInfo.allocatorSymbol);
          if (allocatorInfo.isRef &&
              m_ReferenceParamValueTypes.count(heapInfo.allocatorSymbol) > 0) {
            auto *slotType = GetPointeeType(allocatorStorage);
            allocatorStorage = CreateLoad(allocatorStorage, slotType,
                                          heapInfo.allocatorSymbol +
                                              ".allocator.free.ref");
          }
          Builder->CreateCall(freeFunction,
                              {allocatorStorage, data, sizeValue, alignValue});
          return;
        }
      }
      CreateDefaultHeapFree(data);
    };

    auto *destructor = Module->getFunction(heapInfo.typeName + ".destructor");
    llvm::Function *function = Builder->GetInsertBlock()->getParent();

    if (heapInfo.isShared) {
      auto *handle = Builder->CreateLoad(GetSharedPointerTy(), storage,
                                         symbolName + ".drop.heap.sp");
      auto *countAsI8 = ExtractSharedPointerCount(handle);
      auto *isLive = Builder->CreateICmpNE(
          countAsI8, llvm::ConstantPointerNull::get(GetI8PtrTy()),
          symbolName + ".drop.heap.live");
      auto *releaseBB = llvm::BasicBlock::Create(
          Builder->getContext(), "heap.shared.release", function);
      auto *contBB = llvm::BasicBlock::Create(
          Builder->getContext(), "heap.shared.cont", function);
      Builder->CreateCondBr(isLive, releaseBB, contBB);

      Builder->SetInsertPoint(releaseBB);
      auto *count = Builder->CreatePointerCast(
          countAsI8, llvm::PointerType::get(Builder->getContext(), 0));
      auto *one = llvm::ConstantInt::get(Builder->getInt64Ty(), 1);
      auto *oldCount = Builder->CreateAtomicRMW(
          llvm::AtomicRMWInst::Sub, count, one, llvm::MaybeAlign(),
          llvm::AtomicOrdering::AcquireRelease);
      auto *isLast = Builder->CreateICmpEQ(oldCount, one,
                                           symbolName + ".drop.heap.last");
      auto *destroyBB = llvm::BasicBlock::Create(
          Builder->getContext(), "heap.shared.destroy", function);
      auto *afterReleaseBB = llvm::BasicBlock::Create(
          Builder->getContext(), "heap.shared.after", function);
      Builder->CreateCondBr(isLast, destroyBB, afterReleaseBB);

      Builder->SetInsertPoint(destroyBB);
      auto *data = ExtractSharedPointerData(handle);
      if (destructor != nullptr) {
        Builder->CreateCall(destructor, {data});
      }
      freeHeapData(data);
      CreateDefaultHeapFree(countAsI8);
      Builder->CreateBr(afterReleaseBB);

      Builder->SetInsertPoint(afterReleaseBB);
      if (markDropped) {
        Builder->CreateStore(CreateNullSharedPointerHandle(), storage);
      }
      Builder->CreateBr(contBB);

      Builder->SetInsertPoint(contBB);
      if (markDropped) {
        m_CodegenDroppedSymbols.insert(symbolName);
      }
      return handle;
    }

    auto *pointerType = GetType(symbolInfo.type, symbolInfo.indirectionLevel);
    auto *data = Builder->CreateLoad(pointerType, storage,
                                     symbolName + ".drop.heap.ptr");
    auto *isLive = Builder->CreateICmpNE(
        data, llvm::ConstantPointerNull::get(
                  llvm::cast<llvm::PointerType>(data->getType())),
        symbolName + ".drop.heap.live");
    auto *destroyBB = llvm::BasicBlock::Create(
        Builder->getContext(), "heap.unique.destroy", function);
    auto *contBB = llvm::BasicBlock::Create(
        Builder->getContext(), "heap.unique.cont", function);
    Builder->CreateCondBr(isLive, destroyBB, contBB);

    Builder->SetInsertPoint(destroyBB);
    if (destructor != nullptr) {
      Builder->CreateCall(destructor, {data});
    }
    freeHeapData(data);
    if (markDropped) {
      Builder->CreateStore(llvm::Constant::getNullValue(pointerType), storage);
    }
    Builder->CreateBr(contBB);

    Builder->SetInsertPoint(contBB);
    if (markDropped) {
      m_CodegenDroppedSymbols.insert(symbolName);
    }
    return data;
  }

  llvm::Value *self = storage;
  if (symbolInfo.indirectionLevel > 0 && !symbolInfo.isRef) {
    if (IsSharedPointerSymbol(symbolInfo)) {
      auto *handle = Builder->CreateLoad(GetSharedPointerTy(), storage,
                                         symbolName + ".drop.sp");
      self = ExtractSharedPointerData(handle);
      if (markDropped) {
        ReleaseSharedPointer(handle);
        Builder->CreateStore(CreateNullSharedPointerHandle(), storage);
      }
    } else {
      auto *pointerType = GetType(symbolInfo.type, symbolInfo.indirectionLevel);
      self = Builder->CreateLoad(pointerType, storage, symbolName + ".drop.ptr");
      if (markDropped) {
        Builder->CreateStore(llvm::Constant::getNullValue(pointerType),
                             storage);
      }
    }
  }

  llvm::Value *result = nullptr;
  auto *destructor = Module->getFunction(symbolInfo.definedTypeName +
                                         ".destructor");
  if (destructor != nullptr) {
    result = Builder->CreateCall(destructor, {self});
  }

  if (markDropped) {
    m_CodegenDroppedSymbols.insert(symbolName);
  }
  return result;
}

void Visitor::emitScopeExitDestructors() {
  auto *insertBlock = Builder->GetInsertBlock();
  if (insertBlock == nullptr || insertBlock->getTerminator() != nullptr) {
    return;
  }

  for (auto it = m_ScopeDestructors.rbegin();
       it != m_ScopeDestructors.rend(); ++it) {
    if (m_CodegenDroppedSymbols.count(it->symbolName) != 0) {
      continue;
    }
    emitDropForSymbol(it->symbolName, false);
  }

  for (const auto &entry : m_HeapAllocations) {
    if (m_CodegenDroppedSymbols.count(entry.first) != 0) {
      continue;
    }
    emitDropForSymbol(entry.first, false);
  }
}

ValuePtr Visitor::createBinaryOp(BinaryOpAST &binaryOpAst) {
  llvm::Value *value = nullptr, *rhs, *lhs;

  if (binaryOpAst.m_BinOpKind == B_DOT) {
    if (binaryOpAst.m_LHS != nullptr && binaryOpAst.m_RHS != nullptr &&
        binaryOpAst.m_LHS->m_ExprKind == ExprKind::SymbolExpr &&
        binaryOpAst.m_RHS->m_ExprKind == ExprKind::SymbolExpr) {
      auto *enumSymbol = static_cast<SymbolAST *>(binaryOpAst.m_LHS.get());
      auto *memberSymbol = static_cast<SymbolAST *>(binaryOpAst.m_RHS.get());
      auto enumIt = EnumTable.find(enumSymbol->m_SymbolName);
      if (enumIt != EnumTable.end()) {
        auto *member =
            FindEnumMember(enumSymbol->m_SymbolName, memberSymbol->m_SymbolName);
        if (member == nullptr) {
          assert(false && "Enum member should have been checked in pass1.");
        }

        auto *enumType = GetType(enumIt->second.underlyingType, 0);
        m_LastType = enumType;
        m_LastDefinedTypeName = enumSymbol->m_SymbolName;
        m_LastSigned = IsSigned(enumIt->second.underlyingType);
        return llvm::ConstantInt::get(enumType, member->value, m_LastSigned);
      }
    }

    struct FieldAddress {
      llvm::Value *address = nullptr;
      llvm::Type *valueType = nullptr;
      SymbolInfo symbolInfo;
      std::string name;
    };

    auto resolveFieldAddress =
        [&](auto &self, IAST *node) -> FieldAddress {
      if (node == nullptr) {
        assert(false && "Struct field access requires symbol.field.");
      }

      if (node->m_ExprKind == ExprKind::SymbolExpr) {
        auto *symbol = static_cast<SymbolAST *>(node);
        auto info = getSymbolInfo(symbol->m_SymbolName);
        auto *storage = FindStorage(m_LocalVarsOnScope, m_GlobalVars,
                                    symbol->m_SymbolName);
        if (storage == nullptr) {
          assert(false && "Struct field base storage was not found.");
        }
        if (info.isRef && m_ReferenceParamValueTypes.count(symbol->m_SymbolName) > 0) {
          auto *slotType = GetPointeeType(storage);
          storage = CreateLoad(storage, slotType,
                               symbol->m_SymbolName + ".ref.field");
        }
        if (info.type == TypeSpecifier::SPEC_DEFINED && !info.isRef &&
            info.indirectionLevel > 0) {
          if (IsSharedPointerSymbol(info)) {
            auto *handle = Builder->CreateLoad(GetSharedPointerTy(), storage,
                                               symbol->m_SymbolName + ".sp");
            storage = ExtractSharedPointerData(handle);
          } else {
            auto *pointerType = GetType(info.type, info.indirectionLevel);
            storage = Builder->CreateLoad(pointerType, storage,
                                          symbol->m_SymbolName + ".ptr");
          }
          info.indirectionLevel = 0;
          info.isUnique = false;
          info.isRef = false;
        }
        return {storage, GetSymbolLLVMType(info), info, symbol->m_SymbolName};
      }

      if (node->m_ExprKind != ExprKind::BinOp) {
        assert(false && "Struct field access requires symbol.field.");
      }

      auto *fieldAccess = static_cast<BinaryOpAST *>(node);
      if (fieldAccess->m_BinOpKind != B_DOT ||
          fieldAccess->m_RHS == nullptr ||
          fieldAccess->m_RHS->m_ExprKind != ExprKind::SymbolExpr) {
        assert(false && "Struct field access requires symbol.field.");
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
      fieldInfo.isSubscriptable = false;
      fieldInfo.arrayDimensions.clear();
      return {fieldAddress, GetStructFieldLLVMType(field), fieldInfo,
              fieldName};
    };

    auto field = resolveFieldAddress(resolveFieldAddress, &binaryOpAst);
    m_LastType = field.valueType;
    m_LastDefinedTypeName = field.symbolInfo.definedTypeName;
    m_LastSigned = IsSigned(field.symbolInfo.type);
    return Builder->CreateLoad(field.valueType, field.address, field.name);
  }

  if (binaryOpAst.m_BinOpKind == B_TER) {
    if (binaryOpAst.m_LHS == nullptr || binaryOpAst.m_RHS == nullptr ||
        binaryOpAst.m_Extra == nullptr) {
      return nullptr;
    }

    auto *targetType = m_LastType;
    const auto targetDefinedTypeName = m_LastDefinedTypeName;
    const auto targetSigned = m_LastSigned;

    m_LastType = nullptr;
    m_LastDefinedTypeName.clear();
    auto *condition = binaryOpAst.m_LHS->accept(*this);
    condition = CastValueToBranchCondition(condition, m_LastSigned);
    if (condition == nullptr) {
      m_LastType = targetType;
      m_LastDefinedTypeName = targetDefinedTypeName;
      m_LastSigned = targetSigned;
      return nullptr;
    }

    m_LastType = targetType;
    m_LastDefinedTypeName = targetDefinedTypeName;
    m_LastSigned = targetSigned;
    auto *trueValue = binaryOpAst.m_RHS->accept(*this);
    if (trueValue == nullptr) {
      return nullptr;
    }

    auto *selectType = targetType != nullptr ? targetType : trueValue->getType();
    trueValue = CastValueToType(trueValue, selectType, m_LastSigned);

    m_LastType = selectType;
    m_LastDefinedTypeName = targetDefinedTypeName;
    auto *falseValue = binaryOpAst.m_Extra->accept(*this);
    if (falseValue == nullptr) {
      return nullptr;
    }
    falseValue = CastValueToType(falseValue, selectType, m_LastSigned);

    m_LastType = selectType;
    m_LastDefinedTypeName = targetDefinedTypeName;
    m_LastSigned = targetSigned;
    return Builder->CreateSelect(condition, trueValue, falseValue,
                                 "selecttmp");
  }

  if (binaryOpAst.m_BinOpKind == B_ARRS) {
    m_LastArrayIndex = true;
    lhs = binaryOpAst.m_LHS->accept(*this);

    auto *previousType = m_LastType;
    bool previousSigned = m_LastSigned;
    m_LastArrayIndex = false;
    m_LastType = Builder->getInt64Ty();
    m_LastSigned = true;
    rhs = binaryOpAst.m_RHS->accept(*this);
    m_LastType = previousType;
    m_LastSigned = previousSigned;
  } else {
    const auto overloadMethod = ValueOperatorMethodName(binaryOpAst.m_BinOpKind);
    if (!overloadMethod.empty() &&
        binaryOpAst.m_LHS->m_ExprKind == ExprKind::SymbolExpr) {
      auto *lhsSymbol = static_cast<SymbolAST *>(binaryOpAst.m_LHS.get());
      auto lhsInfo = getSymbolInfo(lhsSymbol->m_SymbolName);
      if (lhsInfo.type == TypeSpecifier::SPEC_DEFINED &&
          lhsInfo.indirectionLevel == 0) {
        auto *function =
            Module->getFunction(lhsInfo.definedTypeName + "." + overloadMethod);
        auto *lhsStorage = FindStorage(m_LocalVarsOnScope, m_GlobalVars,
                                       lhsSymbol->m_SymbolName);
        if (function != nullptr && lhsStorage != nullptr) {
          if (lhsInfo.isRef &&
              m_ReferenceParamValueTypes.count(lhsSymbol->m_SymbolName) > 0) {
            auto *slotType = GetPointeeType(lhsStorage);
            lhsStorage = CreateLoad(lhsStorage, slotType,
                                    lhsSymbol->m_SymbolName + ".op.ref");
          }
          auto *paramType = function->getFunctionType()->getParamType(1);
          rhs = binaryOpAst.m_RHS->accept(*this);
          rhs = CastValueToType(rhs, paramType, m_LastSigned);
          value = Builder->CreateCall(function, {lhsStorage, rhs},
                                      "operator.call");
          m_LastType = function->getReturnType();
          m_LastSigned = true;
          return value;
        }
      }
    }

    lhs = binaryOpAst.m_LHS->accept(*this);
    auto *lhsType = m_LastType;
    const bool lhsSigned = m_LastSigned;

    m_LastType = lhsType;
    m_LastSigned = lhsSigned;
    rhs = binaryOpAst.m_RHS->accept(*this);

    const bool rhsSigned = m_LastSigned;
    if (lhsType != nullptr) {
      m_LastType = lhsType;
    }
    m_LastSigned = lhsSigned && rhsSigned;
  }

  // NSW (No Signed Wrap) and NUW(No Unsigned Wrap)
  switch (binaryOpAst.m_BinOpKind) {
    case B_ADD: {
      value = CreateArithmeticValue(B_ADD, lhs, rhs, m_LastType, m_LastSigned);
      break;
    }
    case B_SUB:
      value = CreateArithmeticValue(B_SUB, lhs, rhs, m_LastType, m_LastSigned);
      break;
    case B_MUL:
      value = CreateArithmeticValue(B_MUL, lhs, rhs, m_LastType, m_LastSigned);
      break;
    case B_DIV: {
      value = CreateArithmeticValue(B_DIV, lhs, rhs, m_LastType, m_LastSigned);
      break;
    }
    case B_MOD:
      value = CreateArithmeticValue(B_MOD, lhs, rhs, m_LastType, m_LastSigned);
      break;
    case B_AND:
      value = Builder->CreateAnd(lhs, rhs, "andtemp");
      break;
    case B_LAND:
      value = CreateLogicalValue(B_LAND, lhs, rhs, m_LastSigned);
      break;
    case B_OR:
      value = Builder->CreateOr(lhs, rhs, "andtemp");
      break;
    case B_LOR:
      value = CreateLogicalValue(B_LOR, lhs, rhs, m_LastSigned);
      break;
    case B_XOR:
      value = Builder->CreateXor(lhs, rhs, "andtemp");
      break;
    case B_GT:
      value = CreateOrderedCompare(llvm::CmpInst::FCMP_OGT,
                                   llvm::CmpInst::ICMP_SGT,
                                   llvm::CmpInst::ICMP_UGT, lhs, rhs,
                                   m_LastType, m_LastSigned, "gttmp");
      break;
    case B_GTEQ:
      value = CreateOrderedCompare(llvm::CmpInst::FCMP_OGE,
                                   llvm::CmpInst::ICMP_SGE,
                                   llvm::CmpInst::ICMP_UGE, lhs, rhs,
                                   m_LastType, m_LastSigned, "getmp");
      break;
    case B_LT:
      value = CreateOrderedCompare(llvm::CmpInst::FCMP_OLT,
                                   llvm::CmpInst::ICMP_SLT,
                                   llvm::CmpInst::ICMP_ULT, lhs, rhs,
                                   m_LastType, m_LastSigned, "lttmp");
      break;
    case B_LTEQ:
      value = CreateOrderedCompare(llvm::CmpInst::FCMP_OLE,
                                   llvm::CmpInst::ICMP_SLE,
                                   llvm::CmpInst::ICMP_ULE, lhs, rhs,
                                   m_LastType, m_LastSigned, "letmp");
      break;
    case B_SHL:
      value = Builder->CreateShl(lhs, rhs, "andtemp");
      break;
    case B_SHR:
      if (m_LastSigned) {
        value = Builder->CreateAShr(lhs, rhs, "ashrtemp");
      } else {
        value = Builder->CreateLShr(lhs, rhs, "lshrtemp");
      }
      break;
    case B_EQ: {
      value = CreateEqualityCompare(true, lhs, rhs, m_LastType, "eqtmp");
      break;
    }
    case B_NEQ: {
      value = CreateEqualityCompare(false, lhs, rhs, m_LastType, "netmp");
      break;
    }
    case B_TER: {
      break;
    }
    case B_ARRS: {
      // just constant or getelement will be in the value...
      if (rhs != nullptr) {
        // if the rhs is constant and also float number then we have to
        // interpret as int of it.
        if (rhs->getType()->isFloatTy() || rhs->getType()->isDoubleTy()) {
          SymbolInfo symbolInfo;
          symbolInfo.begin = binaryOpAst.m_SemLoc.begin;
          symbolInfo.end = binaryOpAst.m_SemLoc.end;
          symbolInfo.line = binaryOpAst.m_SemLoc.line;

          // not working right now.
          m_TypeWarningMessages.emplace_back(
              "Array subscript is not an integer. It casted to int value",
              symbolInfo);

          rhs = Builder->CreateFPToSI(rhs, Builder->getInt64Ty());
        }
      }

      auto symbol = lhs;
      std::vector<llvm::Value *> indexes;
      auto symbolName = ((SymbolAST *)binaryOpAst.m_LHS.get())->m_SymbolName;
      auto symbolInfo = getSymbolInfo(symbolName);
      if (rhs != nullptr) {
        indexes.push_back(CastArrayIndex(rhs));
      } else {
        indexes = std::move(m_Indices);
        m_Indices.clear();
        m_IndicesAsStr.clear();
      }

      auto symbolType = GetPointeeType(symbol);
      if (!symbolType->isArrayTy() &&
          binaryOpAst.m_LHS->m_ExprKind == ExprKind::SymbolExpr) {
        auto *arraySymbol = static_cast<SymbolAST *>(binaryOpAst.m_LHS.get());
        auto arrayParamType = m_ArrayParamValueTypes.find(arraySymbol->m_SymbolName);
        if (arrayParamType != m_ArrayParamValueTypes.end()) {
          symbolType = arrayParamType->second;
        }
      }
      std::vector<llvm::Value *> idxList = {
          llvm::ConstantInt::get(Builder->getInt64Ty(), 0),
          CreateLinearArrayIndex(indexes, symbolInfo.arrayDimensions)};
      auto gep = Builder->CreateInBoundsGEP(symbolType, symbol, idxList);
      auto loaded = Visitor::Builder->CreateLoad(
          symbolType->getArrayElementType(), gep);
      m_LastType = symbolType;
      if (m_LastType->isArrayTy()) {
        m_LastType = m_LastType->getArrayElementType();
      }

      value = loaded;

      break;
    }
    case B_COMM:
      break;
    case B_DOT:
      break;
    case B_ARW:
      break;
    case B_CCOL:
      break;
    case B_MARRS: {
      // TODO: You have to extract values and symbols from the
      // rhs and you have to use it as index for GEP
      // but if it's symbol or any binary op then you have to
      // call m_RHS->accept before...
      if (lhs != nullptr) {
        m_Indices.push_back(lhs);
      }

      if (rhs != nullptr) {
        m_Indices.push_back(rhs);
      }
      // value = Builder->CreateInBoundsGEP(m_LastType, )
      break;
    }
  }

  return value;
}
