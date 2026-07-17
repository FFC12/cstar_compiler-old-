#include <visitor/codegen/codegen_private.hpp>

ValuePtr Visitor::visit(CastOpAST &castOpAst) {
  if (castOpAst.m_CastOpKind == CastOpKind::C_DYNAMIC_REF_AS ||
      castOpAst.m_CastOpKind == CastOpKind::C_DYNAMIC_MOVE_AS) {
    auto *targetTypeAst = dynamic_cast<TypeAST *>(castOpAst.m_TypeNode.get());
    if (targetTypeAst == nullptr || targetTypeAst->m_Symbol == nullptr ||
        targetTypeAst->m_Symbol->m_ExprKind != ExprKind::SymbolExpr) {
      assert(false && "Dynamic trait erasure target must be a trait type.");
    }

    if (castOpAst.m_Node == nullptr ||
        castOpAst.m_Node->m_ExprKind != ExprKind::SymbolExpr) {
      assert(false && "Dynamic trait erasure source must be a named value.");
    }

    auto *traitSymbol =
        static_cast<SymbolAST *>(targetTypeAst->m_Symbol.get());
    const auto traitName = traitSymbol->m_SymbolName;
    auto *sourceSymbol = static_cast<SymbolAST *>(castOpAst.m_Node.get());
    auto sourceInfo = getSymbolInfo(sourceSymbol->m_SymbolName);
    auto *storage = FindStorage(m_LocalVarsOnScope, m_GlobalVars,
                                sourceSymbol->m_SymbolName);
    if (storage == nullptr) {
      assert(false && "Dynamic trait erasure source storage was not found.");
    }

    llvm::Value *dataPtr = storage;
    if (sourceInfo.isDynamicTraitObject) {
      auto *object = Builder->CreateLoad(GetDynamicTraitObjectTy(), storage,
                                         sourceSymbol->m_SymbolName +
                                             ".dyn");
      m_LastType = GetDynamicTraitObjectTy();
      m_LastSigned = false;
      return object;
    }

    if (sourceInfo.isRef &&
        m_ReferenceParamValueTypes.count(sourceSymbol->m_SymbolName) > 0) {
      auto *slotType = GetPointeeType(storage);
      dataPtr = CreateLoad(storage, slotType,
                           sourceSymbol->m_SymbolName + ".dyn.ref");
    } else if (sourceInfo.indirectionLevel > 0) {
      if (IsSharedPointerSymbol(sourceInfo)) {
        auto *handle = Builder->CreateLoad(GetSharedPointerTy(), storage,
                                           sourceSymbol->m_SymbolName +
                                               ".dyn.sp");
        dataPtr = ExtractSharedPointerData(handle);
      } else {
        auto *pointerType =
            GetType(sourceInfo.type, sourceInfo.indirectionLevel);
        dataPtr = Builder->CreateLoad(pointerType, storage,
                                      sourceSymbol->m_SymbolName + ".dyn.ptr");
      }
    }

    dataPtr = Builder->CreatePointerCast(dataPtr, GetI8PtrTy(),
                                         sourceSymbol->m_SymbolName +
                                             ".dyn.data");

    auto *vtable = GetOrCreateDynamicTraitVTable(traitName,
                                                 sourceInfo.definedTypeName);
    if (vtable == nullptr) {
      assert(false && "Dynamic trait vtable could not be created.");
    }
    auto *vtablePtr = Builder->CreatePointerCast(vtable, GetI8PtrTy(),
                                                 traitName + ".dyn.vtable");

    llvm::Value *object = llvm::UndefValue::get(GetDynamicTraitObjectTy());
    object = Builder->CreateInsertValue(object, dataPtr, {0}, "dyn.data.in");
    object =
        Builder->CreateInsertValue(object, vtablePtr, {1}, "dyn.vtable.in");
    m_LastType = GetDynamicTraitObjectTy();
    m_LastSigned = false;
    return object;
  }

  if (!castOpAst.m_HasTypeAttrib || castOpAst.m_TypeNode == nullptr) {
    assert(false && "Cast expression requires an explicit target type.");
  }

  auto *targetTypeAst = dynamic_cast<TypeAST *>(castOpAst.m_TypeNode.get());
  if (targetTypeAst == nullptr) {
    assert(false && "Cast target must be a type expression.");
  }

  auto *targetType = GetStorageType(targetTypeAst->m_TypeSpec,
                                    targetTypeAst->m_IndirectLevel,
                                    targetTypeAst->m_IsUniquePtr,
                                    targetTypeAst->m_IsRef);
  auto *targetDataType =
      GetType(targetTypeAst->m_TypeSpec, targetTypeAst->m_IndirectLevel);
  const bool targetSigned = IsSigned(targetTypeAst->m_TypeSpec);

  llvm::Type *previousType = m_LastType;
  bool previousSigned = m_LastSigned;
  const bool expectedRawPointer =
      previousType != nullptr && previousType->isPointerTy() &&
      targetTypeAst->m_IndirectLevel > 0;
  if (expectedRawPointer) {
    targetType = previousType;
    targetDataType = previousType;
  }

  m_LastType = nullptr;
  m_LastSigned = targetSigned;
  llvm::Value *value = castOpAst.m_Node->accept(*this);

  if (castOpAst.m_CastOpKind == CastOpKind::C_UNSAFE_CAST) {
    if (IsSharedPointerTy(targetType)) {
      auto *data = UnsafeCastValueToType(value, targetDataType, targetSigned);
      auto *counter = CreateSharedPointerCounter("sp.cast", 1);
      value = CreateSharedPointerHandle(data, counter);
    } else {
      value = UnsafeCastValueToType(value, targetType, targetSigned);
    }
  } else {
    value = CastValueToType(value, targetType, targetSigned);
  }

  m_LastType = targetType;
  m_LastSigned = targetSigned;

  if (previousType != nullptr && !expectedRawPointer) {
    value = CastValueToType(value, previousType, previousSigned);
    m_LastType = previousType;
    m_LastSigned = previousSigned;
  }

  return value;
}
