#include <visitor/codegen/codegen_private.hpp>

ValuePtr Visitor::visit(CastOpAST &castOpAst) {
  if (castOpAst.m_CastOpKind == CastOpKind::C_DYNAMIC_REF_AS ||
      castOpAst.m_CastOpKind == CastOpKind::C_DYNAMIC_MOVE_AS) {
    assert(false &&
           "Dynamic trait object ABI/vtable lowering is not implemented yet.");
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
