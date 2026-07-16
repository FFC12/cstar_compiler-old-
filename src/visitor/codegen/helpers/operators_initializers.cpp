#include <visitor/codegen/codegen_private.hpp>

llvm::Value *CreateIntegerOrPointerCompare(
    llvm::CmpInst::Predicate signedPredicate,
    llvm::CmpInst::Predicate unsignedPredicate, llvm::Value *lhs,
    llvm::Value *rhs, bool isSigned, const llvm::Twine &name) {
  auto predicate = isSigned ? signedPredicate : unsignedPredicate;
  return Visitor::Builder->CreateICmp(predicate, lhs, rhs, name);
}

llvm::Value *CreateOrderedCompare(llvm::CmpInst::Predicate floatPredicate,
                                         llvm::CmpInst::Predicate signedPredicate,
                                         llvm::CmpInst::Predicate unsignedPredicate,
                                         llvm::Value *lhs, llvm::Value *rhs,
                                         llvm::Type *valueType, bool isSigned,
                                         const llvm::Twine &name) {
  lhs = CastValueToType(lhs, valueType, isSigned);
  rhs = CastValueToType(rhs, valueType, isSigned);

  if (valueType->isFloatTy() || valueType->isDoubleTy()) {
    return Visitor::Builder->CreateFCmp(floatPredicate, lhs, rhs, name);
  }

  return CreateIntegerOrPointerCompare(signedPredicate, unsignedPredicate, lhs,
                                       rhs, isSigned, name);
}

llvm::Value *CreateEqualityCompare(bool equals, llvm::Value *lhs,
                                          llvm::Value *rhs,
                                          llvm::Type *valueType,
                                          const llvm::Twine &name) {
  lhs = CastValueToType(lhs, valueType, true);
  rhs = CastValueToType(rhs, valueType, true);

  if (valueType->isFloatTy() || valueType->isDoubleTy()) {
    return Visitor::Builder->CreateFCmp(
        equals ? llvm::CmpInst::FCMP_OEQ : llvm::CmpInst::FCMP_ONE, lhs, rhs,
        name);
  }

  return Visitor::Builder->CreateICmp(equals ? llvm::CmpInst::ICMP_EQ
                                             : llvm::CmpInst::ICMP_NE,
                                      lhs, rhs, name);
}

bool IsFloatingPointType(llvm::Type *type) {
  return type != nullptr && (type->isFloatTy() || type->isDoubleTy());
}

llvm::Value *CreateArithmeticValue(BinOpKind op, llvm::Value *lhs,
                                          llvm::Value *rhs,
                                          llvm::Type *valueType,
                                          bool isSigned) {
  const bool isFloat = IsFloatingPointType(valueType);

  switch (op) {
    case B_ADD:
      if (isFloat) return Visitor::Builder->CreateFAdd(lhs, rhs, "addtmp");
      return isSigned ? Visitor::Builder->CreateNSWAdd(lhs, rhs, "addtmp")
                      : Visitor::Builder->CreateNUWAdd(lhs, rhs, "addtmp");
    case B_SUB:
      if (isFloat) return Visitor::Builder->CreateFSub(lhs, rhs, "subtmp");
      return isSigned ? Visitor::Builder->CreateNSWSub(lhs, rhs, "subtmp")
                      : Visitor::Builder->CreateNUWSub(lhs, rhs, "subtmp");
    case B_MUL:
      if (isFloat) return Visitor::Builder->CreateFMul(lhs, rhs, "multmp");
      return isSigned ? Visitor::Builder->CreateNSWMul(lhs, rhs, "multmp")
                      : Visitor::Builder->CreateNUWMul(lhs, rhs, "multmp");
    case B_DIV:
      if (isFloat) return Visitor::Builder->CreateFDiv(lhs, rhs, "divtmp");
      return isSigned ? Visitor::Builder->CreateSDiv(lhs, rhs, "divtmp")
                      : Visitor::Builder->CreateUDiv(lhs, rhs, "divtmp");
    case B_MOD:
      if (isFloat) return Visitor::Builder->CreateFRem(lhs, rhs, "remtmp");
      return isSigned ? Visitor::Builder->CreateSRem(lhs, rhs, "remtmp")
                      : Visitor::Builder->CreateURem(lhs, rhs, "remtmp");
    default:
      assert(false && "Unsupported arithmetic binary operator.");
      return nullptr;
  }
}

llvm::Value *CreateLogicalValue(BinOpKind op, llvm::Value *lhs,
                                       llvm::Value *rhs, bool isSigned) {
  switch (op) {
    case B_LAND:
      lhs = CastValueToBranchCondition(lhs, isSigned);
      rhs = CastValueToBranchCondition(rhs, isSigned);
      return Visitor::Builder->CreateLogicalAnd(lhs, rhs, "landtmp");
    case B_LOR:
      lhs = CastValueToBranchCondition(lhs, isSigned);
      rhs = CastValueToBranchCondition(rhs, isSigned);
      return Visitor::Builder->CreateLogicalOr(lhs, rhs, "lortmp");
    default:
      assert(false && "Unsupported logical binary operator.");
      return nullptr;
  }
}

void CreateBranchIfNeeded(llvm::BasicBlock *target) {
  auto *insertBlock = Visitor::Builder->GetInsertBlock();
  if (insertBlock != nullptr && insertBlock->getTerminator() == nullptr) {
    Visitor::Builder->CreateBr(target);
  }
}

void EmitScope(Visitor &visitor, std::vector<ASTNode> &scope) {
  for (auto &node : scope) {
    auto *insertBlock = Visitor::Builder->GetInsertBlock();
    if (insertBlock != nullptr && insertBlock->getTerminator() != nullptr) {
      break;
    }
    node->accept(visitor);
  }
}

SymbolInfo Visitor::getSymbolInfo(const std::string &symbolName) {
  SymbolInfo symbolInfo;

  bool flag = false;
  for (auto &symbol : LocalSymbolTable[m_LastFuncName]) {
    if (symbol.symbolName == symbolName) {
      symbolInfo = symbol.symbolInfo;
      flag = true;
      break;
    }
  }

  if (!flag) {
    for (auto &symbol : GlobalSymbolTable) {
      if (symbol.symbolName == symbolName) {
        symbolInfo = symbol.symbolInfo;
        flag = true;
        break;
      }
    }
  }

  return symbolInfo;
}

bool IsAnyBinOpOrSymbolInvolved(std::vector<BinOpOrVal> &v) {
  for (const auto &element : v) {
    if (element.isBinOp || element.isSymbol) {
      return true;
    }
  }

  return false;
}

llvm::Value *CreateInitializerValue(Visitor &visitor,
                                           const BinOpOrVal &element,
                                           llvm::Type *elementType) {
  if (element.isSymbol) {
    auto *symbolAst = reinterpret_cast<SymbolAST *>(element.address);
    return symbolAst->accept(visitor);
  }

  if (element.isBinOp) {
    auto *binaryAst = reinterpret_cast<BinaryOpAST *>(element.address);
    return visitor.createBinaryOp(*binaryAst);
  }

  return CreateLiteralConstant(elementType, element.value);
}

llvm::Value *CreateArrayElementAddress(llvm::Type *arrayType,
                                              llvm::Value *storage,
                                              size_t index) {
  auto *zero = llvm::ConstantInt::get(Visitor::Builder->getInt64Ty(), 0);
  auto *elementIndex =
      llvm::ConstantInt::get(Visitor::Builder->getInt64Ty(), index);
  return Visitor::Builder->CreateInBoundsGEP(arrayType, storage,
                                             {zero, elementIndex});
}

llvm::Type *GetType(TypeSpecifier typeSpecifier, size_t indirectLevel,
                           bool isRef) {
  llvm::Type *type = nullptr;
  switch (typeSpecifier) {
    case SPEC_VOID:
      if (indirectLevel == 0)
        type = Visitor::Builder->getVoidTy();
      else
        type = GetI8PtrTy();
      break;
    case SPEC_I8:
      if (indirectLevel == 0)
        type = Visitor::Builder->getInt8Ty();
      else
        type = llvm::IntegerType::getInt8Ty(Visitor::Module->getContext());
      break;
    case SPEC_I16:
      if (indirectLevel == 0)
        type = Visitor::Builder->getInt16Ty();
      else
        type = llvm::IntegerType::getInt16Ty(Visitor::Module->getContext());
      break;
    case SPEC_I32:
      if (indirectLevel == 0)
        type = Visitor::Builder->getInt32Ty();
      else
        type = llvm::IntegerType::getInt32Ty(Visitor::Module->getContext());
      break;
    case SPEC_I64:
      if (indirectLevel == 0)
        type = Visitor::Builder->getInt64Ty();
      else
        type = llvm::IntegerType::getInt64Ty(Visitor::Module->getContext());
      break;
    case SPEC_INT:
      if (indirectLevel == 0)
#if defined(_M_X64) || defined(__amd64__)
        type = Visitor::Builder->getInt64Ty();
#else
        type = Visitor::Builder->getInt32Ty();
#endif
      else
#if defined(_M_X64) || defined(__amd64__)
        type = llvm::IntegerType::getInt64Ty(Visitor::Module->getContext());
#else
        type = llvm::IntegerType::getInt32Ty(Visitor::Module->getContext());
#endif
      break;
    case SPEC_U8:
      if (indirectLevel == 0)
        type = Visitor::Builder->getInt8Ty();
      else
        type = llvm::IntegerType::getInt8Ty(Visitor::Module->getContext());
      break;
    case SPEC_U16:
      if (indirectLevel == 0)
        type = Visitor::Builder->getInt16Ty();
      else
        type = llvm::IntegerType::getInt16Ty(Visitor::Module->getContext());
      break;
    case SPEC_U32:
      if (indirectLevel == 0)
        type = Visitor::Builder->getInt32Ty();
      else
        type = llvm::IntegerType::getInt32Ty(Visitor::Module->getContext());
      break;
    case SPEC_U64:
      if (indirectLevel == 0)
        type = Visitor::Builder->getInt64Ty();
      else
        type = llvm::IntegerType::getInt64Ty(Visitor::Module->getContext());
      break;
    case SPEC_U128:
      if (indirectLevel == 0)
        type = Visitor::Builder->getInt128Ty();
      else
        type = llvm::IntegerType::getInt128Ty(Visitor::Module->getContext());
      break;
    case SPEC_UINT:
      if (indirectLevel == 0)
#if defined(_M_X64) || defined(__amd64__)
        type = Visitor::Builder->getInt128Ty();
#else
        type = Visitor::Builder->getInt64Ty();
#endif
      else
#if defined(_M_X64) || defined(__amd64__)
        type = llvm::IntegerType::getInt128Ty(Visitor::Module->getContext());
#else
        type = llvm::IntegerType::getInt128Ty(Visitor::Module->getContext());
#endif
      break;
    case SPEC_ISIZE:
      if (indirectLevel == 0)
#if defined(_M_X64) || defined(__amd64__)
        type = Visitor::Builder->getInt64Ty();
#else
        type = Visitor::Builder->getInt32Ty();
#endif
      else
#if defined(_M_X64) || defined(__amd64__)
        type = llvm::IntegerType::getInt64Ty(Visitor::Module->getContext());
#else
        type = llvm::IntegerType::getInt64Ty(Visitor::Module->getContext());
#endif
      break;
    case SPEC_USIZE:
      if (indirectLevel == 0)
#if defined(_M_X64) || defined(__amd64__)
        type = Visitor::Builder->getInt128Ty();
#else
        type = Visitor::Builder->getInt64Ty();
#endif
      else
#if defined(_M_X64) || defined(__amd64__)
        type = llvm::IntegerType::getInt128Ty(Visitor::Module->getContext());
#else
        type = llvm::IntegerType::getInt64Ty(Visitor::Module->getContext());
#endif
      break;
    case SPEC_F32:
      if (indirectLevel == 0)
        type = Visitor::Builder->getFloatTy();
      else
        type = llvm::Type::getFloatTy(Visitor::Module->getContext());
      break;
    case SPEC_F64:
      if (indirectLevel == 0)
        type = Visitor::Builder->getDoubleTy();
      else
        type = llvm::Type::getDoubleTy(Visitor::Module->getContext());
      break;
    case SPEC_FLOAT:
      if (indirectLevel == 0)
#if defined(_M_X64) || defined(__amd64__)
        type = Visitor::Builder->getDoubleTy();
#else
        type = Visitor::Builder->getFloatTy();
#endif
      else
#if defined(_M_X64) || defined(__amd64__)
        type = llvm::Type::getDoubleTy(Visitor::Module->getContext());
#else
        type = llvm::Type::getFloatTy(Visitor::Module->getContext());
#endif
      break;
    case SPEC_CHAR:
      if (indirectLevel == 0)
        type = Visitor::Builder->getInt8Ty();
      else
        type = llvm::IntegerType::getInt8Ty(Visitor::Module->getContext());
      break;
    case SPEC_UCHAR:
      if (indirectLevel == 0)
        type = Visitor::Builder->getInt8Ty();
      else
        type = llvm::IntegerType::getInt8Ty(Visitor::Module->getContext());
      break;
    case SPEC_BOOL:
      if (indirectLevel == 0)
        type = Visitor::Builder->getInt8Ty();
      else
        type = llvm::IntegerType::getInt8Ty(Visitor::Module->getContext());
      break;
    case SPEC_VEC2:
      break;
    case SPEC_VEC3:
      break;
    case SPEC_VEC4:
      break;
    case SPEC_NIL:
      break;
    case SPEC_DEFINED:
      break;
  }

  if (indirectLevel > 0) {
    type = llvm::PointerType::get(Visitor::Builder->getContext(), 0);
  }

  return type;
}

Visitor::FunctionParamLayout Visitor::buildFunctionParamLayout(FuncAST &funcAst,
                                                               bool nativeAbi) {
  FunctionParamLayout layout;
  layout.irTypes.reserve(funcAst.m_Params.size());
  layout.valueTypes.reserve(funcAst.m_Params.size());
  layout.names.reserve(funcAst.m_Params.size());
  layout.isReference.reserve(funcAst.m_Params.size());
  layout.isArray.reserve(funcAst.m_Params.size());

  for (auto &param : funcAst.m_Params) {
    auto *paramAst = static_cast<ParamAST *>(param.get());
    if (paramAst->m_TypeAmbiguous) {
      continue;
    }

    auto *typeAst = static_cast<TypeAST *>(paramAst->m_TypeNode.get());
    const auto definedTypeName = DefinedTypeNameFromTypeAst(typeAst);
    auto *valueType =
        nativeAbi && typeAst->m_IndirectLevel > 0
            ? GetType(typeAst->m_TypeSpec, typeAst->m_IndirectLevel,
                      typeAst->m_IsRef)
            : GetStorageType(typeAst->m_TypeSpec,
                             typeAst->m_IndirectLevel,
                             typeAst->m_IsUniquePtr,
                             typeAst->m_IsRef, definedTypeName);
    auto *irType = typeAst->m_IsRef
                       ? llvm::PointerType::get(Visitor::Builder->getContext(), 0)
                       : valueType;

    if (paramAst->m_IsSubscriptable) {
      valueType = GetArrayType(valueType, paramAst->m_ArrDim);
      irType = llvm::PointerType::get(Visitor::Builder->getContext(), 0);
    }

    layout.valueTypes.push_back(valueType);
    layout.irTypes.push_back(irType);
    layout.isReference.push_back(typeAst->m_IsRef);
    layout.isArray.push_back(paramAst->m_IsSubscriptable);

    if (paramAst->m_Symbol0 != nullptr) {
      auto *symbolAst = static_cast<SymbolAST *>(paramAst->m_Symbol0.get());
      layout.names.push_back(symbolAst->m_SymbolName);
    } else {
      layout.names.emplace_back();
    }
  }

  return layout;
}
