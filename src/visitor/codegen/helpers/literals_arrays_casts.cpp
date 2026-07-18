#include <visitor/codegen/codegen_private.hpp>

bool TryParseUnsignedIntegerLiteral(const std::string &literal,
                                           uint64_t &value) {
  try {
    size_t parsedChars = 0;
    value = std::stoull(literal, &parsedChars);
    return parsedChars == literal.size();
  } catch (...) {
    return false;
  }
}

bool TryParseSignedIntegerLiteral(const std::string &literal,
                                         int64_t &value) {
  try {
    size_t parsedChars = 0;
    value = std::stoll(literal, &parsedChars);
    return parsedChars == literal.size();
  } catch (...) {
    return false;
  }
}

llvm::Constant *CreateLiteralConstant(llvm::Type *type,
                                             const std::string &literal) {
  if (type->isDoubleTy() || type->isFloatTy()) {
    return llvm::ConstantFP::get(type, literal);
  }

  if (!literal.empty() && literal[0] == '-') {
    int64_t signedValue = 0;
    if (TryParseSignedIntegerLiteral(literal, signedValue)) {
      return llvm::ConstantInt::get(type, signedValue, true);
    }
  }

  uint64_t unsignedValue = 0;
  if (TryParseUnsignedIntegerLiteral(literal, unsignedValue)) {
    return llvm::ConstantInt::get(type, unsignedValue);
  }

  return llvm::ConstantInt::get(type, 0);
}

size_t FlatArrayLength(const std::vector<size_t> &dimensions) {
  size_t length = 1;
  for (auto dimension : dimensions) {
    if (dimension == 0 ||
        length > std::numeric_limits<size_t>::max() / dimension) {
      return 1;
    }
    length *= dimension;
  }
  return length;
}

llvm::Value *CastArrayIndex(llvm::Value *index) {
  if (index->getType()->isIntegerTy()) {
    return Visitor::Builder->CreateIntCast(index, Visitor::Builder->getInt64Ty(),
                                           true);
  }

  if (index->getType()->isFloatTy() || index->getType()->isDoubleTy()) {
    return Visitor::Builder->CreateFPToSI(index, Visitor::Builder->getInt64Ty());
  }

  return index;
}

llvm::Value *NormalizeArrayIndex(llvm::Value *index, size_t dimension) {
  index = CastArrayIndex(index);
  auto *indexType = Visitor::Builder->getInt64Ty();
  auto *dimensionValue =
      llvm::ConstantInt::get(indexType, static_cast<uint64_t>(dimension));

  if (auto *constant = llvm::dyn_cast<llvm::ConstantInt>(index)) {
    const auto signedValue = constant->getSExtValue();
    if (signedValue < 0) {
      return llvm::ConstantInt::get(indexType, signedValue +
                                                 static_cast<int64_t>(dimension));
    }
    return index;
  }

  auto *zero = llvm::ConstantInt::get(indexType, 0);
  auto *isNegative =
      CreateNormalizedICmp(llvm::CmpInst::ICMP_SLT, index, zero,
                           "array.index.neg");
  auto *fromEnd =
      Visitor::Builder->CreateAdd(index, dimensionValue, "array.index.fromend");
  return Visitor::Builder->CreateSelect(isNegative, fromEnd, index,
                                        "array.index.norm");
}

llvm::Value *CreateLinearArrayIndex(
    const std::vector<llvm::Value *> &indexes,
    const std::vector<size_t> &dimensions) {
  if (indexes.empty()) {
    return llvm::ConstantInt::get(Visitor::Builder->getInt64Ty(), 0);
  }

  const size_t usableDims = std::min(indexes.size(), dimensions.size());
  llvm::Value *linear =
      usableDims == 0
          ? CastArrayIndex(indexes.front())
          : NormalizeArrayIndex(indexes.front(), dimensions.front());
  for (size_t i = 1; i < usableDims; ++i) {
    auto *stride =
        llvm::ConstantInt::get(Visitor::Builder->getInt64Ty(), dimensions[i]);
    linear = Visitor::Builder->CreateMul(linear, stride, "array.stride");
    linear = Visitor::Builder->CreateAdd(
        linear, NormalizeArrayIndex(indexes[i], dimensions[i]), "array.index");
  }

  return linear;
}

llvm::Type *GetPointeeType(llvm::Value *value) {
  if (auto *alloca = llvm::dyn_cast<llvm::AllocaInst>(value)) {
    return alloca->getAllocatedType();
  }

  if (auto *global = llvm::dyn_cast<llvm::GlobalVariable>(value)) {
    return global->getValueType();
  }

  return value->getType();
}

bool IsSigned(TypeSpecifier typeSpecifier) {
  switch (typeSpecifier) {
    case SPEC_U8:
    case SPEC_U16:
    case SPEC_U32:
    case SPEC_U64:
    case SPEC_U128:
    case SPEC_UINT:
    case SPEC_USIZE:
      return false;
    default:
      return true;
  }
}

llvm::Value *CreateOneValue(llvm::Type *type) {
  if (type->isFloatingPointTy()) {
    return llvm::ConstantFP::get(type, 1.0);
  }

  return llvm::ConstantInt::get(type, 1);
}

std::string DecodeCStringEscapes(const std::string &value) {
  std::string decoded;
  decoded.reserve(value.size());

  for (size_t i = 0; i < value.size(); ++i) {
    if (value[i] != '\\' || i + 1 >= value.size()) {
      decoded += value[i];
      continue;
    }

    const char escaped = value[++i];
    switch (escaped) {
      case 'n':
        decoded += '\n';
        break;
      case 'r':
        decoded += '\r';
        break;
      case 't':
        decoded += '\t';
        break;
      case '\\':
        decoded += '\\';
        break;
      case '"':
        decoded += '"';
        break;
      case '0':
        decoded += '\0';
        break;
      default:
        decoded += '\\';
        decoded += escaped;
        break;
    }
  }

  return decoded;
}

llvm::Constant *CreateGlobalStringPointer(llvm::StringRef value,
                                                 const llvm::Twine &name) {
  auto *globalString = Visitor::Builder->CreateGlobalString(value, name);
  auto *zero = llvm::ConstantInt::get(Visitor::Builder->getInt32Ty(), 0);
  llvm::Constant *indices[] = {zero, zero};
  return llvm::ConstantExpr::getInBoundsGetElementPtr(
      globalString->getValueType(), globalString, indices);
}

llvm::Value *CastValueToType(llvm::Value *value, llvm::Type *targetType,
                                    bool isSigned) {
  if (value == nullptr || targetType == nullptr ||
      value->getType() == targetType) {
    return value;
  }

  llvm::Type *sourceType = value->getType();
  if (sourceType->isIntegerTy() && targetType->isIntegerTy()) {
    return Visitor::Builder->CreateIntCast(value, targetType, isSigned);
  }

  if (sourceType->isFloatingPointTy() && targetType->isFloatingPointTy()) {
    return Visitor::Builder->CreateFPCast(value, targetType);
  }

  if (sourceType->isIntegerTy() && targetType->isFloatingPointTy()) {
    return isSigned ? Visitor::Builder->CreateSIToFP(value, targetType)
                    : Visitor::Builder->CreateUIToFP(value, targetType);
  }

  if (sourceType->isFloatingPointTy() && targetType->isIntegerTy()) {
    return isSigned ? Visitor::Builder->CreateFPToSI(value, targetType)
                    : Visitor::Builder->CreateFPToUI(value, targetType);
  }

  if (sourceType->isPointerTy() && targetType->isPointerTy()) {
    return Visitor::Builder->CreatePointerCast(value, targetType);
  }

  return value;
}

llvm::Value *UnsafeCastValueToType(llvm::Value *value,
                                          llvm::Type *targetType,
                                          bool isSigned) {
  if (value == nullptr || targetType == nullptr ||
      value->getType() == targetType) {
    return value;
  }

  auto *sourceType = value->getType();
  if (sourceType->isPointerTy() && targetType->isIntegerTy()) {
    return Visitor::Builder->CreatePtrToInt(value, targetType);
  }

  if (sourceType->isIntegerTy() && targetType->isPointerTy()) {
    return Visitor::Builder->CreateIntToPtr(value, targetType);
  }

  if (sourceType->isPointerTy() && targetType->isPointerTy()) {
    return Visitor::Builder->CreatePointerCast(value, targetType);
  }

  return CastValueToType(value, targetType, isSigned);
}

llvm::Value *CastValueToBranchCondition(llvm::Value *value,
                                               bool isSigned) {
  if (value == nullptr) {
    return nullptr;
  }

  if (value->getType()->isIntegerTy(1)) {
    return value;
  }

  if (value->getType()->isIntegerTy()) {
    auto *zero = llvm::ConstantInt::get(value->getType(), 0);
    return CreateNormalizedICmp(llvm::CmpInst::ICMP_NE, value, zero,
                                "ifcond", isSigned);
  }

  if (value->getType()->isFloatingPointTy()) {
    auto *zero = llvm::ConstantFP::get(value->getType(), 0.0);
    return Visitor::Builder->CreateFCmpONE(value, zero, "ifcond");
  }

  if (value->getType()->isPointerTy()) {
    auto *nullPtr = llvm::ConstantPointerNull::get(
        llvm::cast<llvm::PointerType>(value->getType()));
    return CreateNormalizedICmp(llvm::CmpInst::ICMP_NE, value, nullPtr,
                                "ifcond");
  }

  if (IsSharedPointerTy(value->getType())) {
    auto *data = ExtractSharedPointerData(value);
    auto *nullPtr = llvm::ConstantPointerNull::get(GetI8PtrTy());
    return CreateNormalizedICmp(llvm::CmpInst::ICMP_NE, data, nullPtr,
                                "ifcond");
  }

  return value;
}
