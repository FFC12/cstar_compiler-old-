#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>

#include <ast/assignment_ast.hpp>
#include <ast/ast.hpp>
#include <ast/binary_op_ast.hpp>
#include <ast/cast_op_ast.hpp>
#include <ast/control_flow_ast.hpp>
#include <ast/enum_ast.hpp>
#include <ast/fix_ast.hpp>
#include <ast/func_ast.hpp>
#include <ast/func_call_ast.hpp>
#include <ast/if_stmt.hpp>
#include <ast/loop_stmt.hpp>
#include <ast/new_ast.hpp>
#include <ast/option_stmt.hpp>
#include <ast/param_ast.hpp>
#include <ast/ret_ast.hpp>
#include <ast/scalar_ast.hpp>
#include <ast/struct_ast.hpp>
#include <ast/symbol_ast.hpp>
#include <ast/trait_ast.hpp>
#include <ast/type_ast.hpp>
#include <ast/unary_op_ast.hpp>
#include <ast/var_ast.hpp>
#include <codegen/native_runtime.hpp>
#include <visitor/visitor.hpp>

llvm::BasicBlock *Visitor::MainFuncBB = nullptr;
llvm::GlobalVariable *Visitor::LastGlobVarRef = nullptr;

SymbolAST *Visitor::symbolFromMoveSource(IAST *node) const {
  if (node == nullptr) {
    return nullptr;
  }

  if (node->m_ExprKind == ExprKind::SymbolExpr) {
    return static_cast<SymbolAST *>(node);
  }

  if (node->m_ExprKind == ExprKind::UnaryOp) {
    auto *unary = static_cast<UnaryOpAST *>(node);
    if (unary->m_UnaryOpKind == U_MOVE &&
        unary->m_Node->m_ExprKind == ExprKind::SymbolExpr) {
      return static_cast<SymbolAST *>(unary->m_Node.get());
    }
  }

  return nullptr;
}

static llvm::PointerType *GetI8PtrTy() {
  return llvm::PointerType::get(Visitor::Builder->getContext(), 0);
}

static llvm::Type *GetType(TypeSpecifier typeSpecifier, size_t indirectLevel,
                           bool isRef = false);

static llvm::StructType *GetSharedPointerTy() {
  auto &ctx = Visitor::Builder->getContext();
  return llvm::StructType::get(ctx, {GetI8PtrTy(), GetI8PtrTy()});
}

static std::string DefinedTypeNameFromTypeAst(TypeAST *typeAst) {
  if (typeAst == nullptr || typeAst->typeSpec() != TypeSpecifier::SPEC_DEFINED ||
      typeAst->symbol() == nullptr ||
      typeAst->symbol()->getExprKind() != ExprKind::SymbolExpr) {
    return {};
  }

  auto *symbol = static_cast<SymbolAST *>(typeAst->symbol().get());
  return symbol->name();
}

static std::string DefinedTypeNameFromVarAst(VarAST &varAst) {
  if (varAst.typeSpec() != TypeSpecifier::SPEC_DEFINED ||
      varAst.typeName() == nullptr ||
      varAst.typeName()->getExprKind() != ExprKind::SymbolExpr) {
    return {};
  }

  auto *symbol = static_cast<SymbolAST *>(varAst.typeName().get());
  return symbol->name();
}

static std::string ValueOperatorMethodName(BinOpKind kind) {
  switch (kind) {
    case B_ADD:
      return "operator+";
    case B_SUB:
      return "operator-";
    case B_MUL:
      return "operator*";
    case B_DIV:
      return "operator/";
    case B_MOD:
      return "operator%";
    case B_EQ:
      return "operator==";
    case B_NEQ:
      return "operator!=";
    case B_LT:
      return "operator<";
    case B_LTEQ:
      return "operator<=";
    case B_GT:
      return "operator>";
    case B_GTEQ:
      return "operator>=";
    default:
      return {};
  }
}

static llvm::Type *GetStorageType(TypeSpecifier typeSpecifier,
                                  size_t indirectLevel, bool isUnique,
                                  bool isRef,
                                  const std::string &definedTypeName);

static llvm::Type *GetStructFieldLLVMType(const StructFieldInfo &field);

static llvm::StructType *GetDefinedStructTy(const std::string &name) {
  auto existing = Visitor::LLVMStructTypes.find(name);
  if (existing != Visitor::LLVMStructTypes.end()) {
    return existing->second;
  }

  auto structIt = Visitor::StructTable.find(name);
  if (structIt == Visitor::StructTable.end()) {
    return nullptr;
  }

  auto *structType =
      llvm::StructType::create(Visitor::Builder->getContext(), name);
  Visitor::LLVMStructTypes[name] = structType;

  std::vector<llvm::Type *> fieldTypes;
  fieldTypes.reserve(structIt->second.fields.size());
  for (const auto &field : structIt->second.fields) {
    fieldTypes.push_back(GetStructFieldLLVMType(field));
  }
  structType->setBody(fieldTypes, false);
  return structType;
}

static TypeSpecifier GetDefinedStorageType(const std::string &definedTypeName) {
  auto enumIt = Visitor::EnumTable.find(definedTypeName);
  if (enumIt != Visitor::EnumTable.end()) {
    return enumIt->second.underlyingType;
  }

  return TypeSpecifier::SPEC_DEFINED;
}

static TypeSpecifier GetEffectiveStorageType(TypeSpecifier typeSpecifier,
                                             const std::string &definedTypeName) {
  if (typeSpecifier == TypeSpecifier::SPEC_DEFINED) {
    return GetDefinedStorageType(definedTypeName);
  }

  return typeSpecifier;
}

static const EnumMemberInfo *FindEnumMember(const std::string &enumName,
                                            const std::string &memberName) {
  auto enumIt = Visitor::EnumTable.find(enumName);
  if (enumIt == Visitor::EnumTable.end()) {
    return nullptr;
  }

  auto memberIt = enumIt->second.memberIndexes.find(memberName);
  if (memberIt == enumIt->second.memberIndexes.end()) {
    return nullptr;
  }

  return &enumIt->second.members[memberIt->second];
}

static llvm::Value *ExtractSharedPointerData(llvm::Value *handle);
static llvm::Value *ExtractSharedPointerCount(llvm::Value *handle);

static bool IsSharedPointerTy(llvm::Type *type) {
  return type == GetSharedPointerTy();
}

static bool IsSharedPointerSymbol(const SymbolInfo &symbolInfo) {
  if (symbolInfo.type == TypeSpecifier::SPEC_CHAR ||
      symbolInfo.type == TypeSpecifier::SPEC_UCHAR) {
    return false;
  }
  return symbolInfo.indirectionLevel > 0 && !symbolInfo.isUnique &&
         !symbolInfo.isRef;
}

static llvm::Type *GetStorageType(TypeSpecifier typeSpecifier,
                                  size_t indirectLevel, bool isUnique,
                                  bool isRef = false) {
  if ((typeSpecifier == TypeSpecifier::SPEC_CHAR ||
       typeSpecifier == TypeSpecifier::SPEC_UCHAR) &&
      indirectLevel > 0) {
    return GetType(typeSpecifier, indirectLevel, isRef);
  }

  if (indirectLevel > 0 && !isUnique && !isRef) {
    return GetSharedPointerTy();
  }

  return GetType(typeSpecifier, indirectLevel, isRef);
}

static llvm::Type *GetStorageType(TypeSpecifier typeSpecifier,
                                  size_t indirectLevel, bool isUnique,
                                  bool isRef,
                                  const std::string &definedTypeName) {
  if (typeSpecifier == TypeSpecifier::SPEC_DEFINED && indirectLevel == 0) {
    auto enumIt = Visitor::EnumTable.find(definedTypeName);
    if (enumIt != Visitor::EnumTable.end()) {
      return GetType(enumIt->second.underlyingType, 0, false);
    }

    return GetDefinedStructTy(definedTypeName);
  }

  return GetStorageType(typeSpecifier, indirectLevel, isUnique, isRef);
}

static llvm::Type *GetStructFieldLLVMType(const StructFieldInfo &field) {
  return GetStorageType(field.type, field.indirectionLevel, field.isUnique,
                        field.isRef, field.definedTypeName);
}

static llvm::Type *GetSymbolLLVMType(const SymbolInfo &symbolInfo) {
  return GetStorageType(symbolInfo.type, symbolInfo.indirectionLevel,
                        symbolInfo.isUnique, symbolInfo.isRef,
                        symbolInfo.definedTypeName);
}

static llvm::ArrayType *GetArrayType(llvm::Type *elementType,
                                     const std::vector<ASTNode> &dimensions) {
  size_t length = 1;
  for (const auto &dim : dimensions) {
    auto *scalar = dynamic_cast<ScalarOrLiteralAST *>(dim.get());
    if (scalar == nullptr) {
      assert(false && "Only constant array parameter dimensions are supported.");
    }
    length *= std::stoull(scalar->getValue());
  }

  return llvm::ArrayType::get(elementType, length);
}

static llvm::Value *CastArrayIndex(llvm::Value *index) {
  if (index->getType()->isIntegerTy()) {
    return Visitor::Builder->CreateIntCast(index, Visitor::Builder->getInt64Ty(),
                                           true);
  }

  if (index->getType()->isFloatTy() || index->getType()->isDoubleTy()) {
    return Visitor::Builder->CreateFPToSI(index, Visitor::Builder->getInt64Ty());
  }

  return index;
}

static llvm::Value *NormalizeArrayIndex(llvm::Value *index, size_t dimension) {
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
  auto *isNegative = Visitor::Builder->CreateICmpSLT(index, zero,
                                                     "array.index.neg");
  auto *fromEnd =
      Visitor::Builder->CreateAdd(index, dimensionValue, "array.index.fromend");
  return Visitor::Builder->CreateSelect(isNegative, fromEnd, index,
                                        "array.index.norm");
}

static llvm::Value *CreateLinearArrayIndex(
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

static llvm::Type *GetPointeeType(llvm::Value *value) {
  if (auto *alloca = llvm::dyn_cast<llvm::AllocaInst>(value)) {
    return alloca->getAllocatedType();
  }

  if (auto *global = llvm::dyn_cast<llvm::GlobalVariable>(value)) {
    return global->getValueType();
  }

  return value->getType();
}

static bool IsSigned(TypeSpecifier typeSpecifier) {
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

static llvm::Value *CreateOneValue(llvm::Type *type) {
  if (type->isFloatingPointTy()) {
    return llvm::ConstantFP::get(type, 1.0);
  }

  return llvm::ConstantInt::get(type, 1);
}

static std::string DecodeCStringEscapes(const std::string &value) {
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

static llvm::Constant *CreateGlobalStringPointer(llvm::StringRef value,
                                                 const llvm::Twine &name = "") {
  auto *globalString = Visitor::Builder->CreateGlobalString(value, name);
  auto *zero = llvm::ConstantInt::get(Visitor::Builder->getInt32Ty(), 0);
  llvm::Constant *indices[] = {zero, zero};
  return llvm::ConstantExpr::getInBoundsGetElementPtr(
      globalString->getValueType(), globalString, indices);
}

static llvm::Value *CastValueToType(llvm::Value *value, llvm::Type *targetType,
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

static llvm::Value *UnsafeCastValueToType(llvm::Value *value,
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

static llvm::Value *CastValueToBranchCondition(llvm::Value *value,
                                               bool isSigned) {
  if (value == nullptr) {
    return nullptr;
  }

  if (value->getType()->isIntegerTy(1)) {
    return value;
  }

  if (value->getType()->isIntegerTy()) {
    auto *zero = llvm::ConstantInt::get(value->getType(), 0);
    return Visitor::Builder->CreateICmpNE(value, zero, "ifcond");
  }

  if (value->getType()->isFloatingPointTy()) {
    auto *zero = llvm::ConstantFP::get(value->getType(), 0.0);
    return Visitor::Builder->CreateFCmpONE(value, zero, "ifcond");
  }

  if (value->getType()->isPointerTy()) {
    auto *nullPtr = llvm::ConstantPointerNull::get(
        llvm::cast<llvm::PointerType>(value->getType()));
    return Visitor::Builder->CreateICmpNE(value, nullPtr, "ifcond");
  }

  if (IsSharedPointerTy(value->getType())) {
    auto *data = ExtractSharedPointerData(value);
    auto *nullPtr = llvm::ConstantPointerNull::get(GetI8PtrTy());
    return Visitor::Builder->CreateICmpNE(data, nullPtr, "ifcond");
  }

  return value;
}

static llvm::Value *CreateIntegerOrPointerCompare(
    llvm::CmpInst::Predicate signedPredicate,
    llvm::CmpInst::Predicate unsignedPredicate, llvm::Value *lhs,
    llvm::Value *rhs, bool isSigned, const llvm::Twine &name) {
  auto predicate = isSigned ? signedPredicate : unsignedPredicate;
  return Visitor::Builder->CreateICmp(predicate, lhs, rhs, name);
}

static llvm::Value *CreateOrderedCompare(llvm::CmpInst::Predicate floatPredicate,
                                         llvm::CmpInst::Predicate signedPredicate,
                                         llvm::CmpInst::Predicate unsignedPredicate,
                                         llvm::Value *lhs, llvm::Value *rhs,
                                         llvm::Type *valueType, bool isSigned,
                                         const llvm::Twine &name) {
  if (valueType->isFloatTy() || valueType->isDoubleTy()) {
    return Visitor::Builder->CreateFCmp(floatPredicate, lhs, rhs, name);
  }

  return CreateIntegerOrPointerCompare(signedPredicate, unsignedPredicate, lhs,
                                       rhs, isSigned, name);
}

static llvm::Value *CreateEqualityCompare(bool equals, llvm::Value *lhs,
                                          llvm::Value *rhs,
                                          llvm::Type *valueType,
                                          const llvm::Twine &name) {
  if (valueType->isFloatTy() || valueType->isDoubleTy()) {
    return Visitor::Builder->CreateFCmp(
        equals ? llvm::CmpInst::FCMP_OEQ : llvm::CmpInst::FCMP_ONE, lhs, rhs,
        name);
  }

  return Visitor::Builder->CreateICmp(equals ? llvm::CmpInst::ICMP_EQ
                                             : llvm::CmpInst::ICMP_NE,
                                      lhs, rhs, name);
}

static bool IsFloatingPointType(llvm::Type *type) {
  return type != nullptr && (type->isFloatTy() || type->isDoubleTy());
}

static llvm::Value *CreateArithmeticValue(BinOpKind op, llvm::Value *lhs,
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

static llvm::Value *CreateLogicalValue(BinOpKind op, llvm::Value *lhs,
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

static void CreateBranchIfNeeded(llvm::BasicBlock *target) {
  auto *insertBlock = Visitor::Builder->GetInsertBlock();
  if (insertBlock != nullptr && insertBlock->getTerminator() == nullptr) {
    Visitor::Builder->CreateBr(target);
  }
}

static void EmitScope(Visitor &visitor, std::vector<ASTNode> &scope) {
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

static bool IsAnyBinOpOrSymbolInvolved(std::vector<BinOpOrVal> &v) {
  bool flag = false;

  for (auto &e : v) {
    if (e.isBinOp || e.isSymbol) {
      flag = true;
      break;
    }
  }

  return flag;
}

static llvm::Type *GetType(TypeSpecifier typeSpecifier, size_t indirectLevel,
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

Visitor::FunctionParamLayout Visitor::buildFunctionParamLayout(FuncAST &funcAst) {
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
    auto *valueType = GetStorageType(typeAst->m_TypeSpec,
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

struct AssignmentTarget {
  llvm::Value *address = nullptr;
  llvm::Type *valueType = nullptr;
};

static AssignmentTarget ResolveAssignmentTarget(llvm::Value *storage,
                                                const SymbolInfo &symbolInfo,
                                                size_t derefLevel) {
  AssignmentTarget target;

  if (derefLevel == 0) {
    target.address = storage;
    target.valueType = GetPointeeType(storage);
    return target;
  }

  if (derefLevel > symbolInfo.indirectionLevel) {
    assert(false && "Invalid dereference level for assignment target.");
  }

  if (IsSharedPointerSymbol(symbolInfo)) {
    llvm::Value *handle = Visitor::Builder->CreateLoad(GetSharedPointerTy(),
                                                       storage,
                                                       "deref.assign.sp");
    for (size_t level = 1; level <= derefLevel; ++level) {
      auto *data = ExtractSharedPointerData(handle);
      const size_t remaining = symbolInfo.indirectionLevel - level;
      if (remaining > 0) {
        if (level == derefLevel) {
          target.address = data;
          target.valueType = GetSharedPointerTy();
          return target;
        }
        handle = Visitor::Builder->CreateLoad(GetSharedPointerTy(), data,
                                              "deref.assign.sp");
      } else {
        target.address = data;
        target.valueType = GetType(symbolInfo.type, 0);
        return target;
      }
    }
  }

  auto *pointerType = GetType(symbolInfo.type, symbolInfo.indirectionLevel);
  target.address =
      Visitor::Builder->CreateLoad(pointerType, storage, "deref.assign.ptr");

  for (size_t level = 1; level < derefLevel; ++level) {
    const size_t remainingIndirection = symbolInfo.indirectionLevel - level;
    auto *intermediateType = GetType(symbolInfo.type, remainingIndirection);
    target.address = Visitor::Builder->CreateLoad(intermediateType,
                                                  target.address,
                                                  "deref.assign.ptr");
  }

  target.valueType =
      GetType(symbolInfo.type, symbolInfo.indirectionLevel - derefLevel);
  return target;
}

static llvm::Value *CreateShortcutAssignmentValue(llvm::Value *currentValue,
                                                  llvm::Value *rhs,
                                                  llvm::Type *targetType,
                                                  ShortcutOp shortcutOp,
                                                  bool isSigned) {
  switch (shortcutOp) {
    case ShortcutOp::S_PLUS:
      return targetType->isFloatingPointTy()
                 ? Visitor::Builder->CreateFAdd(currentValue, rhs, "addeqtmp")
                 : (isSigned ? Visitor::Builder->CreateNSWAdd(currentValue,
                                                               rhs, "addeqtmp")
                             : Visitor::Builder->CreateNUWAdd(currentValue,
                                                               rhs,
                                                               "addeqtmp"));
    case ShortcutOp::S_MIN:
      return targetType->isFloatingPointTy()
                 ? Visitor::Builder->CreateFSub(currentValue, rhs, "subeqtmp")
                 : (isSigned ? Visitor::Builder->CreateNSWSub(currentValue,
                                                               rhs, "subeqtmp")
                             : Visitor::Builder->CreateNUWSub(currentValue,
                                                               rhs,
                                                               "subeqtmp"));
    case ShortcutOp::S_STA:
      return targetType->isFloatingPointTy()
                 ? Visitor::Builder->CreateFMul(currentValue, rhs, "muleqtmp")
                 : (isSigned ? Visitor::Builder->CreateNSWMul(currentValue,
                                                               rhs, "muleqtmp")
                             : Visitor::Builder->CreateNUWMul(currentValue,
                                                               rhs,
                                                               "muleqtmp"));
    case ShortcutOp::S_DIV:
      return targetType->isFloatingPointTy()
                 ? Visitor::Builder->CreateFDiv(currentValue, rhs, "diveqtmp")
                 : (isSigned ? Visitor::Builder->CreateSDiv(currentValue, rhs,
                                                             "diveqtmp")
                             : Visitor::Builder->CreateUDiv(currentValue, rhs,
                                                             "diveqtmp"));
    case ShortcutOp::S_MOD:
      return targetType->isFloatingPointTy()
                 ? Visitor::Builder->CreateFRem(currentValue, rhs, "modeqtmp")
                 : (isSigned ? Visitor::Builder->CreateSRem(currentValue, rhs,
                                                             "modeqtmp")
                             : Visitor::Builder->CreateURem(currentValue, rhs,
                                                             "modeqtmp"));
    case ShortcutOp::S_SHR:
      return isSigned
                 ? Visitor::Builder->CreateAShr(currentValue, rhs, "shreqtmp")
                 : Visitor::Builder->CreateLShr(currentValue, rhs, "shreqtmp");
    case ShortcutOp::S_SHL:
      return Visitor::Builder->CreateShl(currentValue, rhs, "shleqtmp");
    case ShortcutOp::S_AND:
      return Visitor::Builder->CreateAnd(currentValue, rhs, "andeqtmp");
    case ShortcutOp::S_OR:
      return Visitor::Builder->CreateOr(currentValue, rhs, "oreqtmp");
    case ShortcutOp::S_XOR:
      return Visitor::Builder->CreateXor(currentValue, rhs, "xoreqtmp");
    case ShortcutOp::S_MOV:
    case ShortcutOp::S_NONE:
      return rhs;
  }

  return rhs;
}

static llvm::GlobalVariable *CreateConstantGlobalVar(
    const std::string &name, VisibilitySpecifier specifier, llvm::Type *type,
    llvm::Value *value) {
  Visitor::Module->getOrInsertGlobal(llvm::StringRef(name), type);
  auto globVar = Visitor::Module->getNamedGlobal(llvm::StringRef(name));

  auto *constant = llvm::dyn_cast<llvm::Constant>(value);
  if (!constant) {
    assert(false && "Impossible!");
  }

  auto dl = Visitor::Module->getDataLayout();
  auto align = dl.getPrefTypeAlign(type);
  globVar->setAlignment(align);

  if (specifier == VisibilitySpecifier::VIS_STATIC) {
    globVar->setLinkage(llvm::GlobalValue::InternalLinkage);
    globVar->setInitializer(constant);
  } else if (specifier == VisibilitySpecifier::VIS_DEFAULT) {
    // ConstantStruct
    globVar->setDSOLocal(true);
    globVar->setInitializer(constant);
  } else if (specifier == VisibilitySpecifier::VIS_EXPORT ||
             specifier == VisibilitySpecifier::VIS_IMPORT) {
    globVar->setDSOLocal(true);
    globVar->setLinkage(llvm::GlobalValue::ExternalLinkage);
  }

  return globVar;
}

static llvm::GlobalVariable *CreateZeroInitConstantGlobalVar(
    const std::string &name, VisibilitySpecifier specifier, llvm::Type *type) {
  Visitor::Module->getOrInsertGlobal(llvm::StringRef(name), type);
  auto globVar = Visitor::Module->getNamedGlobal(llvm::StringRef(name));

  llvm::Constant *constant = llvm::ConstantAggregateZero::get(type);
  if (!constant) {
    assert(false && "Impossible!");
  }

  auto dl = Visitor::Module->getDataLayout();
  auto align = dl.getPrefTypeAlign(type);
  globVar->setAlignment(align);

  if (specifier == VisibilitySpecifier::VIS_STATIC) {
    globVar->setLinkage(llvm::GlobalValue::InternalLinkage);
    globVar->setInitializer(constant);
  } else if (specifier == VisibilitySpecifier::VIS_DEFAULT) {
    // ConstantStruct
    globVar->setDSOLocal(true);
    globVar->setInitializer(constant);
  } else if (specifier == VisibilitySpecifier::VIS_EXPORT ||
             specifier == VisibilitySpecifier::VIS_IMPORT) {
    globVar->setDSOLocal(true);
    globVar->setLinkage(llvm::GlobalValue::ExternalLinkage);
  }

  return globVar;
}

static llvm::GlobalVariable *CreateInitConstantGlobalVar(
    const std::string &name, VisibilitySpecifier specifier, llvm::Type *type,
    llvm::Constant *value) {
  Visitor::Module->getOrInsertGlobal(llvm::StringRef(name), type);
  auto globVar = Visitor::Module->getNamedGlobal(llvm::StringRef(name));

  auto dl = Visitor::Module->getDataLayout();
  auto align = dl.getPrefTypeAlign(type);
  globVar->setAlignment(align);

  if (specifier == VisibilitySpecifier::VIS_STATIC) {
    globVar->setLinkage(llvm::GlobalVariable::LinkageTypes::InternalLinkage);
    globVar->setInitializer(value);
  } else if (specifier == VisibilitySpecifier::VIS_DEFAULT) {
    // ConstantStruct
    globVar->setDSOLocal(true);
    globVar->setInitializer(value);
  } else if (specifier == VisibilitySpecifier::VIS_EXPORT ||
             specifier == VisibilitySpecifier::VIS_IMPORT) {
    globVar->setDSOLocal(true);
    globVar->setLinkage(llvm::GlobalVariable::LinkageTypes::ExternalLinkage);
  } else {
    globVar->setLinkage(llvm::GlobalVariable::LinkageTypes::PrivateLinkage);
    globVar->setUnnamedAddr(llvm::GlobalVariable::UnnamedAddr::Global);
    globVar->setInitializer(value);
  }

  return globVar;
}

// This will be added to .text.startup
static llvm::Function *CreateGlobalVarInitFunc() {
  llvm::Function *function = llvm::Function::Create(
      llvm::FunctionType::get(Visitor::Builder->getVoidTy(), false),
      llvm::Function::LinkageTypes::InternalLinkage, "__cstar_global_var_init",
      *Visitor::Module);
  function->setSection(".text.startup");

  llvm::BasicBlock *entry =
      llvm::BasicBlock::Create(Visitor::Builder->getContext(), "", function);

  Visitor::Builder->SetInsertPoint(entry);
  llvm::verifyFunction(*function);

  return function;
}

// This will be inserted and called before "main" function.
static llvm::Function *CreateGlobalFuncSubToMain(
    std::vector<llvm::StringRef> &initFuncs) {
  // sub for main (calling before main)
  llvm::Function *function = llvm::Function::Create(
      llvm::FunctionType::get(Visitor::Builder->getVoidTy(), false),
      llvm::Function::LinkageTypes::InternalLinkage,
      "__GLOBAL_cstar__sub__" + Visitor::Module->getName(), *Visitor::Module);
  function->setSection(".text.startup");

  auto insertPoint = Visitor::Builder->GetInsertBlock();

  llvm::BasicBlock *entry = llvm::BasicBlock::Create(
      Visitor::Builder->getContext(), "sub_entry", function);

  Visitor::Builder->SetInsertPoint(entry);

  std::vector<llvm::Value *> emptyArgs;
  for (auto &initFunc : initFuncs) {
    Visitor::Builder->CreateCall(Visitor::Module->getFunction(initFunc),
                                 emptyArgs);
  }

  Visitor::Builder->CreateRetVoid();
  Visitor::Builder->SetInsertPoint(insertPoint);

  return function;
}

static llvm::Value *CreateAlloca(const std::string &name, llvm::Type *type) {
  llvm::Function *parentFunc = Visitor::Builder->GetInsertBlock()->getParent();
  llvm::AllocaInst *var =
      Visitor::Builder->CreateAlloca(type, nullptr, llvm::Twine(name));

  return var;
}

static llvm::Value *CreateLocalVariable(const std::string &name,
                                        llvm::Type *type, llvm::Value *value) {
  llvm::Function *parentFunc = Visitor::Builder->GetInsertBlock()->getParent();

  auto *var = llvm::dyn_cast<llvm::AllocaInst>(CreateAlloca(name, type));
  Visitor::Builder->CreateStore(value, var);

  return var;
}

static llvm::FunctionCallee GetMallocFunction() {
  auto *intType = Visitor::Builder->getInt64Ty();
  auto *ptrType = GetI8PtrTy();
  auto *fnType = llvm::FunctionType::get(ptrType, {intType}, false);
  return Visitor::Module->getOrInsertFunction("malloc", fnType);
}

static llvm::FunctionCallee GetFreeFunction() {
  auto *ptrType = GetI8PtrTy();
  auto *fnType = llvm::FunctionType::get(Visitor::Builder->getVoidTy(),
                                         {ptrType}, false);
  return Visitor::Module->getOrInsertFunction("free", fnType);
}

static llvm::Value *CreateDefaultHeapAlloc(llvm::Type *type,
                                           const llvm::Twine &name) {
  auto size = Visitor::Module->getDataLayout().getTypeAllocSize(type);
  auto *sizeValue =
      llvm::ConstantInt::get(Visitor::Builder->getInt64Ty(), size);
  return Visitor::Builder->CreateCall(GetMallocFunction(), {sizeValue}, name);
}

static void CreateDefaultHeapFree(llvm::Value *ptr) {
  auto *asI8 = Visitor::Builder->CreatePointerCast(ptr, GetI8PtrTy());
  Visitor::Builder->CreateCall(GetFreeFunction(), {asI8});
}

static llvm::Value *CreateLoad(llvm::Value *address, llvm::Type *type,
                               const llvm::Twine &name = "") {
  return Visitor::Builder->CreateLoad(type, address, name);
}

static llvm::Value *CreateAtomicLoad(llvm::Value *address, llvm::Type *type,
                                     const llvm::Twine &name = "") {
  auto *load = Visitor::Builder->CreateLoad(type, address, name);
  auto dl = Visitor::Module->getDataLayout();
  load->setAtomic(llvm::AtomicOrdering::Acquire);
  load->setAlignment(dl.getPrefTypeAlign(type));
  return load;
}

static llvm::Value *CreateSharedPointerHandle(llvm::Value *data,
                                              llvm::Value *refCount) {
  auto *handleType = GetSharedPointerTy();
  llvm::Value *handle = llvm::UndefValue::get(handleType);
  data = Visitor::Builder->CreatePointerCast(data, GetI8PtrTy());
  refCount = Visitor::Builder->CreatePointerCast(refCount, GetI8PtrTy());
  handle = Visitor::Builder->CreateInsertValue(handle, data, {0}, "sp.data");
  handle = Visitor::Builder->CreateInsertValue(handle, refCount, {1}, "sp.count");
  return handle;
}

static llvm::Value *CreateNullSharedPointerHandle() {
  auto *nullPtr = llvm::ConstantPointerNull::get(GetI8PtrTy());
  return CreateSharedPointerHandle(nullPtr, nullPtr);
}

static llvm::Value *ExtractSharedPointerData(llvm::Value *handle) {
  return Visitor::Builder->CreateExtractValue(handle, {0}, "sp.data");
}

static llvm::Value *ExtractSharedPointerCount(llvm::Value *handle) {
  return Visitor::Builder->CreateExtractValue(handle, {1}, "sp.count");
}

static llvm::Value *CreateSharedPointerCounter(const std::string &name,
                                               uint64_t initialValue) {
  auto *counterType = Visitor::Builder->getInt64Ty();
  auto *counter = Visitor::Builder->CreateAlloca(
      counterType, nullptr, llvm::Twine(name + ".strong"));
  Visitor::Builder->CreateStore(
      llvm::ConstantInt::get(counterType, initialValue), counter);
  return counter;
}

static void AtomicBumpSharedPointer(llvm::Value *handle, int64_t delta) {
  auto *countAsI8 = ExtractSharedPointerCount(handle);
  auto *isLive = Visitor::Builder->CreateICmpNE(
      countAsI8, llvm::ConstantPointerNull::get(GetI8PtrTy()),
      "sp.count.live");
  auto *function = Visitor::Builder->GetInsertBlock()->getParent();
  auto *bumpBB = llvm::BasicBlock::Create(Visitor::Builder->getContext(),
                                          "sp.bump", function);
  auto *contBB = llvm::BasicBlock::Create(Visitor::Builder->getContext(),
                                          "sp.bump.cont", function);
  Visitor::Builder->CreateCondBr(isLive, bumpBB, contBB);
  Visitor::Builder->SetInsertPoint(bumpBB);

  auto *count = Visitor::Builder->CreatePointerCast(
      countAsI8, llvm::PointerType::get(Visitor::Builder->getContext(), 0));
  auto *amount = llvm::ConstantInt::get(Visitor::Builder->getInt64Ty(),
                                        static_cast<uint64_t>(delta), true);
  Visitor::Builder->CreateAtomicRMW(
      delta >= 0 ? llvm::AtomicRMWInst::Add : llvm::AtomicRMWInst::Sub, count,
      amount, llvm::MaybeAlign(), llvm::AtomicOrdering::AcquireRelease);
  Visitor::Builder->CreateBr(contBB);
  Visitor::Builder->SetInsertPoint(contBB);
}

static void RetainSharedPointer(llvm::Value *handle) {
  AtomicBumpSharedPointer(handle, 1);
}

static void ReleaseSharedPointer(llvm::Value *handle) {
  AtomicBumpSharedPointer(handle, -1);
}

static llvm::Value *LoadSharedPointerStrongCount(llvm::Value *handle) {
  auto *countAsI8 = ExtractSharedPointerCount(handle);
  auto *isLive = Visitor::Builder->CreateICmpNE(
      countAsI8, llvm::ConstantPointerNull::get(GetI8PtrTy()),
      "sp.count.live");
  auto *function = Visitor::Builder->GetInsertBlock()->getParent();
  auto *loadBB = llvm::BasicBlock::Create(Visitor::Builder->getContext(),
                                          "sp.count.load", function);
  auto *zeroBB = llvm::BasicBlock::Create(Visitor::Builder->getContext(),
                                          "sp.count.zero", function);
  auto *contBB = llvm::BasicBlock::Create(Visitor::Builder->getContext(),
                                          "sp.count.cont", function);
  Visitor::Builder->CreateCondBr(isLive, loadBB, zeroBB);

  Visitor::Builder->SetInsertPoint(loadBB);
  auto *count = Visitor::Builder->CreatePointerCast(
      countAsI8, llvm::PointerType::get(Visitor::Builder->getContext(), 0));
  auto *loaded = CreateAtomicLoad(count, Visitor::Builder->getInt64Ty(),
                                  "sp.strong");
  Visitor::Builder->CreateBr(contBB);
  loadBB = Visitor::Builder->GetInsertBlock();

  Visitor::Builder->SetInsertPoint(zeroBB);
  auto *zero = llvm::ConstantInt::get(Visitor::Builder->getInt64Ty(), 0);
  Visitor::Builder->CreateBr(contBB);
  zeroBB = Visitor::Builder->GetInsertBlock();

  Visitor::Builder->SetInsertPoint(contBB);
  auto *phi =
      Visitor::Builder->CreatePHI(Visitor::Builder->getInt64Ty(), 2,
                                  "sp.strong.result");
  phi->addIncoming(loaded, loadBB);
  phi->addIncoming(zero, zeroBB);
  return phi;
}

static llvm::Value *FindStorage(std::map<std::string, llvm::AllocaInst*> &locals,
                                std::map<std::string, llvm::GlobalVariable*> &globals,
                                const std::string &name) {
  if (locals.count(name) > 0) {
    return locals[name];
  }
  if (globals.count(name) > 0) {
    return globals[name];
  }
  return nullptr;
}

void Visitor::registerScopeDestructor(const SymbolInfo &symbolInfo) {
  if (symbolInfo.type != TypeSpecifier::SPEC_DEFINED ||
      symbolInfo.indirectionLevel != 0 || symbolInfo.isRef || symbolInfo.isGlob) {
    return;
  }

  if (FunctionTable.count(symbolInfo.definedTypeName + ".destructor") == 0) {
    return;
  }

  m_CodegenDroppedSymbols.erase(symbolInfo.symbolName);
  m_ScopeDestructors.push_back(symbolInfo);
}

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
    rhs = binaryOpAst.m_RHS->accept(*this);
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
      if (lhs == nullptr || rhs == nullptr || binaryOpAst.m_Extra == nullptr) {
        return nullptr;
      }
      auto extra = binaryOpAst.m_Extra->accept(*this);
      if (extra == nullptr) {
        return nullptr;
      }
      if (lhs->getType()->getIntegerBitWidth() != 1) {
        lhs = Builder->CreateIntCast(lhs, Builder->getInt1Ty(), false);
      }

      value = Builder->CreateSelect(lhs, rhs, extra, "selectmp");
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

ValuePtr Visitor::visit(SymbolAST &symbolAst) {
  llvm::Value *value = nullptr;
  bool isSigned = false;

  // if (this->m_LastVarDecl) {
  if (m_LocalVarsOnScope.count(symbolAst.m_SymbolName) == 0) {
    if (m_GlobalVars.count(symbolAst.m_SymbolName) == 0) {
      assert(false && "This should not be happened. _WTF_");
    } else {
      value = m_GlobalVars[symbolAst.m_SymbolName];
    }
  } else {
    value = m_LocalVarsOnScope[symbolAst.m_SymbolName];
  }

  llvm::Type *valueType = m_LastType;
  auto referenceParam = m_ReferenceParamValueTypes.find(symbolAst.m_SymbolName);
  if (referenceParam != m_ReferenceParamValueTypes.end() && !m_LastArrayIndex) {
    auto *referenceSlotType = GetPointeeType(value);
    auto *referencedAddress = CreateLoad(
        value, referenceSlotType, symbolAst.m_SymbolName + ".ref.addr");
    auto *referencedType = referenceParam->second;
    auto *loadedValue = CreateLoad(referencedAddress, referencedType,
                                   symbolAst.m_SymbolName + ".ref");

    auto symbolInfo = getSymbolInfo(symbolAst.m_SymbolName);
    isSigned = IsSigned(GetEffectiveStorageType(symbolInfo.type,
                                                symbolInfo.definedTypeName));
    if (m_LastType == nullptr) {
      m_LastType = referencedType;
      m_LastSigned = isSigned;
      return loadedValue;
    }

    value = CastValueToType(loadedValue, m_LastType, isSigned);
    m_LastSigned = isSigned;
    return value;
  }

  auto arrayParam = m_ArrayParamValueTypes.find(symbolAst.m_SymbolName);
  if (arrayParam != m_ArrayParamValueTypes.end() && m_LastArrayIndex) {
    auto *slotType = GetPointeeType(value);
    auto *arrayAddress =
        CreateLoad(value, slotType, symbolAst.m_SymbolName + ".array.addr");
    m_LastType = arrayParam->second;
    auto symbolInfo = getSymbolInfo(symbolAst.m_SymbolName);
    m_LastSigned = IsSigned(GetEffectiveStorageType(symbolInfo.type,
                                                    symbolInfo.definedTypeName));
    return arrayAddress;
  }

  if (this->m_LastVarDecl && !this->m_LastArrayIndex) {
    valueType = GetPointeeType(value);

    auto symbolInfo = getSymbolInfo(symbolAst.m_SymbolName);
    isSigned = IsSigned(GetEffectiveStorageType(symbolInfo.type,
                                                symbolInfo.definedTypeName));
    llvm::Type *type = value->getType();

    if (value->getType()->isPointerTy()) {
      type = GetPointeeType(value);
    }

    llvm::LoadInst *loadedVal = nullptr;
    if (m_LastGlobVar) {
      auto dl = Visitor::Module->getDataLayout();
      auto align = dl.getPrefTypeAlign(type);
      loadedVal = Builder->CreateAlignedLoad(
          type, value, align, llvm::Twine(""));
    } else {
      loadedVal = Builder->CreateLoad(type, value, llvm::Twine(""));
    }

    if (m_LastType == nullptr) {
      m_LastType = type;
      m_LastSigned = isSigned;
      return loadedVal;
    }

    if (valueType->isPointerTy()) {
      valueType = GetPointeeType(value);
    }

    if (type->getTypeID() != m_LastType->getTypeID()) {
      if (m_LastType->isFloatTy() || m_LastType->isDoubleTy()) {
        if (isSigned) {
          value = Builder->CreateSIToFP(loadedVal, m_LastType);
        } else {
          value = Builder->CreateUIToFP(loadedVal, m_LastType);
        }
      } else {
        if (isSigned) {
          value = Builder->CreateFPToSI(loadedVal, m_LastType);
        } else {
          value = Builder->CreateFPToUI(loadedVal, m_LastType);
        }
      }
    } else {
      value = loadedVal;
    }
  } else {
    if (!m_LastArrayIndex) {
      m_LastType = value->getType();
      if (m_LastType->isPointerTy()) {
        llvm::LoadInst *loadedVal = nullptr;
        llvm::Type *type = m_LastType = GetPointeeType(value);
        if (m_LastGlobVar) {
          auto dl = Visitor::Module->getDataLayout();
          auto align = dl.getPrefTypeAlign(type);
          loadedVal = Builder->CreateAlignedLoad(
              type, value, align, llvm::Twine(""));
        } else {
          loadedVal = Builder->CreateLoad(type, value, llvm::Twine(""));
        }
        value = loadedVal;
      }
    }
  }

  return value;
}

ValuePtr Visitor::visit(ScalarOrLiteralAST &scalarAst) {
  llvm::Value *value = nullptr;

  // TODO: m_LastType is always null for the binary operation which is not RHS
  // of any vardecl.
  if (m_LastType == nullptr) {
    m_LastType = scalarAst.m_IsFloat ? Builder->getDoubleTy()
                                     : Builder->getInt64Ty();
  }

  if (m_LastArrayIndex) {
    m_IndicesAsStr.emplace_back(scalarAst.m_Value, scalarAst.m_IsFloat);
    // }
    // if (!m_LastVarDecl) {
    if (scalarAst.m_IsFloat) {
      value = llvm::ConstantFP::get(Builder->getDoubleTy(), scalarAst.m_Value);
    } else {
      if (m_LastType != nullptr) {
        if (m_LastType->isDoubleTy() || m_LastType->isFloatTy()) {
          value = llvm::ConstantFP::get(m_LastType, scalarAst.m_Value);
        } else {
          auto scalarVal = std::stoll(scalarAst.m_Value);
          value = llvm::ConstantInt::get(Builder->getInt64Ty(), scalarVal);
        }
      }
    }
  } else {
    if (m_LastType->isFloatTy() || m_LastType->isDoubleTy()) {
      value = llvm::ConstantFP::get(m_LastType, scalarAst.m_Value);
    } else {
      if (scalarAst.m_IsLiteral) {
        auto literalValue = DecodeCStringEscapes(scalarAst.m_Value);
        value = CreateGlobalStringPointer(llvm::StringRef(literalValue));
      } else if (scalarAst.m_IsBoolean) {
        value = llvm::ConstantInt::get(
            m_LastType, scalarAst.m_Value == "true" ? 1 : 0);
      } else {
        if (!m_LastSigned) {  // unsigned
          auto scalarVal = std::stoull(scalarAst.m_Value);

          llvm::Type *type = m_LastType;
          if (type->isPointerTy()) {
            type = GetPointeeType(value);
          }

          int maxVal = 0;
          if (type->isArrayTy()) {
            type = type->getArrayElementType();
            maxVal = 1 << (type->getIntegerBitWidth() - 1);
          } else {
            maxVal = 1 << (type->getIntegerBitWidth() - 1);
          }

          // -1 for 0
          maxVal = scalarVal > 0 ? maxVal - 1 : maxVal;

          // two's complement
          if (scalarVal > maxVal) {
            // - 1 for 0
            scalarVal = (0 - ~scalarVal) - ((scalarVal > 0) ? 1 : 0);
          }

          value = llvm::ConstantInt::get(type, scalarVal);
        } else {  // signed
          if (scalarAst.m_IsLetter) {
            char val = scalarAst.m_Value[0];
            value = llvm::ConstantInt::get(m_LastType, (int)val);
          } else {
            auto scalarVal = std::stoll(scalarAst.m_Value);

            value = llvm::ConstantInt::get(m_LastType, scalarVal);
          }
        }
      }
    }
  }

  return value;
}

ValuePtr Visitor::visit(VarAST &varAst) {
  llvm::Value *value;
  this->m_LastVarDecl = true;
  const auto definedTypeName = DefinedTypeNameFromVarAst(varAst);
  auto type = GetStorageType(varAst.m_TypeSpec, varAst.m_IndirectLevel,
                             varAst.m_IsUniquePtr, varAst.m_IsRef,
                             definedTypeName);
  this->m_LastType = type;
  this->m_LastDefinedTypeName = definedTypeName;
  this->m_LastGlobVar = !varAst.m_IsLocal;
  this->m_LastSigned =
      IsSigned(GetEffectiveStorageType(varAst.m_TypeSpec, definedTypeName));
  this->m_LastInitializerList = varAst.m_IsInitializerList;
  this->m_LastVarName = varAst.m_Name;
  Visitor::LastGlobVarRef = nullptr;
  llvm::ArrayType *arrayType = nullptr;

  if (varAst.m_IsInitializerList) {
    this->m_LastArrayDims.clear();

    for (auto &dim : varAst.m_ArrDim) {
      auto val = (ScalarOrLiteralAST *)dim.get();
      this->m_LastArrayDims.push_back(std::stoull(val->m_Value));
    }
    size_t length = 1;
    for (auto &dim : m_LastArrayDims) {
      length *= dim;
    }
    arrayType = llvm::ArrayType::get(type, length);
  }

  if (varAst.m_RHS != nullptr) {
    llvm::GlobalVariable *globVar = nullptr;
    std::string constructorName;
    if (constructorInitializer(varAst, constructorName)) {
      if (!varAst.m_IsLocal) {
        assert(false && "Constructor initializer requires local storage.");
      }

      value = llvm::ConstantAggregateZero::get(type);
      auto *storage = llvm::dyn_cast<llvm::AllocaInst>(
          CreateLocalVariable(varAst.m_Name, type, value));
      m_LocalVarsOnScope[varAst.m_Name] = storage;

      auto *call = static_cast<FuncCallAST *>(varAst.m_RHS.get());
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
      collectArgs(collectArgs, call->m_Args.get());

      auto *function = Visitor::Module->getFunction(constructorName);
      if (function == nullptr) {
        assert(false && "Constructor function must be declared before use.");
      }

      std::vector<llvm::Value *> args;
      args.push_back(storage);
      auto signatureIt = FunctionTable.find(constructorName);
      llvm::Type *previousType = m_LastType;
      bool previousSigned = m_LastSigned;

      for (size_t i = 0; i < argNodes.size(); ++i) {
        const size_t paramIndex = i + 1;
        auto *paramType = function->getFunctionType()->getParamType(paramIndex);
        const SymbolInfo *paramInfo =
            signatureIt != FunctionTable.end() &&
                    paramIndex < signatureIt->second.params.size()
                ? &signatureIt->second.params[paramIndex]
                : nullptr;

        m_LastType = paramType;
        m_LastSigned = true;

        if (paramInfo != nullptr && paramInfo->isRef &&
            argNodes[i]->m_ExprKind == ExprKind::SymbolExpr) {
          auto *argSymbol = static_cast<SymbolAST *>(argNodes[i]);
          auto *argStorage = FindStorage(m_LocalVarsOnScope, m_GlobalVars,
                                         argSymbol->m_SymbolName);
          if (argStorage == nullptr) {
            assert(false && "Reference argument storage was not found.");
          }

          auto referenceParam =
              m_ReferenceParamValueTypes.find(argSymbol->m_SymbolName);
          if (referenceParam != m_ReferenceParamValueTypes.end()) {
            auto *slotType = GetPointeeType(argStorage);
            argStorage = CreateLoad(argStorage, slotType,
                                    argSymbol->m_SymbolName + ".ref.arg");
          }
          args.push_back(argStorage);
          continue;
        }

        auto *argValue = argNodes[i]->accept(*this);
        argValue = CastValueToType(argValue, paramType, m_LastSigned);
        args.push_back(argValue);
      }

      m_LastType = previousType;
      m_LastSigned = previousSigned;
      Builder->CreateCall(function, args);
      registerScopeDestructor(getSymbolInfo(varAst.m_Name));
      this->m_LastVarDecl = false;
      this->m_LastType = nullptr;
      this->m_LastDefinedTypeName.clear();
      this->m_LastSigned = false;
      return storage;
    }

    if (varAst.m_IsInitializerList && varAst.m_RHS->m_ExprKind != BinOp) {
      std::vector<BinOpOrVal> elements;
      getElementsOfArray(*varAst.m_RHS, elements);
      auto emitInitializerElement = [&](const BinOpOrVal &el) -> llvm::Value * {
        if (el.isSymbol) {
          auto *symbolAst = reinterpret_cast<SymbolAST *>(el.address);
          return symbolAst->accept(*this);
        }
        if (el.isBinOp) {
          auto *binaryAst = reinterpret_cast<BinaryOpAST *>(el.address);
          return createBinaryOp(*binaryAst);
        }
        if (m_LastType->isDoubleTy() || m_LastType->isFloatTy()) {
          return llvm::ConstantFP::get(m_LastType, el.value);
        }
        uint64_t val = std::stoull(el.value);
        return llvm::ConstantInt::get(m_LastType, val);
      };

      if (varAst.m_IsLocal) {
        auto *storage = Builder->CreateAlloca(arrayType, nullptr,
                                              llvm::Twine(varAst.m_Name));
        auto *zero = llvm::ConstantAggregateZero::get(arrayType);
        Builder->CreateStore(zero, storage);

        for (size_t i = 0; i < elements.size(); ++i) {
          auto *elementValue = emitInitializerElement(elements[i]);
          elementValue = CastValueToType(elementValue, type, m_LastSigned);
          auto *zeroIndex = llvm::ConstantInt::get(Builder->getInt64Ty(), 0);
          auto *elementIndex =
              llvm::ConstantInt::get(Builder->getInt64Ty(), i);
          auto *slot = Builder->CreateInBoundsGEP(
              arrayType, storage, {zeroIndex, elementIndex});
          Builder->CreateStore(elementValue, slot);
        }

        m_LocalVarsOnScope[varAst.m_Name] = storage;
        this->m_LastVarDecl = false;
        this->m_LastType = nullptr;
        this->m_LastDefinedTypeName.clear();
        this->m_LastSigned = false;
        return storage;
      }

      std::vector<llvm::Constant *> values;
      bool constantInitializer = true;
      for (auto &el : elements) {
        if (el.isSymbol || el.isBinOp) {
          constantInitializer = false;
          break;
        }

        llvm::Constant *constant = nullptr;
        if (m_LastType->isDoubleTy() || m_LastType->isFloatTy()) {
          constant = llvm::ConstantFP::get(m_LastType, el.value);
        } else {
          uint64_t val = std::stoull(el.value);
          constant = llvm::ConstantInt::get(m_LastType, val);
        }
        values.push_back(constant);
      }

      if (constantInitializer) {
        auto *init = llvm::ConstantArray::get(arrayType, values);
        globVar = CreateInitConstantGlobalVar(
            varAst.m_Name, varAst.m_VisibilitySpec, arrayType, init);
      } else {
        auto *zero = llvm::ConstantAggregateZero::get(arrayType);
        globVar = CreateInitConstantGlobalVar(
            varAst.m_Name, varAst.m_VisibilitySpec, arrayType,
            llvm::dyn_cast<llvm::Constant>(zero));

        auto *lastInsertPoint = Builder->GetInsertBlock();
        auto *globInitFunc = CreateGlobalVarInitFunc();
        m_GlobaInitVarFunc.push_back(globInitFunc->getName());
        Builder->SetInsertPoint(&globInitFunc->getEntryBlock());
        for (size_t i = 0; i < elements.size(); ++i) {
          auto *elementValue = emitInitializerElement(elements[i]);
          elementValue = CastValueToType(elementValue, type, m_LastSigned);
          auto *zeroIndex = llvm::ConstantInt::get(Builder->getInt64Ty(), 0);
          auto *elementIndex =
              llvm::ConstantInt::get(Builder->getInt64Ty(), i);
          auto *slot = Builder->CreateInBoundsGEP(
              arrayType, globVar, {zeroIndex, elementIndex});
          Builder->CreateStore(elementValue, slot);
        }
        Builder->CreateRetVoid();
        if (lastInsertPoint != nullptr) {
          Builder->SetInsertPoint(lastInsertPoint);
        }
      }

      this->m_GlobalVars[varAst.m_Name] = globVar;
      this->m_LastVarDecl = false;
      this->m_LastType = nullptr;
      this->m_LastDefinedTypeName.clear();
      this->m_LastSigned = false;
      return globVar;
    }

    if (!varAst.m_IsLocal && varAst.m_RHS->m_ExprKind == BinOp &&
        !m_LastInitializerList) {
      llvm::Constant *constant = nullptr;
      if (type->isFloatTy() || type->isDoubleTy()) {
        constant = llvm::ConstantFP::get(type, 0.0f);
      } else {
        constant = llvm::ConstantInt::get(type, 0);
      }
      globVar = CreateConstantGlobalVar(varAst.m_Name, varAst.m_VisibilitySpec,
                                        type, constant);
      Visitor::LastGlobVarRef = globVar;
    } else {
      if (!varAst.m_IsLocal && m_LastInitializerList &&
          varAst.m_RHS->m_ExprKind == BinOp) {
        std::vector<BinOpOrVal> elements;
        getElementsOfArray(*varAst.m_RHS, elements);

        if (IsAnyBinOpOrSymbolInvolved(elements)) {
          value = llvm::ConstantAggregateZero::get(arrayType);
          globVar = CreateInitConstantGlobalVar(
              varAst.m_Name, varAst.m_VisibilitySpec, arrayType,
              llvm::dyn_cast<llvm::Constant>(value));
        }
        Visitor::LastGlobVarRef = globVar;
      }
    }

    const bool targetIsShared =
        varAst.m_IndirectLevel > 0 && !varAst.m_IsUniquePtr && !varAst.m_IsRef;
    auto *rhsSymbol = symbolFromMoveSource(varAst.m_RHS.get());
    auto *rhsUnary = dynamic_cast<UnaryOpAST *>(varAst.m_RHS.get());
    const bool rhsIsMove =
        rhsUnary != nullptr && rhsUnary->m_UnaryOpKind == U_MOVE;

    value = varAst.m_RHS->accept(*this);
    value = CastValueToType(value, type, this->m_LastSigned);
    if (targetIsShared && rhsSymbol != nullptr && !rhsIsMove) {
      RetainSharedPointer(value);
    }

    if (varAst.m_RHS->m_ExprKind != BinOp) {
      if (varAst.m_IsLocal) {
        if (varAst.m_IsInitializerList) {
          value->setName(varAst.m_Name);
        } else {
          m_LocalVarsOnScope[varAst.m_Name] = llvm::dyn_cast<llvm::AllocaInst>(
              CreateLocalVariable(varAst.m_Name, type, value));
          if (targetIsShared && rhsSymbol != nullptr && rhsIsMove) {
            auto *sourceStorage = FindStorage(m_LocalVarsOnScope, m_GlobalVars,
                                              rhsSymbol->m_SymbolName);
            Builder->CreateStore(CreateNullSharedPointerHandle(),
                                 sourceStorage);
          }
        }
      } else {
        globVar = CreateConstantGlobalVar(varAst.m_Name,
                                          varAst.m_VisibilitySpec, type, value);
        this->m_GlobalVars[varAst.m_Name] = globVar;
      }
    } else {
      if (varAst.m_IsLocal) {
        if (varAst.m_IsInitializerList) {
          value->setName(varAst.m_Name);
          this->m_LocalVarsOnScope[varAst.m_Name] =
              llvm::dyn_cast<llvm::AllocaInst>(value);
        } else {
          this->m_LocalVarsOnScope[varAst.m_Name] =
              llvm::dyn_cast<llvm::AllocaInst>(
                  CreateLocalVariable(varAst.m_Name, type, value));
        }
      } else {
        this->m_GlobalVars[varAst.m_Name] = globVar;
      }
    }
  } else {
    // if no data bound
    if (!varAst.m_IsLocal) {
      llvm::GlobalVariable *globVar = nullptr;
      if (varAst.m_IsInitializerList) {
        globVar = CreateZeroInitConstantGlobalVar(
            varAst.m_Name, varAst.m_VisibilitySpec, arrayType);
      } else {
        if (type->isStructTy()) {
          value = llvm::ConstantAggregateZero::get(type);
        } else if (IsSharedPointerTy(type)) {
          value = llvm::ConstantAggregateZero::get(type);
        } else if (type->isFloatTy() || type->isDoubleTy()) {
          value = llvm::ConstantFP::get(type, 0.0f);
        } else {
          value = llvm::ConstantInt::get(type, 0);
        }
        globVar = CreateConstantGlobalVar(varAst.m_Name,
                                          varAst.m_VisibilitySpec, type, value);
      }
      this->m_GlobalVars[varAst.m_Name] = globVar;
    } else {
      if (varAst.m_IsInitializerList) {
        value = llvm::ConstantAggregateZero::get(arrayType);
        m_LocalVarsOnScope[varAst.m_Name] = llvm::dyn_cast<llvm::AllocaInst>(
            CreateLocalVariable(varAst.m_Name, arrayType, value));
      } else {
        if (type->isStructTy()) {
          value = llvm::ConstantAggregateZero::get(type);
        } else if (IsSharedPointerTy(type)) {
          value = CreateNullSharedPointerHandle();
        } else if (type->isFloatTy() || type->isDoubleTy()) {
          value = llvm::ConstantFP::get(type, 0.0f);
        } else {
          value = llvm::ConstantInt::get(type, 0);
        }
        m_LocalVarsOnScope[varAst.m_Name] = llvm::dyn_cast<llvm::AllocaInst>(
            CreateLocalVariable(varAst.m_Name, type, value));
      }
    }
  }

  this->m_LastVarDecl = false;
  this->m_LastType = nullptr;
  this->m_LastDefinedTypeName.clear();
  this->m_LastSigned = false;

  if (varAst.m_IsLocal && varAst.m_RHS != nullptr &&
      varAst.m_RHS->m_ExprKind == ExprKind::NewExpr) {
    auto *newAst = static_cast<NewAST *>(varAst.m_RHS.get());
    HeapAllocationInfo allocationInfo;
    allocationInfo.typeName = newAst->m_TypeName;
    allocationInfo.isShared = newAst->m_IsShared;
    if (newAst->m_Allocator != nullptr &&
        newAst->m_Allocator->m_ExprKind == ExprKind::SymbolExpr) {
      auto *allocatorSymbol =
          static_cast<SymbolAST *>(newAst->m_Allocator.get());
      allocationInfo.allocatorSymbol = allocatorSymbol->m_SymbolName;
      auto allocatorInfo = getSymbolInfo(allocatorSymbol->m_SymbolName);
      allocationInfo.allocatorTypeName = allocatorInfo.definedTypeName;
    }
    m_HeapAllocations[varAst.m_Name] = std::move(allocationInfo);
  }

  if (varAst.m_IsLocal && varAst.m_TypeSpec == TypeSpecifier::SPEC_DEFINED) {
    registerScopeDestructor(getSymbolInfo(varAst.m_Name));
  }

  return value;
}

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
      fieldInfo.isSubscriptable = false;
      fieldInfo.arrayDimensions.clear();
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
    llvm::Type *arrayType = GetPointeeType(target.address);
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

ValuePtr Visitor::visit(BinaryOpAST &binaryOpAst) {
  llvm::Value *value = nullptr;

  if (m_LastVarDecl) {
    llvm::ArrayType *arrayType = nullptr;

    if (m_LastInitializerList) {
      size_t length = 1;
      for (auto &dim : m_LastArrayDims) {
        length *= dim;
      }
      arrayType = llvm::ArrayType::get(m_LastType, length);
    }

    if (m_LastGlobVar) {
      if (m_LastInitializerList) {
        std::vector<BinOpOrVal> elements;
        getElementsOfArray(binaryOpAst, elements);
        if (IsAnyBinOpOrSymbolInvolved(elements)) {
          // zeroinitializer will be set for the global variable.
          value = nullptr;

          // global variable which has binary operation
          auto lastInsertPoint = Builder->GetInsertBlock();
          auto globInitFunc = CreateGlobalVarInitFunc();
          m_GlobaInitVarFunc.push_back(globInitFunc->getName());
          Builder->SetInsertPoint(&globInitFunc->getEntryBlock());
          std::vector<llvm::Value *> values;
          auto symbol = LastGlobVarRef;

          int i = 0;
          for (auto &el : elements) {
            if (el.isSymbol) {
              auto symbolAst = reinterpret_cast<SymbolAST *>(el.address);
              value = symbolAst->accept(*this);
            } else if (el.isBinOp) {
              auto binaryAst = reinterpret_cast<BinaryOpAST *>(el.address);
              value = createBinaryOp(*binaryAst);
            } else {
              if (m_LastType->isDoubleTy() || m_LastType->isFloatTy()) {
                value = llvm::ConstantFP::get(m_LastType, el.value);
              } else {
                uint64_t val = std::stoull(el.value);
                value = llvm::ConstantInt::get(m_LastType, val);
              }
            }

            std::vector<llvm::Value *> idxList;
            auto index = llvm::ConstantInt::get(Builder->getInt64Ty(), 0);
            idxList.push_back(index);
            index = llvm::ConstantInt::get(Builder->getInt64Ty(), i);
            idxList.push_back(index);
            i++;

            auto gep =
                Builder->CreateInBoundsGEP(GetPointeeType(symbol), symbol,
                                           idxList);

            Visitor::Builder->CreateStore(value, gep);
          }

          // value = Visitor::Builder->CreateStore(value, LastGlobVarRef);
          Visitor::Builder->CreateRetVoid();
        } else {
          // there is no binary operation in the initializer list so just
          // initialize the array
          std::vector<llvm::Constant *> values;

          for (auto &el : elements) {
            if (m_LastType->isDoubleTy() || m_LastType->isFloatTy()) {
              values.push_back(llvm::ConstantFP::get(m_LastType, el.value));
            } else {
              uint64_t val = std::stoull(el.value);
              values.push_back(llvm::ConstantInt::get(m_LastType, val));
            }
          }

          value = llvm::ConstantArray::get(arrayType, values);
        }
      } else {
        // global variable which has binary operation
        auto lastInsertPoint = Builder->GetInsertBlock();
        auto globInitFunc = CreateGlobalVarInitFunc();
        m_GlobaInitVarFunc.push_back(globInitFunc->getName());
        Builder->SetInsertPoint(&globInitFunc->getEntryBlock());

        // Since if we let it to call itself again
        // it will cause to a problem.
        if (m_LastVarDecl) m_LastVarDecl = !m_LastVarDecl;
        value = createBinaryOp(binaryOpAst);
        // insert binary instruction to this global_Var_init func
        // Builder->Insert(value);
        value = Visitor::Builder->CreateStore(value, LastGlobVarRef);
        Visitor::Builder->CreateRetVoid();
      }
    } else {
      if (m_LastInitializerList) {
        auto ptr = Builder->CreateAlloca(arrayType, nullptr);
        std::vector<BinOpOrVal> elements;
        getElementsOfArray(binaryOpAst, elements);

        if (IsAnyBinOpOrSymbolInvolved(elements)) {
          size_t i = 0;
          for (auto &el : elements) {
            if (el.isSymbol) {
              auto symbolAst = reinterpret_cast<SymbolAST *>(el.address);
              value = symbolAst->accept(*this);
            } else if (el.isBinOp) {
              auto binaryAst = reinterpret_cast<BinaryOpAST *>(el.address);
              value = createBinaryOp(*binaryAst);
            } else {
              if (m_LastType->isDoubleTy() || m_LastType->isFloatTy()) {
                value = llvm::ConstantFP::get(m_LastType, el.value);
              } else {
                uint64_t val = std::stoull(el.value);
                value = llvm::ConstantInt::get(m_LastType, val);
              }
            }

            std::vector<llvm::Value *> idxList;
            auto index = llvm::ConstantInt::get(Builder->getInt64Ty(), 0);
            idxList.push_back(index);
            index = llvm::ConstantInt::get(Builder->getInt64Ty(), i);
            idxList.push_back(index);
            i++;

            auto gep = Builder->CreateInBoundsGEP(arrayType, ptr, idxList);

            Visitor::Builder->CreateStore(value, gep);
          }

          value = ptr;
        } else {
          std::vector<llvm::Constant *> values;
          for (auto &el : elements) {
            if (m_LastType->isDoubleTy() || m_LastType->isFloatTy()) {
              value = llvm::ConstantFP::get(m_LastType, el.value);
            } else {
              uint64_t val = std::stoull(el.value);
              value = llvm::ConstantInt::get(m_LastType, val);
            }
            values.push_back(llvm::dyn_cast<llvm::Constant>(value));
          }
          auto init = llvm::ConstantArray::get(arrayType, values);
          auto globVar = CreateInitConstantGlobalVar(
              "__const." + m_LastFuncName + "." + m_LastVarName,
              VisibilitySpecifier::VIS_NONE, arrayType, init);
          auto bitcast = Builder->CreateBitCast(ptr, GetI8PtrTy());
          auto globBitcast = Builder->CreateBitCast(globVar, GetI8PtrTy());
          auto copySize = Visitor::Module->getDataLayout().getTypeAllocSize(
              arrayType);
          Builder->CreateMemCpy(bitcast, ptr->getAlign(), globBitcast,
                                globVar->getAlign(), copySize);
          value = ptr;
        }
      } else {
        value = createBinaryOp(binaryOpAst);
      }
    }
  } else {
    // not a variable decl
    value = createBinaryOp(binaryOpAst);
  }

  return value;
}

ValuePtr Visitor::visit(CastOpAST &castOpAst) {
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

  if (previousType != nullptr) {
    value = CastValueToType(value, previousType, previousSigned);
    m_LastType = previousType;
    m_LastSigned = previousSigned;
  }

  return value;
}

llvm::Function *Visitor::declareFunction(FuncAST &funcAst) {
  if (auto *function = Visitor::Module->getFunction(funcAst.m_FuncName)) {
    return function;
  }

  auto *retType = static_cast<TypeAST *>(funcAst.m_RetType.get());
  auto paramLayout = buildFunctionParamLayout(funcAst);
  const auto retDefinedTypeName = DefinedTypeNameFromTypeAst(retType);

  auto *functionType = llvm::FunctionType::get(
      GetStorageType(retType->m_TypeSpec, retType->m_IndirectLevel,
                     retType->m_IsUniquePtr, retType->m_IsRef,
                     retDefinedTypeName),
      paramLayout.hasParams() ? paramLayout.irTypes : std::vector<llvm::Type *>(),
      funcAst.m_IsVariadic);

  auto linkage = funcAst.m_IsStatic ? llvm::Function::InternalLinkage
                                    : llvm::Function::ExternalLinkage;
  auto *function = llvm::Function::Create(functionType, linkage,
                                          funcAst.m_FuncName,
                                          *Visitor::Module);
  function->setCallingConv(llvm::CallingConv::C);
  return function;
}

ValuePtr Visitor::visit(FuncAST &funcAst) {
  auto *function = declareFunction(funcAst);

  this->m_LastFuncName = funcAst.m_FuncName;
  auto paramLayout = buildFunctionParamLayout(funcAst);

  if (!funcAst.m_IsForwardDecl) {
    if (!function->empty()) {
      return function;
    }

    m_LocalVarsOnScope.clear();
    m_ReferenceParamValueTypes.clear();
    m_ArrayParamValueTypes.clear();
    m_ScopeDestructors.clear();
    m_CodegenDroppedSymbols.clear();
    m_HeapAllocations.clear();

    llvm::BasicBlock *funcBlock = llvm::BasicBlock::Create(
        Visitor::Builder->getContext(), "entry", function);
    Visitor::Builder->SetInsertPoint(funcBlock);

    // Keep the main function body's address
    if (funcAst.m_FuncName == "main") {
      MainFuncBB = funcBlock;
    }

    auto datalayout = Visitor::Module->getDataLayout();
    if (paramLayout.hasParams()) {
      for (auto &param : function->args()) {
        size_t paramNo = param.getArgNo();
        const std::string &paramName = paramLayout.names[paramNo];
        llvm::Type *paramType = paramLayout.irTypes[paramNo];
        llvm::Type *valueType = paramLayout.valueTypes[paramNo];
        uint64_t typeByte = datalayout.getPrefTypeAlign(valueType).value();

        if (paramLayout.isReference[paramNo]) {
          function->addParamAttr(paramNo, llvm::Attribute::NonNull);
          function->addDereferenceableParamAttr(paramNo, typeByte);
        }

        if (paramType->isPointerTy()) {
          llvm::Attribute attribute = llvm::Attribute::getWithAlignment(
              Visitor::Module->getContext(), llvm::Align(typeByte));
          function->addParamAttr(paramNo, attribute);
        }

        if (paramLayout.isReference[paramNo]) {
          m_ReferenceParamValueTypes[paramName] = valueType;
        }
        if (paramLayout.isArray[paramNo]) {
          m_ArrayParamValueTypes[paramName] = valueType;
        }
        m_LocalVarsOnScope[paramName] = Visitor::Builder->CreateAlloca(
            paramType, nullptr, llvm::Twine(paramName));
        Visitor::Builder->CreateStore(&param, m_LocalVarsOnScope[paramName]);
      }
    }

    for (auto &localExpr : funcAst.m_Scope) {
      auto expr = localExpr->accept(*this);
    }

    auto insertBlock = Visitor::Builder->GetInsertBlock();
    if (insertBlock != nullptr && insertBlock->getTerminator() == nullptr) {
      emitScopeExitDestructors();
      if (function->getReturnType()->isVoidTy()) {
        Visitor::Builder->CreateRetVoid();
      } else {
        Visitor::Builder->CreateRet(
            llvm::Constant::getNullValue(function->getReturnType()));
      }
    }
  }

  return function;
}

ValuePtr Visitor::visit(FuncCallAST &funcCallAst) {
  SymbolInfo callSymbolInfo;
  auto funcName =
      resolveFunctionCallName(funcCallAst.m_FuncSymbol.get(), callSymbolInfo,
                              false);
  if (funcName.empty()) {
    assert(false && "Function call target must resolve to a symbol.");
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
  collectArgs(collectArgs, funcCallAst.m_Args.get());
  const bool hasImplicitMethodReceiver =
      methodCallReceiver(funcCallAst.m_FuncSymbol.get()) != nullptr;
  if (auto *receiver = methodCallReceiver(funcCallAst.m_FuncSymbol.get())) {
    argNodes.insert(argNodes.begin(), receiver);
  }
  cstar::codegen::NativeRuntime nativeRuntime(*Visitor::Module,
                                              *Visitor::Builder);

  if (funcName == "print") {
    llvm::Value *lastCall = nullptr;
    llvm::Type *previousType = m_LastType;
    bool previousSigned = m_LastSigned;

    for (auto *argNode : argNodes) {
      m_LastType = nullptr;
      m_LastSigned = true;
      llvm::Value *argValue = argNode->accept(*this);
      if (argValue == nullptr) {
        continue;
      }

      lastCall = nativeRuntime.emitPrintValue(argValue, m_LastSigned);
    }

    m_LastType = previousType;
    m_LastSigned = previousSigned;
    return lastCall;
  }

  if (funcName == "input_int") {
    if (!argNodes.empty()) {
      assert(false && "Builtin 'input_int' does not accept arguments.");
    }

    auto *value = nativeRuntime.emitInputInt();
    m_LastType = Builder->getInt64Ty();
    m_LastSigned = true;
    return value;
  }

  if (funcName == "input_string") {
    if (!argNodes.empty()) {
      assert(false && "Builtin 'input_string' does not accept arguments.");
    }

    auto *bufferPtr = nativeRuntime.emitInputString();
    m_LastType = GetI8PtrTy();
    m_LastSigned = false;
    return bufferPtr;
  }

  if (funcName == "clear_screen") {
    if (!argNodes.empty()) {
      assert(false && "Builtin 'clear_screen' does not accept arguments.");
    }
    return nativeRuntime.emitClearScreen();
  }

  if (funcName == "flush_output") {
    if (!argNodes.empty()) {
      assert(false && "Builtin 'flush_output' does not accept arguments.");
    }
    return nativeRuntime.emitFlushOutput();
  }

  if (funcName == "sleep_ms") {
    if (argNodes.size() != 1) {
      assert(false && "Builtin 'sleep_ms' expects one argument.");
    }

    llvm::Type *previousType = m_LastType;
    bool previousSigned = m_LastSigned;
    m_LastType = Builder->getInt32Ty();
    m_LastSigned = true;
    auto *milliseconds = argNodes[0]->accept(*this);
    milliseconds = CastValueToType(milliseconds, Builder->getInt32Ty(), true);
    auto *call = nativeRuntime.emitSleepMs(milliseconds, m_LastSigned);
    m_LastType = previousType;
    m_LastSigned = previousSigned;
    return call;
  }

  if (funcName == "enable_raw_input") {
    if (!argNodes.empty()) {
      assert(false && "Builtin 'enable_raw_input' does not accept arguments.");
    }
    return nativeRuntime.emitEnableRawInput();
  }

  if (funcName == "disable_raw_input") {
    if (!argNodes.empty()) {
      assert(false && "Builtin 'disable_raw_input' does not accept arguments.");
    }
    return nativeRuntime.emitDisableRawInput();
  }

  if (funcName == "read_key") {
    if (!argNodes.empty()) {
      assert(false && "Builtin 'read_key' does not accept arguments.");
    }

    auto *value = nativeRuntime.emitReadKey();
    m_LastType = Builder->getInt32Ty();
    m_LastSigned = true;
    return value;
  }

  if (funcName == "strong_count") {
    if (argNodes.size() != 1 ||
        argNodes[0]->m_ExprKind != ExprKind::SymbolExpr) {
      assert(false && "Builtin 'strong_count' expects one symbol argument.");
    }

    auto *symbol = static_cast<SymbolAST *>(argNodes[0]);
    auto *storage = FindStorage(m_LocalVarsOnScope, m_GlobalVars,
                                symbol->m_SymbolName);
    if (storage == nullptr) {
      assert(false && "strong_count target was not found.");
    }

    auto *handle = Builder->CreateLoad(GetSharedPointerTy(), storage,
                                       symbol->m_SymbolName + ".sp");
    auto *count = LoadSharedPointerStrongCount(handle);
    m_LastType = Builder->getInt64Ty();
    m_LastSigned = true;
    return count;
  }

  auto *function = Visitor::Module->getFunction(funcName);
  if (function == nullptr) {
    assert(false && "Function must be declared before it is called.");
  }

  const bool isVariadic = function->getFunctionType()->isVarArg();
  const size_t fixedArgCount = function->arg_size();
  if ((!isVariadic && fixedArgCount != argNodes.size()) ||
      (isVariadic && argNodes.size() < fixedArgCount)) {
    assert(false && "Function call argument count mismatch.");
  }

  std::vector<llvm::Value *> args;
  llvm::Type *previousType = m_LastType;
  bool previousSigned = m_LastSigned;
  auto signatureIt = FunctionTable.find(funcName);

  for (size_t i = 0; i < argNodes.size(); ++i) {
    const bool isVariadicArg = i >= fixedArgCount;
    llvm::Type *paramType =
        isVariadicArg ? nullptr : function->getFunctionType()->getParamType(i);
    const SymbolInfo *paramInfo =
        signatureIt != FunctionTable.end() && i < signatureIt->second.params.size()
            ? &signatureIt->second.params[i]
            : nullptr;

    const bool expectsArray =
        signatureIt != FunctionTable.end() &&
        i < signatureIt->second.params.size() &&
        signatureIt->second.params[i].isSubscriptable;
    if (expectsArray) {
      if (argNodes[i]->m_ExprKind != ExprKind::SymbolExpr) {
        assert(false && "Array parameter arguments must be symbols.");
      }

      auto *argSymbol = static_cast<SymbolAST *>(argNodes[i]);
      auto *storage = FindStorage(m_LocalVarsOnScope, m_GlobalVars,
                                  argSymbol->m_SymbolName);
      if (storage == nullptr) {
        assert(false && "Array argument storage was not found.");
      }

      if (m_ArrayParamValueTypes.count(argSymbol->m_SymbolName) > 0) {
        auto *slotType = GetPointeeType(storage);
        storage = CreateLoad(storage, slotType,
                             argSymbol->m_SymbolName + ".array.arg");
      }

      args.push_back(storage);
      continue;
    }

    m_LastType = paramType;
    m_LastSigned = true;

    auto *argSymbol = symbolFromMoveSource(argNodes[i]);
    auto *argUnary = dynamic_cast<UnaryOpAST *>(argNodes[i]);
    const bool argIsMove =
        argUnary != nullptr && argUnary->m_UnaryOpKind == U_MOVE;
    const bool paramIsShared =
        paramInfo != nullptr && IsSharedPointerSymbol(*paramInfo);

    if (!isVariadicArg && paramInfo != nullptr && paramInfo->isRef &&
        argNodes[i]->m_ExprKind == ExprKind::SymbolExpr) {
      auto *argSymbol = static_cast<SymbolAST *>(argNodes[i]);
      auto *storage = FindStorage(m_LocalVarsOnScope, m_GlobalVars,
                                  argSymbol->m_SymbolName);
      if (storage == nullptr) {
        assert(false && "Reference argument storage was not found.");
      }

      auto referenceParam =
          m_ReferenceParamValueTypes.find(argSymbol->m_SymbolName);
      if (referenceParam != m_ReferenceParamValueTypes.end()) {
        auto *slotType = GetPointeeType(storage);
        storage = CreateLoad(storage, slotType,
                             argSymbol->m_SymbolName + ".ref.arg");
      } else if (hasImplicitMethodReceiver && i == 0) {
        auto receiverInfo = getSymbolInfo(argSymbol->m_SymbolName);
        if (receiverInfo.type == TypeSpecifier::SPEC_DEFINED &&
            receiverInfo.indirectionLevel > 0) {
          if (IsSharedPointerSymbol(receiverInfo)) {
            auto *handle = Builder->CreateLoad(
                GetSharedPointerTy(), storage,
                argSymbol->m_SymbolName + ".receiver.sp");
            storage = ExtractSharedPointerData(handle);
          } else {
            auto *pointerType =
                GetType(receiverInfo.type, receiverInfo.indirectionLevel);
            storage = Builder->CreateLoad(
                pointerType, storage,
                argSymbol->m_SymbolName + ".receiver.ptr");
          }
        }
      }
      args.push_back(storage);
      continue;
    }

    llvm::Value *argValue = nullptr;
    if (isVariadicArg && argNodes[i]->m_ExprKind == ExprKind::SymbolExpr) {
      auto *varArgSymbol = static_cast<SymbolAST *>(argNodes[i]);
      auto varArgInfo = getSymbolInfo(varArgSymbol->m_SymbolName);
      if (varArgInfo.isSubscriptable) {
        argValue = FindStorage(m_LocalVarsOnScope, m_GlobalVars,
                               varArgSymbol->m_SymbolName);
        if (argValue == nullptr) {
          assert(false && "Variadic array argument storage was not found.");
        }
      }
    }
    if (argValue == nullptr) {
      argValue = argNodes[i]->accept(*this);
    }
    if (!isVariadicArg) {
      argValue = CastValueToType(argValue, paramType, m_LastSigned);
    }
    if (paramIsShared && argSymbol != nullptr && !argIsMove) {
      RetainSharedPointer(argValue);
    }
    args.push_back(argValue);

    if (argIsMove && argSymbol != nullptr && paramInfo != nullptr &&
        paramInfo->indirectionLevel > 0 && !paramInfo->isRef) {
      auto *sourceStorage = FindStorage(m_LocalVarsOnScope, m_GlobalVars,
                                        argSymbol->m_SymbolName);
      if (sourceStorage != nullptr) {
        auto *sourceType = GetPointeeType(sourceStorage);
        llvm::Value *nullValue = IsSharedPointerTy(sourceType)
                                     ? CreateNullSharedPointerHandle()
                                     : llvm::Constant::getNullValue(sourceType);
        Builder->CreateStore(nullValue, sourceStorage);
      }
    }
  }

  m_LastType = previousType;
  m_LastSigned = previousSigned;

  auto *call =
      Builder->CreateCall(function, args,
                          function->getReturnType()->isVoidTy() ? ""
                                                                 : "calltmp");
  if (!function->getReturnType()->isVoidTy()) {
    m_LastType = function->getReturnType();
  }

  return call;
}

ValuePtr Visitor::visit(NewAST &newAst) {
  auto *structType = GetDefinedStructTy(newAst.m_TypeName);
  if (structType == nullptr) {
    assert(false && "`new` target struct type must exist.");
  }

  llvm::Value *storage = nullptr;
  const auto allocationSize = Module->getDataLayout().getTypeAllocSize(
      structType);
  const auto allocationAlign = Module->getDataLayout().getPrefTypeAlign(
      structType).value();
  auto *sizeValue = llvm::ConstantInt::get(Builder->getInt64Ty(),
                                           allocationSize);
  auto *alignValue = llvm::ConstantInt::get(Builder->getInt64Ty(),
                                            allocationAlign);

  if (newAst.m_Allocator != nullptr &&
      newAst.m_Allocator->m_ExprKind == ExprKind::SymbolExpr) {
    auto *allocatorSymbol = static_cast<SymbolAST *>(newAst.m_Allocator.get());
    auto allocatorInfo = getSymbolInfo(allocatorSymbol->m_SymbolName);
    auto *allocatorStorage =
        FindStorage(m_LocalVarsOnScope, m_GlobalVars,
                    allocatorSymbol->m_SymbolName);
    auto *allocFunction = Module->getFunction(allocatorInfo.definedTypeName +
                                              ".alloc");
    if (allocatorStorage != nullptr && allocFunction != nullptr) {
      if (allocatorInfo.isRef &&
          m_ReferenceParamValueTypes.count(allocatorSymbol->m_SymbolName) > 0) {
        auto *slotType = GetPointeeType(allocatorStorage);
        allocatorStorage = CreateLoad(
            allocatorStorage, slotType,
            allocatorSymbol->m_SymbolName + ".allocator.ref");
      }
      storage = Builder->CreateCall(
          allocFunction, {allocatorStorage, sizeValue, alignValue},
          "new.alloc");
    }
  }

  if (storage == nullptr) {
    storage = CreateDefaultHeapAlloc(structType, "new.malloc");
  }

  storage = Builder->CreatePointerCast(storage, GetType(TypeSpecifier::SPEC_DEFINED, 1));

  auto *constructor = Module->getFunction(newAst.m_TypeName + ".constructor");
  if (constructor != nullptr) {
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

    std::vector<llvm::Value *> args;
    args.push_back(storage);
    auto signatureIt = FunctionTable.find(newAst.m_TypeName + ".constructor");
    llvm::Type *previousType = m_LastType;
    bool previousSigned = m_LastSigned;

    for (size_t i = 0; i < argNodes.size(); ++i) {
      const size_t paramIndex = i + 1;
      auto *paramType = constructor->getFunctionType()->getParamType(paramIndex);
      const SymbolInfo *paramInfo =
          signatureIt != FunctionTable.end() &&
                  paramIndex < signatureIt->second.params.size()
              ? &signatureIt->second.params[paramIndex]
              : nullptr;

      m_LastType = paramType;
      m_LastSigned = true;
      if (paramInfo != nullptr && paramInfo->isRef &&
          argNodes[i]->m_ExprKind == ExprKind::SymbolExpr) {
        auto *argSymbol = static_cast<SymbolAST *>(argNodes[i]);
        auto *argStorage =
            FindStorage(m_LocalVarsOnScope, m_GlobalVars,
                        argSymbol->m_SymbolName);
        if (argStorage == nullptr) {
          assert(false && "Reference argument storage was not found.");
        }
        auto referenceParam =
            m_ReferenceParamValueTypes.find(argSymbol->m_SymbolName);
        if (referenceParam != m_ReferenceParamValueTypes.end()) {
          auto *slotType = GetPointeeType(argStorage);
          argStorage = CreateLoad(argStorage, slotType,
                                  argSymbol->m_SymbolName + ".ref.arg");
        }
        args.push_back(argStorage);
        continue;
      }

      auto *argValue = argNodes[i]->accept(*this);
      argValue = CastValueToType(argValue, paramType, m_LastSigned);
      args.push_back(argValue);
    }

    m_LastType = previousType;
    m_LastSigned = previousSigned;
    Builder->CreateCall(constructor, args);
  }

  if (newAst.m_IsShared) {
    auto *counterRaw = CreateDefaultHeapAlloc(Builder->getInt64Ty(),
                                             "new.shared.count");
    auto *one = llvm::ConstantInt::get(Builder->getInt64Ty(), 1);
    Builder->CreateStore(one, counterRaw);
    auto *handle = CreateSharedPointerHandle(storage, counterRaw);
    m_LastType = GetSharedPointerTy();
    m_LastSigned = false;
    return handle;
  }

  m_LastType = GetType(TypeSpecifier::SPEC_DEFINED, 1);
  m_LastSigned = false;
  return storage;
}

llvm::BranchInst *Visitor::createBranch(IAST &ifCond, llvm::BasicBlock *thenBB,
                                        llvm::BasicBlock *elseBB,
                                        llvm::BasicBlock *mergeBB,
                                        bool elif = false) {
  auto cond = ifCond.accept(*this);

  cond = CastValueToBranchCondition(cond, m_LastSigned);

  llvm::Function *parentFunc = Visitor::Builder->GetInsertBlock()->getParent();

  thenBB = llvm::BasicBlock::Create(Builder->getContext(), "then", parentFunc);

  elseBB = llvm::BasicBlock::Create(Builder->getContext(), "else", parentFunc);

  if (!elif) {
    mergeBB =
        llvm::BasicBlock::Create(Builder->getContext(), "ifcont", parentFunc);
  }

  auto branchInst = Builder->CreateCondBr(cond, thenBB, elseBB);

  return branchInst;
}

ValuePtr Visitor::visit(IfStmtAST &ifStmtAst) {
  // if condition (binop)
  auto ifCond = ifStmtAst.m_Cond.begin()->second.first.get();

  auto cond = ifCond->accept(*this);

  cond = CastValueToBranchCondition(cond, m_LastSigned);

  llvm::Function *parentFunc = Visitor::Builder->GetInsertBlock()->getParent();

  llvm::BasicBlock *thenBB =
      llvm::BasicBlock::Create(Builder->getContext(), "then", parentFunc);

  llvm::BasicBlock *elseBB = nullptr;

  llvm::BasicBlock *mergeBB =
      llvm::BasicBlock::Create(Builder->getContext(), "endif", parentFunc);

  llvm::Value *thenVal = nullptr, *elseVal = nullptr;

  if (!ifStmtAst.m_HasElif) {
    if (ifStmtAst.m_HasElse) {
      elseBB =
          llvm::BasicBlock::Create(Builder->getContext(), "else", parentFunc);
      auto branchInst = Builder->CreateCondBr(cond, thenBB, elseBB);
    } else {
      auto branchInst = Builder->CreateCondBr(cond, thenBB, mergeBB);
    }

    Builder->SetInsertPoint(thenBB);
    // if block
    EmitScope(*this, ifStmtAst.m_Cond.begin()->second.second);

    CreateBranchIfNeeded(mergeBB);
    thenBB = Builder->GetInsertBlock();
  } else {
    // retrieving an else if block to be able to connect this with the other
    // branches properly
    llvm::BasicBlock *elifThenBB, *elifElseBB, *elifMergeBB;
    std::vector<std::pair<
        std::unique_ptr<IAST>,
        std::pair<std::vector<std::unique_ptr<IAST>>, llvm::BasicBlock *>>>
        elifBBs;
    auto it = ifStmtAst.m_ElseIfs.begin();
    const auto end = ifStmtAst.m_ElseIfs.end();
    while (it != end) {
      // WARNING: IAST MOVED
      elifThenBB =
          llvm::BasicBlock::Create(Builder->getContext(), "elif", parentFunc);
      std::vector<std::unique_ptr<IAST>> v;
      v.assign(std::make_move_iterator(it->second.second.begin()),
               std::make_move_iterator(it->second.second.end()));
      elifBBs.emplace_back(
          std::make_pair(std::move(it->second.first),
                         std::make_pair(std::move(v), elifThenBB)));
      // Builder->CreateCondBr(cond,elifThenBB,elifElseBB);
      it++;
    }

    auto branchInst =
        Builder->CreateCondBr(cond, thenBB, elifBBs[0].second.second);

    Builder->SetInsertPoint(thenBB);
    // if block
    EmitScope(*this, ifStmtAst.m_Cond.begin()->second.second);

    CreateBranchIfNeeded(mergeBB);
    thenBB = Builder->GetInsertBlock();
    // ---

    // elif
    for (int i = 0; i < elifBBs.size(); i++) {
      Builder->SetInsertPoint(elifBBs[i].second.second);

      auto &elifCond = elifBBs[i].first;
      cond = elifCond->accept(*this);

      cond = CastValueToBranchCondition(cond, m_LastSigned);

      elifThenBB = llvm::BasicBlock::Create(Builder->getContext(), "elifthen",
                                            parentFunc);

      bool terminate = false;
      // next elif
      if (elifBBs.size() - 1 == i) {
        if (ifStmtAst.m_HasElse) {
          elseBB = llvm::BasicBlock::Create(Builder->getContext(), "elifelse",
                                            parentFunc);
          branchInst = Builder->CreateCondBr(cond, elifThenBB, elseBB);
        } else {
          branchInst = Builder->CreateCondBr(cond, elifThenBB, mergeBB);
          terminate = true;
        }
      } else {
        branchInst = Builder->CreateCondBr(cond, elifThenBB,
                                           elifBBs[i + 1].second.second);
      }

      Builder->SetInsertPoint(elifThenBB);

      EmitScope(*this, elifBBs[i].second.first);

      CreateBranchIfNeeded(mergeBB);
      thenBB = Builder->GetInsertBlock();
    }

    // else
    if (ifStmtAst.m_HasElse) {
      Builder->SetInsertPoint(elseBB);

      EmitScope(*this, ifStmtAst.m_Else);

      CreateBranchIfNeeded(mergeBB);
      elseBB = Builder->GetInsertBlock();
    }

    goto skip_else;
  }

  if (ifStmtAst.m_HasElse) {
    Builder->SetInsertPoint(elseBB);

    EmitScope(*this, ifStmtAst.m_Else);
    CreateBranchIfNeeded(mergeBB);
    elseBB = Builder->GetInsertBlock();
    //   Builder->SetInsertPoint(elseBB);
  }
skip_else:

  Builder->SetInsertPoint(mergeBB);
  // FIXME: Will be added to PHI Node...
  /*if (thenVal != nullptr) {
    llvm::PHINode *pn = Builder->CreatePHI(thenVal->getType(), 2, "iftmp");
    pn->addIncoming(thenVal, thenBB);
    pn->addIncoming(elseVal, elseBB);
  }*/

  // There is no returning value here.
  return nullptr;
}

ValuePtr Visitor::visit(OptionStmtAST &optionStmtAst) {
  llvm::Function *parentFunc = Visitor::Builder->GetInsertBlock()->getParent();

  llvm::Value *matchedValue = optionStmtAst.m_Value->accept(*this);
  if (matchedValue == nullptr) {
    return nullptr;
  }

  std::vector<size_t> explicitCases;
  int defaultCase = -1;
  for (size_t i = 0; i < optionStmtAst.m_Cases.size(); ++i) {
    if (optionStmtAst.m_Cases[i].isDefault) {
      defaultCase = static_cast<int>(i);
    } else {
      explicitCases.push_back(i);
    }
  }

  auto *afterBB =
      llvm::BasicBlock::Create(Builder->getContext(), "option.end", parentFunc);
  auto *defaultBB =
      defaultCase >= 0
          ? llvm::BasicBlock::Create(Builder->getContext(), "option.default",
                                     parentFunc)
          : nullptr;

  auto enumPattern = [&](IAST *node, std::string &enumName,
                         std::string &memberName) {
    if (node == nullptr || node->m_ExprKind != ExprKind::BinOp) {
      return false;
    }
    auto *binary = static_cast<BinaryOpAST *>(node);
    if (binary->m_BinOpKind != B_DOT || binary->m_LHS == nullptr ||
        binary->m_RHS == nullptr ||
        binary->m_LHS->m_ExprKind != ExprKind::SymbolExpr ||
        binary->m_RHS->m_ExprKind != ExprKind::SymbolExpr) {
      return false;
    }

    auto *enumSymbol = static_cast<SymbolAST *>(binary->m_LHS.get());
    auto *memberSymbol = static_cast<SymbolAST *>(binary->m_RHS.get());
    enumName = enumSymbol->m_SymbolName;
    memberName = memberSymbol->m_SymbolName;
    return true;
  };

  if (explicitCases.empty()) {
    if (defaultBB != nullptr) {
      CreateBranchIfNeeded(defaultBB);
      Builder->SetInsertPoint(defaultBB);
      EmitScope(*this, optionStmtAst.m_Cases[defaultCase].scope);
      CreateBranchIfNeeded(afterBB);
    } else {
      CreateBranchIfNeeded(afterBB);
    }
    Builder->SetInsertPoint(afterBB);
    return nullptr;
  }

  for (size_t casePosition = 0; casePosition < explicitCases.size();
       ++casePosition) {
    const size_t caseIndex = explicitCases[casePosition];
    std::string enumName;
    std::string memberName;
    if (!enumPattern(optionStmtAst.m_Cases[caseIndex].pattern.get(), enumName,
                     memberName)) {
      assert(false && "option enum pattern should have been checked in pass1.");
    }

    const auto enumIt = EnumTable.find(enumName);
    if (enumIt == EnumTable.end()) {
      assert(false && "option enum should have been registered in pass1.");
    }
    const auto *member = FindEnumMember(enumName, memberName);
    if (member == nullptr) {
      assert(false && "option enum member should have been checked in pass1.");
    }

    auto *caseBB =
        llvm::BasicBlock::Create(Builder->getContext(), "option.case",
                                 parentFunc);
    llvm::BasicBlock *nextBB = nullptr;
    const bool hasNextExplicit = casePosition + 1 < explicitCases.size();
    if (hasNextExplicit) {
      nextBB =
          llvm::BasicBlock::Create(Builder->getContext(), "option.next",
                                   parentFunc);
    } else {
      nextBB = defaultBB != nullptr ? defaultBB : afterBB;
    }

    auto *memberValue = llvm::ConstantInt::get(
        matchedValue->getType(), member->value,
        IsSigned(enumIt->second.underlyingType));
    auto *condition = Builder->CreateICmpEQ(matchedValue, memberValue,
                                            "option.match");
    Builder->CreateCondBr(condition, caseBB, nextBB);

    Builder->SetInsertPoint(caseBB);
    EmitScope(*this, optionStmtAst.m_Cases[caseIndex].scope);
    CreateBranchIfNeeded(afterBB);

    if (hasNextExplicit) {
      Builder->SetInsertPoint(nextBB);
    }
  }

  if (defaultBB != nullptr) {
    Builder->SetInsertPoint(defaultBB);
    EmitScope(*this, optionStmtAst.m_Cases[defaultCase].scope);
    CreateBranchIfNeeded(afterBB);
  }

  Builder->SetInsertPoint(afterBB);
  return nullptr;
}

ValuePtr Visitor::visit(LoopStmtAST &loopStmtAst) {
  llvm::Function *parentFunc = Visitor::Builder->GetInsertBlock()->getParent();

  if (!loopStmtAst.m_RangeLoop) {
    llvm::BasicBlock *conditionBB =
        llvm::BasicBlock::Create(Builder->getContext(), "loop.cond", parentFunc);
    llvm::BasicBlock *bodyBB =
        llvm::BasicBlock::Create(Builder->getContext(), "loop.body", parentFunc);
    llvm::BasicBlock *afterBB =
        llvm::BasicBlock::Create(Builder->getContext(), "loop.end", parentFunc);

    CreateBranchIfNeeded(conditionBB);

    Builder->SetInsertPoint(conditionBB);
    llvm::Value *condition = loopStmtAst.m_Cond->accept(*this);
    condition = CastValueToBranchCondition(condition, m_LastSigned);
    Builder->CreateCondBr(condition, bodyBB, afterBB);

    Builder->SetInsertPoint(bodyBB);
    m_LoopBreakTargets.push_back(afterBB);
    m_LoopContinueTargets.push_back(conditionBB);
    EmitScope(*this, loopStmtAst.m_Scope);
    m_LoopContinueTargets.pop_back();
    m_LoopBreakTargets.pop_back();
    CreateBranchIfNeeded(conditionBB);

    Builder->SetInsertPoint(afterBB);
    return nullptr;
  }

  llvm::BasicBlock *conditionBB =
      llvm::BasicBlock::Create(Builder->getContext(), "loop.iter.cond",
                               parentFunc);
  llvm::BasicBlock *bodyBB =
      llvm::BasicBlock::Create(Builder->getContext(), "loop.iter.body",
                               parentFunc);
  llvm::BasicBlock *latchBB =
      llvm::BasicBlock::Create(Builder->getContext(), "loop.iter.latch",
                               parentFunc);
  llvm::BasicBlock *afterBB =
      llvm::BasicBlock::Create(Builder->getContext(), "loop.iter.end",
                               parentFunc);

  llvm::Type *indexType = Builder->getInt64Ty();
  auto *indexSlot = Builder->CreateAlloca(indexType, nullptr, "loop.index");
  llvm::Value *startValue = llvm::ConstantInt::get(indexType, 0);
  llvm::Value *endValue = nullptr;
  llvm::Value *iterStorage = nullptr;
  llvm::Type *iterArrayType = nullptr;
  llvm::Type *elementType = nullptr;

  if (loopStmtAst.m_HasNumericRange) {
    llvm::Type *previousType = m_LastType;
    bool previousSigned = m_LastSigned;
    m_LastType = indexType;
    m_LastSigned = true;
    startValue = loopStmtAst.m_Min->accept(*this);
    startValue = CastValueToType(startValue, indexType, true);
    endValue = loopStmtAst.m_Max->accept(*this);
    endValue = CastValueToType(endValue, indexType, true);
    m_LastType = previousType;
    m_LastSigned = previousSigned;
  } else {
    auto *iterSymbol = static_cast<SymbolAST *>(loopStmtAst.m_IterSymbol.get());
    iterStorage = FindStorage(m_LocalVarsOnScope, m_GlobalVars,
                              iterSymbol->m_SymbolName);
    if (iterStorage == nullptr) {
      assert(false && "Iterable loop target was not found.");
    }

    auto arrayParamType = m_ArrayParamValueTypes.find(iterSymbol->m_SymbolName);
    if (arrayParamType != m_ArrayParamValueTypes.end()) {
      auto *slotType = GetPointeeType(iterStorage);
      iterStorage =
          CreateLoad(iterStorage, slotType, iterSymbol->m_SymbolName + ".array");
      iterArrayType = arrayParamType->second;
    } else {
      iterArrayType = GetPointeeType(iterStorage);
    }

    if (!iterArrayType->isArrayTy()) {
      assert(false && "Iterable loop target must be an array.");
    }

    endValue = llvm::ConstantInt::get(
        indexType, iterArrayType->getArrayNumElements());
    elementType = iterArrayType->getArrayElementType();
  }

  Builder->CreateStore(startValue, indexSlot);

  std::map<std::string, llvm::AllocaInst *> shadowedLocals;
  std::set<std::string> insertedLocals;
  auto bindLoopLocal = [&](const std::string &name, llvm::Type *type,
                           llvm::Value *initialValue) -> llvm::AllocaInst * {
    auto existing = m_LocalVarsOnScope.find(name);
    if (existing != m_LocalVarsOnScope.end()) {
      shadowedLocals[name] = existing->second;
    } else {
      insertedLocals.insert(name);
    }

    auto *slot = Builder->CreateAlloca(type, nullptr, name);
    Builder->CreateStore(initialValue, slot);
    m_LocalVarsOnScope[name] = slot;
    return slot;
  };

  auto *dataSymbol = static_cast<SymbolAST *>(loopStmtAst.m_DataSymbol.get());
  llvm::AllocaInst *dataSlot = nullptr;
  if (loopStmtAst.m_HasNumericRange) {
    dataSlot = bindLoopLocal(dataSymbol->m_SymbolName, indexType, startValue);
  } else {
    dataSlot = bindLoopLocal(
        dataSymbol->m_SymbolName, elementType,
        llvm::Constant::getNullValue(elementType));
  }

  llvm::AllocaInst *visibleIndexSlot = nullptr;
  if (loopStmtAst.m_Indexable) {
    auto *indexSymbol =
        static_cast<SymbolAST *>(loopStmtAst.m_IndexSymbol.get());
    visibleIndexSlot = bindLoopLocal(indexSymbol->m_SymbolName, indexType,
                                     startValue);
  }

  CreateBranchIfNeeded(conditionBB);
  Builder->SetInsertPoint(conditionBB);
  auto *currentIndex = Builder->CreateLoad(indexType, indexSlot, "loop.i");
  auto *condition = Builder->CreateICmpSLT(currentIndex, endValue, "loop.cond");
  Builder->CreateCondBr(condition, bodyBB, afterBB);

  Builder->SetInsertPoint(bodyBB);
  currentIndex = Builder->CreateLoad(indexType, indexSlot, "loop.i");
  if (loopStmtAst.m_HasNumericRange) {
    Builder->CreateStore(currentIndex, dataSlot);
  } else {
    std::vector<llvm::Value *> idxList = {
        llvm::ConstantInt::get(indexType, 0), currentIndex};
    auto *elementAddress = Builder->CreateInBoundsGEP(
        iterArrayType, iterStorage, idxList, dataSymbol->m_SymbolName + ".addr");
    auto *elementValue = Builder->CreateLoad(
        elementType, elementAddress, dataSymbol->m_SymbolName);
    Builder->CreateStore(elementValue, dataSlot);
  }
  if (visibleIndexSlot != nullptr) {
    Builder->CreateStore(currentIndex, visibleIndexSlot);
  }

  m_LoopBreakTargets.push_back(afterBB);
  m_LoopContinueTargets.push_back(latchBB);
  EmitScope(*this, loopStmtAst.m_Scope);
  m_LoopContinueTargets.pop_back();
  m_LoopBreakTargets.pop_back();
  CreateBranchIfNeeded(latchBB);

  Builder->SetInsertPoint(latchBB);
  currentIndex = Builder->CreateLoad(indexType, indexSlot, "loop.i");
  auto *nextIndex = Builder->CreateAdd(
      currentIndex, llvm::ConstantInt::get(indexType, 1), "loop.next");
  Builder->CreateStore(nextIndex, indexSlot);
  Builder->CreateBr(conditionBB);

  Builder->SetInsertPoint(afterBB);
  for (const auto &entry : shadowedLocals) {
    m_LocalVarsOnScope[entry.first] = entry.second;
  }
  for (const auto &name : insertedLocals) {
    m_LocalVarsOnScope.erase(name);
  }
  return nullptr;
}

ValuePtr Visitor::visit(BreakStmtAST &breakStmtAst) {
  if (m_LoopBreakTargets.empty()) {
    return nullptr;
  }

  return Builder->CreateBr(m_LoopBreakTargets.back());
}

ValuePtr Visitor::visit(ContinueStmtAST &continueStmtAst) {
  if (m_LoopContinueTargets.empty()) {
    return nullptr;
  }

  return Builder->CreateBr(m_LoopContinueTargets.back());
}

ValuePtr Visitor::visit(DropStmtAST &dropStmtAst) {
  return emitDropForSymbol(dropStmtAst.m_SymbolName, true);
}

ValuePtr Visitor::visit(ParamAST &paramAst) { return nullptr; }

ValuePtr Visitor::visit(RetAST &retAst) {
  llvm::Function *function = Visitor::Builder->GetInsertBlock()->getParent();
  llvm::Type *returnType = function->getReturnType();

  if (retAst.m_NoReturn) {
    emitScopeExitDestructors();
    if (returnType->isVoidTy()) {
      return Visitor::Builder->CreateRetVoid();
    }

    return Visitor::Builder->CreateRet(llvm::Constant::getNullValue(returnType));
  }

  llvm::Type *previousType = m_LastType;
  bool previousSigned = m_LastSigned;
  m_LastType = returnType;
  m_LastSigned = true;

  auto *retSymbol = symbolFromMoveSource(retAst.m_RetExpr.get());
  auto *retUnary = dynamic_cast<UnaryOpAST *>(retAst.m_RetExpr.get());
  const bool retIsMove =
      retUnary != nullptr && retUnary->m_UnaryOpKind == U_MOVE;
  llvm::Value *value = retAst.m_RetExpr->accept(*this);
  m_LastType = previousType;
  m_LastSigned = previousSigned;

  if (value == nullptr) {
    return nullptr;
  }

  if (returnType->isVoidTy()) {
    emitScopeExitDestructors();
    Visitor::Builder->CreateRetVoid();
    return value;
  }

  if (value->getType() != returnType) {
    if (value->getType()->isIntegerTy() && returnType->isIntegerTy()) {
      value = Visitor::Builder->CreateIntCast(value, returnType, m_LastSigned);
    } else if (value->getType()->isFloatingPointTy() &&
               returnType->isFloatingPointTy()) {
      value = Visitor::Builder->CreateFPCast(value, returnType);
    }
  }

  if (IsSharedPointerTy(returnType) && retSymbol != nullptr && !retIsMove) {
    RetainSharedPointer(value);
  }

  if (retIsMove && retSymbol != nullptr) {
    auto *sourceStorage = FindStorage(m_LocalVarsOnScope, m_GlobalVars,
                                      retSymbol->m_SymbolName);
    if (sourceStorage != nullptr) {
      auto *sourceType = GetPointeeType(sourceStorage);
      llvm::Value *nullValue = IsSharedPointerTy(sourceType)
                                   ? CreateNullSharedPointerHandle()
                                   : llvm::Constant::getNullValue(sourceType);
      Builder->CreateStore(nullValue, sourceStorage);
    }
  }

  emitScopeExitDestructors();
  return Visitor::Builder->CreateRet(value);
}

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
