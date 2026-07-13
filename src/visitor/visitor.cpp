#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>

#include <ast/assignment_ast.hpp>
#include <ast/ast.hpp>
#include <ast/binary_op_ast.hpp>
#include <ast/cast_op_ast.hpp>
#include <ast/func_ast.hpp>
#include <ast/func_call_ast.hpp>
#include <ast/if_stmt.hpp>
#include <ast/loop_stmt.hpp>
#include <ast/param_ast.hpp>
#include <ast/ret_ast.hpp>
#include <ast/scalar_ast.hpp>
#include <ast/symbol_ast.hpp>
#include <ast/type_ast.hpp>
#include <ast/unary_op_ast.hpp>
#include <ast/var_ast.hpp>
#include <visitor/visitor.hpp>

llvm::BasicBlock *Visitor::MainFuncBB = nullptr;
llvm::GlobalVariable *Visitor::LastGlobVarRef = nullptr;

static llvm::PointerType *GetI8PtrTy() {
  return llvm::PointerType::get(Visitor::Builder->getContext(), 0);
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
                           bool isRef = false) {
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

ValuePtr Visitor::createBinaryOp(BinaryOpAST &binaryOpAst) {
  llvm::Value *value = nullptr, *rhs, *lhs;

  if (binaryOpAst.m_BinOpKind == B_ARRS) m_LastArrayIndex = true;
  lhs = binaryOpAst.m_LHS->accept(*this);

  rhs = binaryOpAst.m_RHS->accept(*this);
  if (binaryOpAst.m_BinOpKind == B_ARRS) m_LastArrayIndex = false;

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
      auto extra = binaryOpAst.m_Extra->accept(*this);
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
      std::vector<llvm::Value *> idxList;
      idxList.push_back(llvm::ConstantInt::get(Builder->getInt64Ty(), 0));
      if (rhs != nullptr) {
        idxList.push_back(rhs);
      } else {
        // flattening the array here.
        //      auto arrayType =
        //      llvm::dyn_cast<llvm::ArrayType>(lhs->getType()->getPointerElementType());
        //        auto arrSize = arrayType->getNumElements();

        auto symbolName = ((SymbolAST *)binaryOpAst.m_LHS.get())->m_SymbolName;
        auto symbolInfo = getSymbolInfo(symbolName);

        size_t el = 0;
        bool lastDim = false;
        // FIXME: It's buggy.
        for (int i = 0; i < m_Indices.size(); i++) {
          if (i == m_Indices.size() - 1) {
            lastDim = true;
          }

          if (m_IndicesAsStr[i].second) {  // it's float
            long long val = 0;
            if (m_IndicesAsStr[i].first[0] == '0') {
              val = 0;
            } else {
              val = std::stoll(m_IndicesAsStr[i].first);
            }

            if (lastDim) {
              el += val;
            } else {
              if (val < 0 || val >= symbolInfo.arrayDimensions[i]) {
                val %= (long long)symbolInfo.arrayDimensions[i];
              }

              if (val != 0) {
                el += val * symbolInfo.arrayDimensions[i];
              }
            }
          } else {
            auto val = std::stoll(m_IndicesAsStr[i].first);
            if (lastDim) {
              el += val;
            } else {
              if (val < 0 || val >= symbolInfo.arrayDimensions[i]) {
                val %= (long long)symbolInfo.arrayDimensions[i];
              }

              if (val != 0) {
                el += val * symbolInfo.arrayDimensions[i];
              }
            }
          }
        }

        auto index = llvm::ConstantInt::get(Builder->getInt64Ty(), el);
        idxList.push_back(index);

        // reset globals/flags
        m_Indices.clear();
        m_IndicesAsStr.clear();
      }

      auto symbolType = GetPointeeType(symbol);
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
      this->m_LastArrayIndex = true;
      if (lhs != nullptr) {
        m_Indices.push_back(lhs);
      }

      if (rhs != nullptr) {
        m_Indices.push_back(rhs);
      }
      this->m_LastArrayIndex = false;
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

  if (this->m_LastVarDecl && !this->m_LastArrayIndex) {
    valueType = GetPointeeType(value);

    isSigned = IsSigned(getSymbolInfo(symbolAst.m_SymbolName).type);
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
        value = Visitor::Builder->CreateGlobalStringPtr(
            llvm::StringRef(literalValue));
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
  auto type = GetType(varAst.m_TypeSpec, varAst.m_IndirectLevel);
  this->m_LastType = type;
  this->m_LastGlobVar = !varAst.m_IsLocal;
  this->m_LastSigned = IsSigned(varAst.m_TypeSpec);
  this->m_LastInitializerList = varAst.m_IsInitializerList;
  this->m_LastVarName = varAst.m_Name;
  Visitor::LastGlobVarRef = nullptr;
  llvm::ArrayType *arrayType = nullptr;

  if (varAst.m_IsInitializerList) {
    this->m_LastArrayDims.clear();

    for (auto &dim : varAst.m_ArrDim) {
      auto val = (ScalarOrLiteralAST *)dim.get();
      this->m_LastArrayDims.push_back(std::stoull(val->m_Value));

      // for the right order
      std::reverse(this->m_LastArrayDims.begin(), this->m_LastArrayDims.end());
    }
    size_t length = 1;
    for (auto &dim : m_LastArrayDims) {
      length *= dim;
    }
    arrayType = llvm::ArrayType::get(type, length);
  }

  if (varAst.m_RHS != nullptr) {
    llvm::GlobalVariable *globVar = nullptr;
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

    value = varAst.m_RHS->accept(*this);
    value = CastValueToType(value, type, this->m_LastSigned);

    if (varAst.m_RHS->m_ExprKind != BinOp) {
      if (varAst.m_IsLocal) {
        if (varAst.m_IsInitializerList) {
          value->setName(varAst.m_Name);
        } else {
          m_LocalVarsOnScope[varAst.m_Name] = llvm::dyn_cast<llvm::AllocaInst>(
              CreateLocalVariable(varAst.m_Name, type, value));
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
        if (type->isFloatTy() || type->isDoubleTy()) {
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
        if (type->isFloatTy() || type->isDoubleTy()) {
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
  this->m_LastSigned = false;

  return value;
}

ValuePtr Visitor::visit(AssignmentAST &assignmentAst) {
  auto *lhs = dynamic_cast<SymbolAST *>(assignmentAst.m_LHS.get());
  if (lhs == nullptr) {
    assert(false && "Only symbol assignment is supported currently.");
  }

  llvm::Value *storage = nullptr;
  if (m_LocalVarsOnScope.count(lhs->m_SymbolName) > 0) {
    storage = m_LocalVarsOnScope[lhs->m_SymbolName];
  } else if (m_GlobalVars.count(lhs->m_SymbolName) > 0) {
    storage = m_GlobalVars[lhs->m_SymbolName];
  } else {
    assert(false && "Assignment target was not found.");
  }

  auto symbolInfo = getSymbolInfo(lhs->m_SymbolName);
  auto target = ResolveAssignmentTarget(
      storage, symbolInfo,
      assignmentAst.m_IsDereferenced ? assignmentAst.m_DerefLevel : 0);

  llvm::Type *previousType = m_LastType;
  bool previousSigned = m_LastSigned;
  bool previousVarDecl = m_LastVarDecl;

  if (assignmentAst.m_Subscriptable) {
    if (assignmentAst.m_SubscriptIndexes.size() != 1) {
      assert(false && "Only one-dimensional array assignment is supported.");
    }

    llvm::Type *arrayType = GetPointeeType(target.address);
    if (!arrayType->isArrayTy()) {
      assert(false && "Subscript assignment target must be an array.");
    }

    m_LastType = Builder->getInt64Ty();
    m_LastSigned = true;
    m_LastVarDecl = false;

    llvm::Value *index = assignmentAst.m_SubscriptIndexes[0]->accept(*this);
    index = CastValueToType(index, Builder->getInt64Ty(), true);

    std::vector<llvm::Value *> idxList = {
        llvm::ConstantInt::get(Builder->getInt64Ty(), 0), index};
    target.address =
        Builder->CreateInBoundsGEP(arrayType, target.address, idxList,
                                   lhs->m_SymbolName + ".element");
    target.valueType = arrayType->getArrayElementType();
  }

  m_LastType = target.valueType;
  m_LastSigned = IsSigned(symbolInfo.type);
  m_LastVarDecl = false;

  llvm::Value *rhs = assignmentAst.m_RHS->accept(*this);
  rhs = CastValueToType(rhs, target.valueType, m_LastSigned);

  llvm::Value *result = rhs;
  if (assignmentAst.m_ShortcutOp != ShortcutOp::S_NONE &&
      assignmentAst.m_ShortcutOp != ShortcutOp::S_MOV) {
    llvm::Value *currentValue = Visitor::Builder->CreateLoad(
        target.valueType, target.address, lhs->m_SymbolName);

    result = CreateShortcutAssignmentValue(currentValue, rhs, target.valueType,
                                           assignmentAst.m_ShortcutOp,
                                           m_LastSigned);
    result = CastValueToType(result, target.valueType, m_LastSigned);
  }

  Visitor::Builder->CreateStore(result, target.address);

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

  auto *targetType =
      GetType(targetTypeAst->m_TypeSpec, targetTypeAst->m_IndirectLevel);
  const bool targetSigned = IsSigned(targetTypeAst->m_TypeSpec);

  llvm::Type *previousType = m_LastType;
  bool previousSigned = m_LastSigned;

  m_LastType = nullptr;
  m_LastSigned = targetSigned;
  llvm::Value *value = castOpAst.m_Node->accept(*this);

  if (castOpAst.m_CastOpKind == CastOpKind::C_UNSAFE_CAST) {
    value = UnsafeCastValueToType(value, targetType, targetSigned);
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

  auto retType = (TypeAST *)funcAst.m_RetType.get();

  std::vector<llvm::Type *> paramTypes;
  bool hasParam = false;
  for (auto &param : funcAst.m_Params) {
    auto paramAst = (ParamAST *)param.get();

    if (!paramAst->m_TypeAmbiguous) {
      auto typeAst = (TypeAST *)paramAst->m_TypeNode.get();
      auto paramType =
          GetType(typeAst->m_TypeSpec,
                  typeAst->m_IndirectLevel + (typeAst->m_IsRef ? 1 : 0),
                  typeAst->m_IsRef);
      paramTypes.push_back(paramType);
    } else {
      bool isLeftOne = false, isRightOne = false;
      for (auto &type : this->m_TypeTable) {
        auto symbol0 = (SymbolAST *)paramAst->m_Symbol0.get();
        auto symbol1 = (SymbolAST *)paramAst->m_Symbol1.get();

        if (type.first == symbol0->m_SymbolName) {
          isLeftOne = true;
        }

        if (type.first == symbol1->m_SymbolName) {
          isRightOne = true;
        }
        // For user-defined types...
      }
    }

    hasParam = true;
  }

  auto *functionType = llvm::FunctionType::get(
      GetType(retType->m_TypeSpec, retType->m_IndirectLevel),
      hasParam ? paramTypes : std::vector<llvm::Type *>(), false);

  auto *function =
      llvm::Function::Create(functionType, llvm::Function::ExternalLinkage,
                             funcAst.m_FuncName, *Visitor::Module);
  function->setCallingConv(llvm::CallingConv::C);
  return function;
}

ValuePtr Visitor::visit(FuncAST &funcAst) {
  auto *function = declareFunction(funcAst);
  auto retType = (TypeAST *)funcAst.m_RetType.get();

  this->m_LastFuncName = funcAst.m_FuncName;
  std::vector<llvm::Type *> paramTypes;
  std::vector<std::string> paramNames;
  bool hasParam = false;
  for (auto &param : funcAst.m_Params) {
    auto paramAst = (ParamAST *)param.get();

    if (!paramAst->m_TypeAmbiguous) {
      auto typeAst = (TypeAST *)paramAst->m_TypeNode.get();
      auto paramType =
          GetType(typeAst->m_TypeSpec,
                  typeAst->m_IndirectLevel + (typeAst->m_IsRef ? 1 : 0),
                  typeAst->m_IsRef);
      paramTypes.push_back(paramType);

      auto symbolAst = (SymbolAST *)paramAst->m_Symbol0.get();
      paramNames.push_back(symbolAst->m_SymbolName);
    }

    hasParam = true;
  }

  if (!funcAst.m_IsForwardDecl) {
    if (!function->empty()) {
      return function;
    }

    llvm::BasicBlock *funcBlock = llvm::BasicBlock::Create(
        Visitor::Builder->getContext(), "entry", function);
    Visitor::Builder->SetInsertPoint(funcBlock);

    // Keep the main function body's address
    if (funcAst.m_FuncName == "main") {
      MainFuncBB = funcBlock;
    }

    auto datalayout = Visitor::Module->getDataLayout();
    if (hasParam) {
      for (auto &param : function->args()) {
        size_t paramNo = param.getArgNo();
        std::string paramName = paramNames[paramNo];
        llvm::Type *paramType = paramTypes[paramNo];
        auto paramAst = (ParamAST *)funcAst.m_Params[paramNo].get();
        auto typeAst = (TypeAST *)paramAst->m_TypeNode.get();

        auto type = GetType(typeAst->m_TypeSpec, typeAst->m_IndirectLevel);
        uint64_t typeByte = datalayout.getPrefTypeAlign(type).value();

        if (typeAst->m_IsRef) {
          function->addParamAttr(paramNo, llvm::Attribute::NonNull);
          function->addDereferenceableParamAttr(paramNo, typeByte);
        }

        if (typeAst->m_IsRef || typeAst->m_IndirectLevel > 0) {
          llvm::Attribute attribute = llvm::Attribute::getWithAlignment(
              Visitor::Module->getContext(), llvm::Align(typeByte));
          function->addParamAttr(paramNo, attribute);
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
  if (funcCallAst.m_FuncSymbol == nullptr ||
      funcCallAst.m_FuncSymbol->m_ExprKind != ExprKind::SymbolExpr) {
    assert(false && "Function call target must be a symbol.");
  }

  auto *funcSymbol = static_cast<SymbolAST *>(funcCallAst.m_FuncSymbol.get());
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

  if (funcSymbol->m_SymbolName == "print") {
    auto *printfFunction = Visitor::Module->getFunction("printf");
    if (printfFunction == nullptr) {
      auto *printfType = llvm::FunctionType::get(
          Builder->getInt32Ty(), {GetI8PtrTy()}, true);
      printfFunction = llvm::Function::Create(
          printfType, llvm::Function::ExternalLinkage, "printf",
          *Visitor::Module);
      printfFunction->setCallingConv(llvm::CallingConv::C);
    }

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

      const char *format = "%s";
      if (argValue->getType()->isIntegerTy()) {
        format = "%lld";
        argValue = Builder->CreateIntCast(argValue, Builder->getInt64Ty(),
                                          m_LastSigned);
      } else if (argValue->getType()->isFloatTy()) {
        format = "%f";
        argValue = Builder->CreateFPExt(argValue, Builder->getDoubleTy());
      } else if (argValue->getType()->isDoubleTy()) {
        format = "%f";
      }

      auto *formatValue = Builder->CreateGlobalStringPtr(format);
      lastCall = Builder->CreateCall(printfFunction, {formatValue, argValue});
    }

    m_LastType = previousType;
    m_LastSigned = previousSigned;
    return lastCall;
  }

  if (funcSymbol->m_SymbolName == "input_int") {
    if (!argNodes.empty()) {
      assert(false && "Builtin 'input_int' does not accept arguments.");
    }

    auto *scanfFunction = Visitor::Module->getFunction("scanf");
    if (scanfFunction == nullptr) {
      auto *scanfType =
          llvm::FunctionType::get(Builder->getInt32Ty(), {GetI8PtrTy()}, true);
      scanfFunction = llvm::Function::Create(
          scanfType, llvm::Function::ExternalLinkage, "scanf",
          *Visitor::Module);
      scanfFunction->setCallingConv(llvm::CallingConv::C);
    }

    auto *valueType = Builder->getInt64Ty();
    auto *slot = Builder->CreateAlloca(valueType, nullptr, "input.int.slot");
    Builder->CreateStore(llvm::ConstantInt::get(valueType, 0), slot);

    auto *formatValue = Builder->CreateGlobalStringPtr("%lld");
    Builder->CreateCall(scanfFunction, {formatValue, slot});

    auto *value = Builder->CreateLoad(valueType, slot, "input.int");
    m_LastType = valueType;
    m_LastSigned = true;
    return value;
  }

  auto *function = Visitor::Module->getFunction(funcSymbol->m_SymbolName);
  if (function == nullptr) {
    assert(false && "Function must be declared before it is called.");
  }

  if (function->arg_size() != argNodes.size()) {
    assert(false && "Function call argument count mismatch.");
  }

  std::vector<llvm::Value *> args;
  llvm::Type *previousType = m_LastType;
  bool previousSigned = m_LastSigned;

  for (size_t i = 0; i < argNodes.size(); ++i) {
    llvm::Type *paramType = function->getFunctionType()->getParamType(i);
    m_LastType = paramType;
    m_LastSigned = true;

    llvm::Value *argValue = argNodes[i]->accept(*this);
    argValue = CastValueToType(argValue, paramType, m_LastSigned);
    args.push_back(argValue);
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
    for (auto &el : ifStmtAst.m_Cond.begin()->second.second) {
      thenVal = el->accept(*this);
    }

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
    for (auto &el : ifStmtAst.m_Cond.begin()->second.second) {
      thenVal = el->accept(*this);
    }

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

      for (auto &el : elifBBs[i].second.first) {
        thenVal = el->accept(*this);
      }

      CreateBranchIfNeeded(mergeBB);
      thenBB = Builder->GetInsertBlock();
    }

    // else
    if (ifStmtAst.m_HasElse) {
      Builder->SetInsertPoint(elseBB);

      for (auto &el : ifStmtAst.m_Else) {
        elseVal = el->accept(*this);
      }

      CreateBranchIfNeeded(mergeBB);
      elseBB = Builder->GetInsertBlock();
    }

    goto skip_else;
  }

  if (ifStmtAst.m_HasElse) {
    Builder->SetInsertPoint(elseBB);

    for (auto &el : ifStmtAst.m_Else) {
      elseVal = el->accept(*this);
    }
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

ValuePtr Visitor::visit(LoopStmtAST &loopStmtAst) {
  llvm::Function *parentFunc = Visitor::Builder->GetInsertBlock()->getParent();
  llvm::BasicBlock *headerBB = Builder->GetInsertBlock();
  llvm::BasicBlock *loopBB =
      llvm::BasicBlock::Create(Builder->getContext(), "loop", parentFunc);

  Builder->CreateBr(loopBB);
  Builder->SetInsertPoint(loopBB);

  if (loopStmtAst.m_RangeLoop) {  // this is basically high level for loop
    auto dataSymbol = (SymbolAST *)loopStmtAst.m_DataSymbol.get();
    auto symbolName = dataSymbol->m_SymbolName;

    llvm::Value *data = nullptr, *oldDataVal = nullptr;
    auto val = llvm::ConstantFP::get(Builder->getContext(), llvm::APFloat(0.0));
    //      auto newLocal = CreateLocalVariable("", Builder->getDoubleTy(),
    //      index);
    data = Visitor::Builder->CreateAlloca(Builder->getDoubleTy(), nullptr);
    auto newLocal = llvm::dyn_cast<llvm::AllocaInst>(data);
    Visitor::Builder->CreateStore(val, newLocal);

    if (m_LocalVarsOnScope.count(symbolName) > 0) {
      oldDataVal = m_LocalVarsOnScope[symbolName];
      m_LocalVarsOnScope[symbolName] = llvm::dyn_cast<llvm::AllocaInst>(data);
    } else {
      m_LocalVarsOnScope[symbolName] = llvm::dyn_cast<llvm::AllocaInst>(data);
    }

    /*llvm::Value *iterVal = nullptr;
    if (m_LocalVarsOnScope.count(symbolName) > 0) {
      iterVal = m_LocalVarsOnScope[symbolName];
    } else if (m_GlobalVars.count(symbolName) > 0) {
      iterVal = m_GlobalVars[symbolName];
    }*/

    llvm::Value *index = nullptr, *oldIndexVal = nullptr;
    if (loopStmtAst.m_Indexable) {
      val = llvm::ConstantFP::get(Builder->getContext(), llvm::APFloat(0.0));
      //      auto newLocal = CreateLocalVariable("", Builder->getDoubleTy(),
      //      index);
      index = Visitor::Builder->CreateAlloca(Builder->getDoubleTy(), nullptr);
      newLocal = llvm::dyn_cast<llvm::AllocaInst>(index);
      Visitor::Builder->CreateStore(val, newLocal);

      auto indexSymbol = (SymbolAST *)loopStmtAst.m_IndexSymbol.get();
      symbolName = indexSymbol->m_SymbolName;
      if (m_LocalVarsOnScope.count(symbolName) > 0) {
        oldIndexVal = m_LocalVarsOnScope[symbolName];
        m_LocalVarsOnScope[symbolName] =
            llvm::dyn_cast<llvm::AllocaInst>(index);
      } else {
        m_LocalVarsOnScope[symbolName] =
            llvm::dyn_cast<llvm::AllocaInst>(index);
      }
    } else {
      val = llvm::ConstantFP::get(Builder->getContext(), llvm::APFloat(0.0));
      index = Visitor::Builder->CreateAlloca(Builder->getDoubleTy(), nullptr);
      newLocal = llvm::dyn_cast<llvm::AllocaInst>(index);
      Visitor::Builder->CreateStore(val, newLocal);
    }

    if (loopStmtAst.m_HasNumericRange) {
    } else {
      auto iterableSymbol = (SymbolAST *)loopStmtAst.m_IterSymbol.get();
      auto iterSymbolName = iterableSymbol->m_SymbolName;

      llvm::Value *value = nullptr;
      bool isLocal = false;
      if (m_LocalVarsOnScope.count(iterSymbolName) > 0) {
        value = m_LocalVarsOnScope[iterSymbolName];
        isLocal = true;
      } else if (m_GlobalVars.count(iterSymbolName)) {
        value = m_GlobalVars[iterSymbolName];
      } else {
        assert(false && "WTF! This is not even possible!");
      }

      llvm::Type *type = value->getType();
      if (type->isPointerTy()) {
        type = GetPointeeType(value);
      }

      llvm::Value *arrSize = nullptr;
      if (type->isArrayTy()) {
        if (isLocal) {
          auto alloca = llvm::dyn_cast<llvm::AllocaInst>(value);
          auto numElements = alloca->getAllocatedType()->getArrayNumElements();
          arrSize = llvm::ConstantFP::get(Builder->getDoubleTy(),
                                          static_cast<int>(numElements));
        } else {
          auto globVar = llvm::dyn_cast<llvm::GlobalVariable>(value);
          auto numElements = globVar->getValueType()->getArrayNumElements();
          arrSize = llvm::ConstantFP::get(Builder->getDoubleTy(),
                                          static_cast<int>(numElements));
        }
      } else {
        assert(false &&
               "Only array type of iterable symbols are supported currently. "
               "SequenceTraits will be added later.");
      }

      llvm::Value *cond;
      index = Builder->CreateLoad(Builder->getDoubleTy(), index);
      index = Builder->CreateFAdd(
          llvm::ConstantFP::get(Builder->getContext(), llvm::APFloat(1.0)),
          index, "next");

      arrSize = Builder->CreateFSub(index, arrSize, "left");
      cond = Builder->CreateFCmpONE(
          arrSize,
          llvm::ConstantFP::get(Builder->getContext(), llvm::APFloat(0.0)),
          "loopcond");

      std::vector<llvm::Value *> idxList;
      idxList.push_back(llvm::ConstantInt::get(Builder->getInt64Ty(), 0));
      index = Builder->CreateFPToSI(index, Builder->getInt64Ty());
      idxList.push_back(index);

      auto gep = Builder->CreateInBoundsGEP(type, value, idxList);

      llvm::BasicBlock *loopEndBB = Builder->GetInsertBlock();
      llvm::BasicBlock *afterBB = llvm::BasicBlock::Create(
          Builder->getContext(), "afterloop", parentFunc);
      Builder->CreateCondBr(cond, loopBB, afterBB);
      Builder->SetInsertPoint(afterBB);

      //      Builder->CreateRet(llvm::Constant::getNullValue(Builder->getDoubleTy()));
      //      Builder->CreateRetVoid();
      //      pn->addIncoming(endVal, loopEndBB);
    }
  } else {  // this is basically a while loop.
    // conditions...
  }

  return nullptr;
}

ValuePtr Visitor::visit(ParamAST &paramAst) { return nullptr; }

ValuePtr Visitor::visit(RetAST &retAst) {
  llvm::Function *function = Visitor::Builder->GetInsertBlock()->getParent();
  llvm::Type *returnType = function->getReturnType();

  if (retAst.m_NoReturn) {
    if (returnType->isVoidTy()) {
      return Visitor::Builder->CreateRetVoid();
    }

    return Visitor::Builder->CreateRet(llvm::Constant::getNullValue(returnType));
  }

  llvm::Type *previousType = m_LastType;
  bool previousSigned = m_LastSigned;
  m_LastType = returnType;
  m_LastSigned = true;

  llvm::Value *value = retAst.m_RetExpr->accept(*this);
  m_LastType = previousType;
  m_LastSigned = previousSigned;

  if (value == nullptr) {
    return nullptr;
  }

  if (returnType->isVoidTy()) {
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

  return Visitor::Builder->CreateRet(value);
}

ValuePtr Visitor::visit(UnaryOpAST &unaryOpAst) {
  llvm::Value *value = nullptr;

  if (unaryOpAst.m_UnaryOpKind == UnaryOpKind::U_NEGATIVE) {
    this->m_LastNegConstant = true;
    value = unaryOpAst.m_Node->accept(*this);
    this->m_LastNegConstant = false;
  }

  if (unaryOpAst.m_UnaryOpKind == UnaryOpKind::U_REF) {
    if (unaryOpAst.m_Node == nullptr ||
        unaryOpAst.m_Node->m_ExprKind != ExprKind::SymbolExpr) {
      assert(false && "'ref' currently expects a symbol operand.");
    }

    auto *symbol = static_cast<SymbolAST *>(unaryOpAst.m_Node.get());
    if (m_LocalVarsOnScope.count(symbol->m_SymbolName) > 0) {
      value = m_LocalVarsOnScope[symbol->m_SymbolName];
    } else if (m_GlobalVars.count(symbol->m_SymbolName) > 0) {
      value = m_GlobalVars[symbol->m_SymbolName];
    } else {
      assert(false && "'ref' operand was not found in codegen symbol tables.");
    }

    m_LastType = value->getType();
  }

  if (unaryOpAst.m_UnaryOpKind == UnaryOpKind::U_DEREF) {
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

ValuePtr Visitor::visit(FixAST &fixAst) { return nullptr; }

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
