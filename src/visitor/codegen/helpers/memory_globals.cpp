#include <visitor/codegen/codegen_private.hpp>

AssignmentTarget ResolveAssignmentTarget(llvm::Value *storage,
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

llvm::Value *CreateShortcutAssignmentValue(llvm::Value *currentValue,
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

llvm::GlobalVariable *CreateConstantGlobalVar(
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

llvm::GlobalVariable *CreateZeroInitConstantGlobalVar(
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

llvm::GlobalVariable *CreateInitConstantGlobalVar(
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
llvm::Function *CreateGlobalVarInitFunc() {
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
llvm::Function *CreateGlobalFuncSubToMain(
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

llvm::Value *CreateAlloca(const std::string &name, llvm::Type *type) {
  llvm::Function *parentFunc = Visitor::Builder->GetInsertBlock()->getParent();
  llvm::IRBuilder<> entryBuilder(
      &parentFunc->getEntryBlock(), parentFunc->getEntryBlock().begin());
  llvm::AllocaInst *var =
      entryBuilder.CreateAlloca(type, nullptr, llvm::Twine(name));

  return var;
}

llvm::Value *CreateLocalVariable(const std::string &name,
                                        llvm::Type *type, llvm::Value *value) {
  llvm::Function *parentFunc = Visitor::Builder->GetInsertBlock()->getParent();

  auto *var = llvm::dyn_cast<llvm::AllocaInst>(CreateAlloca(name, type));
  Visitor::Builder->CreateStore(value, var);

  return var;
}

llvm::FunctionCallee GetMallocFunction() {
  auto *intType = Visitor::Builder->getInt64Ty();
  auto *ptrType = GetI8PtrTy();
  auto *fnType = llvm::FunctionType::get(ptrType, {intType}, false);
  return Visitor::Module->getOrInsertFunction("malloc", fnType);
}

llvm::FunctionCallee GetFreeFunction() {
  auto *ptrType = GetI8PtrTy();
  auto *fnType = llvm::FunctionType::get(Visitor::Builder->getVoidTy(),
                                         {ptrType}, false);
  return Visitor::Module->getOrInsertFunction("free", fnType);
}

llvm::Value *CreateDefaultHeapAlloc(llvm::Type *type,
                                           const llvm::Twine &name) {
  auto size = Visitor::Module->getDataLayout().getTypeAllocSize(type);
  auto *sizeValue =
      llvm::ConstantInt::get(Visitor::Builder->getInt64Ty(), size);
  return Visitor::Builder->CreateCall(GetMallocFunction(), {sizeValue}, name);
}

void CreateDefaultHeapFree(llvm::Value *ptr) {
  auto *asI8 = Visitor::Builder->CreatePointerCast(ptr, GetI8PtrTy());
  Visitor::Builder->CreateCall(GetFreeFunction(), {asI8});
}

llvm::Value *CreateLoad(llvm::Value *address, llvm::Type *type,
                               const llvm::Twine &name) {
  return Visitor::Builder->CreateLoad(type, address, name);
}

llvm::Value *CreateAtomicLoad(llvm::Value *address, llvm::Type *type,
                                     const llvm::Twine &name) {
  auto *load = Visitor::Builder->CreateLoad(type, address, name);
  auto dl = Visitor::Module->getDataLayout();
  load->setAtomic(llvm::AtomicOrdering::Acquire);
  load->setAlignment(dl.getPrefTypeAlign(type));
  return load;
}

llvm::Value *CreateSharedPointerHandle(llvm::Value *data,
                                              llvm::Value *refCount) {
  auto *handleType = GetSharedPointerTy();
  llvm::Value *handle = llvm::UndefValue::get(handleType);
  data = Visitor::Builder->CreatePointerCast(data, GetI8PtrTy());
  refCount = Visitor::Builder->CreatePointerCast(refCount, GetI8PtrTy());
  handle = Visitor::Builder->CreateInsertValue(handle, data, {0}, "sp.data");
  handle = Visitor::Builder->CreateInsertValue(handle, refCount, {1}, "sp.count");
  return handle;
}

llvm::Value *CreateNullSharedPointerHandle() {
  auto *nullPtr = llvm::ConstantPointerNull::get(GetI8PtrTy());
  return CreateSharedPointerHandle(nullPtr, nullPtr);
}

llvm::Value *ExtractSharedPointerData(llvm::Value *handle) {
  return Visitor::Builder->CreateExtractValue(handle, {0}, "sp.data");
}

llvm::Value *ExtractSharedPointerCount(llvm::Value *handle) {
  return Visitor::Builder->CreateExtractValue(handle, {1}, "sp.count");
}

llvm::Value *CreateSharedPointerCounter(const std::string &name,
                                               uint64_t initialValue) {
  auto *counterType = Visitor::Builder->getInt64Ty();
  auto *counter = CreateAlloca(name + ".strong", counterType);
  Visitor::Builder->CreateStore(
      llvm::ConstantInt::get(counterType, initialValue), counter);
  return counter;
}

void AtomicBumpSharedPointer(llvm::Value *handle, int64_t delta) {
  auto *countAsI8 = ExtractSharedPointerCount(handle);
  auto *isLive = CreateNormalizedICmp(
      llvm::CmpInst::ICMP_NE, countAsI8,
      llvm::ConstantPointerNull::get(GetI8PtrTy()), "sp.count.live");
  auto *function = Visitor::Builder->GetInsertBlock()->getParent();
  auto *bumpBB = llvm::BasicBlock::Create(Visitor::Builder->getContext(),
                                          "sp.bump", function);
  auto *contBB = llvm::BasicBlock::Create(Visitor::Builder->getContext(),
                                          "sp.bump.cont", function);
  Visitor::Builder->CreateCondBr(isLive, bumpBB, contBB);
  Visitor::Builder->SetInsertPoint(bumpBB);

  auto *count = Visitor::Builder->CreatePointerCast(
      countAsI8, llvm::PointerType::get(Visitor::Builder->getContext(), 0));
  const auto magnitude = delta < 0 ? static_cast<uint64_t>(-delta)
                                   : static_cast<uint64_t>(delta);
  auto *amount = llvm::ConstantInt::get(Visitor::Builder->getInt64Ty(),
                                        magnitude, false);
  Visitor::Builder->CreateAtomicRMW(
      delta >= 0 ? llvm::AtomicRMWInst::Add : llvm::AtomicRMWInst::Sub, count,
      amount, llvm::MaybeAlign(), llvm::AtomicOrdering::AcquireRelease);
  Visitor::Builder->CreateBr(contBB);
  Visitor::Builder->SetInsertPoint(contBB);
}

void RetainSharedPointer(llvm::Value *handle) {
  AtomicBumpSharedPointer(handle, 1);
}

void ReleaseSharedPointer(llvm::Value *handle) {
  AtomicBumpSharedPointer(handle, -1);
}

llvm::Value *LoadSharedPointerStrongCount(llvm::Value *handle) {
  auto *countAsI8 = ExtractSharedPointerCount(handle);
  auto *isLive = CreateNormalizedICmp(
      llvm::CmpInst::ICMP_NE, countAsI8,
      llvm::ConstantPointerNull::get(GetI8PtrTy()), "sp.count.live");
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

llvm::Value *FindStorage(std::map<std::string, llvm::AllocaInst*> &locals,
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

void Visitor::registerScopeSharedPointerRelease(const SymbolInfo &symbolInfo) {
  if (!IsSharedPointerSymbol(symbolInfo) || symbolInfo.isGlob) {
    return;
  }

  m_CodegenDroppedSymbols.erase(symbolInfo.symbolName);
  m_ScopeSharedPointerReleases.push_back(symbolInfo);
}
