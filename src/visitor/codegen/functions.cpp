#include <visitor/codegen/codegen_private.hpp>

llvm::Function *Visitor::declareFunction(FuncAST &funcAst) {
  if (auto *function = Visitor::Module->getFunction(funcAst.m_FuncName)) {
    return function;
  }

  auto *retType = static_cast<TypeAST *>(funcAst.m_RetType.get());
  const bool nativeAbi = funcAst.m_IsForwardDecl || funcAst.m_IsExported;
  auto paramLayout = buildFunctionParamLayout(funcAst, nativeAbi);
  const auto retDefinedTypeName = DefinedTypeNameFromTypeAst(retType);
  auto *returnType =
      retType->m_IsDynamicTraitObject
          ? static_cast<llvm::Type *>(GetDynamicTraitObjectTy())
      : nativeAbi && retType->m_IndirectLevel > 0
          ? GetType(retType->m_TypeSpec, retType->m_IndirectLevel,
                    retType->m_IsRef)
          : GetStorageType(retType->m_TypeSpec, retType->m_IndirectLevel,
                           retType->m_IsUniquePtr, retType->m_IsRef,
                           retDefinedTypeName);

  auto *functionType = llvm::FunctionType::get(
      returnType,
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
  auto paramLayout =
      buildFunctionParamLayout(funcAst, funcAst.m_IsForwardDecl ||
                                            funcAst.m_IsExported);

  if (!funcAst.m_IsForwardDecl) {
    if (!function->empty()) {
      return function;
    }

    m_LocalVarsOnScope.clear();
    m_ReferenceParamValueTypes.clear();
    m_ArrayParamValueTypes.clear();
    m_SpanParamElementTypes.clear();
    m_ScopeDestructors.clear();
    m_ScopeSharedPointerReleases.clear();
    m_CodegenDroppedSymbols.clear();
    m_HeapAllocations.clear();
    m_ActiveDefers.clear();
    m_EmittingDefers = false;
    m_CodegenProtocolStates.clear();
    m_CodegenProtocolStateOrder.clear();
    m_EmittingProtocolScopeExit = false;

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
        if (paramLayout.isSpan[paramNo]) {
          m_SpanParamElementTypes[paramName] = paramLayout.elementTypes[paramNo];
        }
        m_LocalVarsOnScope[paramName] = llvm::dyn_cast<llvm::AllocaInst>(
            CreateAlloca(paramName, paramType));
        Visitor::Builder->CreateStore(&param, m_LocalVarsOnScope[paramName]);
        auto signatureIt = FunctionTable.find(funcAst.m_FuncName);
        if (signatureIt != FunctionTable.end() &&
            paramNo < signatureIt->second.params.size()) {
          auto paramInfo = signatureIt->second.params[paramNo];
          paramInfo.symbolName = paramName;
          registerScopeSharedPointerRelease(paramInfo);
        }
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

  if (funcName == "unsafe_span") {
    if (argNodes.size() != 2) {
      assert(false && "Builtin 'unsafe_span' expects pointer and length.");
    }

    llvm::Type *previousType = m_LastType;
    bool previousSigned = m_LastSigned;

    m_LastType = nullptr;
    m_LastSigned = false;
    auto *ptr = argNodes[0]->accept(*this);

    m_LastType = Builder->getInt64Ty();
    m_LastSigned = false;
    auto *length = argNodes[1]->accept(*this);
    length = CastValueToType(length, Builder->getInt64Ty(), false);

    m_LastType = previousType;
    m_LastSigned = previousSigned;
    auto *span = CreateSpanValue(ptr, length);
    m_LastType = GetSpanTy();
    m_LastSigned = false;
    return span;
  }

  std::string dynamicTraitName;
  std::string dynamicMethodName;
  if (ParseDynamicTraitDispatchName(funcName, dynamicTraitName,
                                    dynamicMethodName)) {
    if (argNodes.empty() ||
        argNodes[0]->m_ExprKind != ExprKind::SymbolExpr) {
      assert(false && "Dynamic trait method call requires a symbol receiver.");
    }

    std::string concreteMethodName;
    const auto *signature = FindDynamicTraitMethodSignature(
        dynamicTraitName, dynamicMethodName, &concreteMethodName);
    auto *concreteFunction = Module->getFunction(concreteMethodName);
    if (signature == nullptr || concreteFunction == nullptr) {
      assert(false &&
             "Dynamic trait method dispatch requires a concrete signature.");
    }

    auto *receiverSymbol = static_cast<SymbolAST *>(argNodes[0]);
    auto *receiverStorage = FindStorage(m_LocalVarsOnScope, m_GlobalVars,
                                        receiverSymbol->m_SymbolName);
    if (receiverStorage == nullptr) {
      assert(false && "Dynamic trait receiver storage was not found.");
    }

    auto *object = Builder->CreateLoad(GetDynamicTraitObjectTy(),
                                       receiverStorage,
                                       receiverSymbol->m_SymbolName + ".dyn");
    auto *dataPtr = Builder->CreateExtractValue(object, {0}, "dyn.data");
    auto *vtablePtr = Builder->CreateExtractValue(object, {1}, "dyn.vtable");
    auto *vtableTy = GetDynamicTraitVTableTy(dynamicTraitName);
    const auto methodIndex =
        DynamicTraitMethodIndex(dynamicTraitName, dynamicMethodName);
    if (vtableTy == nullptr || methodIndex < 0) {
      assert(false && "Dynamic trait vtable method was not found.");
    }

    auto *methodSlot = Builder->CreateStructGEP(
        vtableTy, vtablePtr, static_cast<unsigned>(methodIndex),
        "dyn.method.slot");
    auto *methodPtr = Builder->CreateLoad(GetI8PtrTy(), methodSlot,
                                          "dyn.method");

    std::vector<llvm::Value *> args;
    args.push_back(dataPtr);

    llvm::Type *previousType = m_LastType;
    bool previousSigned = m_LastSigned;
    for (size_t i = 1; i < argNodes.size(); ++i) {
      auto *paramType = concreteFunction->getFunctionType()->getParamType(i);
      m_LastType = paramType;
      m_LastSigned = true;
      auto *argValue = argNodes[i]->accept(*this);
      argValue = CastValueToType(argValue, paramType, m_LastSigned);
      args.push_back(argValue);
    }
    m_LastType = previousType;
    m_LastSigned = previousSigned;

    auto *call = Builder->CreateCall(concreteFunction->getFunctionType(),
                                     methodPtr, args,
                                     concreteFunction->getReturnType()->isVoidTy()
                                         ? ""
                                         : "dyn.call");
    if (!concreteFunction->getReturnType()->isVoidTy()) {
      m_LastType = concreteFunction->getReturnType();
    }
    return call;
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
        signatureIt->second.params[i].isSubscriptable &&
        !signatureIt->second.params[i].isRuntimeSizedArray;
    const bool expectsSpan =
        signatureIt != FunctionTable.end() &&
        i < signatureIt->second.params.size() &&
        signatureIt->second.params[i].isRuntimeSizedArray;
    if (expectsSpan) {
      m_LastType = paramType;
      m_LastSigned = false;
      args.push_back(argNodes[i]->accept(*this));
      continue;
    }

    if (expectsArray) {
      auto copySource = [](IAST *node) -> SymbolAST * {
        if (node == nullptr || node->m_ExprKind != ExprKind::UnaryOp) {
          return nullptr;
        }
        auto *unary = static_cast<UnaryOpAST *>(node);
        if (unary->m_UnaryOpKind != U_COPY ||
            unary->m_Node->m_ExprKind != ExprKind::SymbolExpr) {
          return nullptr;
        }
        return static_cast<SymbolAST *>(unary->m_Node.get());
      };

      auto *argSymbol = copySource(argNodes[i]);
      const bool explicitCopy = argSymbol != nullptr;
      if (argSymbol == nullptr && argNodes[i]->m_ExprKind == ExprKind::SymbolExpr) {
        argSymbol = static_cast<SymbolAST *>(argNodes[i]);
      }

      if (argSymbol == nullptr) {
        assert(false && "Array parameter arguments must be symbols.");
      }

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

      if (explicitCopy) {
        auto *arrayType = GetPointeeType(storage);
        if (!arrayType->isArrayTy()) {
          auto sourceArrayType =
              m_ArrayParamValueTypes.find(argSymbol->m_SymbolName);
          if (sourceArrayType != m_ArrayParamValueTypes.end()) {
            arrayType = sourceArrayType->second;
          }
        }
        auto *copyStorage = llvm::dyn_cast<llvm::AllocaInst>(
            CreateAlloca(argSymbol->m_SymbolName + ".copy", arrayType));
        auto copySize = Module->getDataLayout().getTypeAllocSize(arrayType);
        Builder->CreateMemCpy(
            Builder->CreateBitCast(copyStorage, GetI8PtrTy()),
            copyStorage->getAlign(),
            Builder->CreateBitCast(storage, GetI8PtrTy()),
            llvm::MaybeAlign(
                Module->getDataLayout().getPrefTypeAlign(arrayType).value()),
            copySize);
        storage = copyStorage;
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
        paramInfo != nullptr && IsSharedPointerSymbol(*paramInfo) &&
        paramType != nullptr && IsSharedPointerTy(paramType);

    if (!isVariadicArg && paramInfo != nullptr && paramInfo->isRef &&
        !paramInfo->isDynamicTraitObject &&
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

  applyCodegenProtocolMethodCall(funcName, funcCallAst);
  return call;
}

ValuePtr Visitor::visit(NewAST &newAst) {
  const bool targetIsDefined = newAst.m_TypeSpec == TypeSpecifier::SPEC_DEFINED;
  llvm::Type *payloadType = nullptr;
  if (targetIsDefined) {
    payloadType = GetDefinedStructTy(newAst.m_TypeName);
    if (payloadType == nullptr) {
      assert(false && "`new` target struct type must exist.");
    }
  } else {
    payloadType = GetType(newAst.m_TypeSpec, 0);
    if (payloadType == nullptr || payloadType->isVoidTy()) {
      assert(false && "`new` target primitive type must be sized.");
    }
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
  collectArgs(collectArgs, newAst.m_Args.get());

  llvm::Value *storage = nullptr;
  const auto allocationSize = Module->getDataLayout().getTypeAllocSize(
      payloadType);
  const auto allocationAlign = Module->getDataLayout().getPrefTypeAlign(
      payloadType).value();
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
    storage = CreateDefaultHeapAlloc(payloadType, "new.malloc");
  }

  auto *payloadPointerType =
      targetIsDefined ? GetType(TypeSpecifier::SPEC_DEFINED, 1)
                      : GetType(newAst.m_TypeSpec, 1);
  storage = Builder->CreatePointerCast(storage, payloadPointerType);

  auto *constructor = targetIsDefined
                          ? Module->getFunction(newAst.m_TypeName +
                                                ".constructor")
                          : nullptr;
  if (constructor != nullptr) {
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
  } else if (!targetIsDefined) {
    llvm::Type *previousType = m_LastType;
    bool previousSigned = m_LastSigned;

    if (!argNodes.empty()) {
      m_LastType = payloadType;
      m_LastSigned = IsSigned(newAst.m_TypeSpec);
      auto *initializer = argNodes[0]->accept(*this);
      initializer = CastValueToType(initializer, payloadType, m_LastSigned);
      Builder->CreateStore(initializer, storage);
    }

    m_LastType = previousType;
    m_LastSigned = previousSigned;
  }

  if (newAst.m_IsShared) {
    llvm::Value *counterRaw = nullptr;
    if (newAst.m_Allocator != nullptr &&
        newAst.m_Allocator->m_ExprKind == ExprKind::SymbolExpr) {
      auto *allocatorSymbol =
          static_cast<SymbolAST *>(newAst.m_Allocator.get());
      auto allocatorInfo = getSymbolInfo(allocatorSymbol->m_SymbolName);
      auto *allocatorStorage =
          FindStorage(m_LocalVarsOnScope, m_GlobalVars,
                      allocatorSymbol->m_SymbolName);
      auto *allocFunction = Module->getFunction(allocatorInfo.definedTypeName +
                                                ".alloc");
      if (allocatorStorage != nullptr && allocFunction != nullptr) {
        if (allocatorInfo.isRef &&
            m_ReferenceParamValueTypes.count(allocatorSymbol->m_SymbolName) >
                0) {
          auto *slotType = GetPointeeType(allocatorStorage);
          allocatorStorage = CreateLoad(
              allocatorStorage, slotType,
              allocatorSymbol->m_SymbolName + ".allocator.count.ref");
        }
        auto *countSize =
            llvm::ConstantInt::get(Builder->getInt64Ty(), 8);
        auto *countAlign =
            llvm::ConstantInt::get(Builder->getInt64Ty(), 8);
        counterRaw = Builder->CreateCall(
            allocFunction, {allocatorStorage, countSize, countAlign},
            "new.shared.count.alloc");
      }
    }
    if (counterRaw == nullptr) {
      counterRaw = CreateDefaultHeapAlloc(Builder->getInt64Ty(),
                                          "new.shared.count");
    }
    auto *one = llvm::ConstantInt::get(Builder->getInt64Ty(), 1);
    Builder->CreateStore(one, counterRaw);
    auto *handle = CreateSharedPointerHandle(storage, counterRaw);
    m_LastType = GetSharedPointerTy();
    m_LastSigned = false;
    return handle;
  }

  m_LastType = payloadPointerType;
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
