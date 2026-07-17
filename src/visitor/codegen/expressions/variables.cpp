#include <visitor/codegen/codegen_private.hpp>

ValuePtr Visitor::visit(VarAST &varAst) {
  llvm::Value *value;
  this->m_LastVarDecl = true;
  const auto definedTypeName = DefinedTypeNameFromVarAst(varAst);
  auto type = varAst.m_IsDynamicTraitObject
                  ? GetDynamicTraitObjectTy()
                  : GetStorageType(varAst.m_TypeSpec, varAst.m_IndirectLevel,
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
      uint64_t dimension = 1;
      TryParseUnsignedIntegerLiteral(val->m_Value, dimension);
      this->m_LastArrayDims.push_back(static_cast<size_t>(dimension));
    }
    size_t length = FlatArrayLength(m_LastArrayDims);
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

      if (varAst.m_IsLocal) {
        auto *storage = llvm::dyn_cast<llvm::AllocaInst>(
            CreateAlloca(varAst.m_Name, arrayType));
        auto *zero = llvm::ConstantAggregateZero::get(arrayType);
        Builder->CreateStore(zero, storage);

        for (size_t i = 0; i < elements.size(); ++i) {
          auto *elementValue =
              CreateInitializerValue(*this, elements[i], m_LastType);
          elementValue = CastValueToType(elementValue, type, m_LastSigned);
          auto *slot = CreateArrayElementAddress(arrayType, storage, i);
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

        auto *constant = CreateLiteralConstant(m_LastType, el.value);
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
          auto *elementValue =
              CreateInitializerValue(*this, elements[i], m_LastType);
          elementValue = CastValueToType(elementValue, type, m_LastSigned);
          auto *slot = CreateArrayElementAddress(arrayType, globVar, i);
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
    auto *rhsCast = dynamic_cast<CastOpAST *>(varAst.m_RHS.get());
    auto dynamicMoveSource = [&]() -> SymbolAST * {
      if (rhsCast == nullptr ||
          rhsCast->m_CastOpKind != CastOpKind::C_DYNAMIC_MOVE_AS ||
          rhsCast->m_Node == nullptr ||
          rhsCast->m_Node->m_ExprKind != ExprKind::SymbolExpr) {
        return nullptr;
      }
      return static_cast<SymbolAST *>(rhsCast->m_Node.get());
    };

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
          if (auto *moveSource = dynamicMoveSource()) {
            auto sourceInfo = getSymbolInfo(moveSource->m_SymbolName);
            auto *sourceStorage =
                FindStorage(m_LocalVarsOnScope, m_GlobalVars,
                            moveSource->m_SymbolName);
            if (sourceStorage != nullptr) {
              auto *sourceType =
                  GetStorageType(sourceInfo.type, sourceInfo.indirectionLevel,
                                 sourceInfo.isUnique, sourceInfo.isRef,
                                 sourceInfo.definedTypeName);
              Builder->CreateStore(llvm::Constant::getNullValue(sourceType),
                                   sourceStorage);
            }

            auto heapIt = m_HeapAllocations.find(moveSource->m_SymbolName);
            if (heapIt != m_HeapAllocations.end()) {
              m_HeapAllocations[varAst.m_Name] = heapIt->second;
              m_HeapAllocations.erase(heapIt);
            }
            m_CodegenDroppedSymbols.insert(moveSource->m_SymbolName);
            m_CodegenDroppedSymbols.erase(varAst.m_Name);
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

  if (varAst.m_IsLocal) {
    registerScopeSharedPointerRelease(getSymbolInfo(varAst.m_Name));
  }

  if (varAst.m_IsLocal && varAst.m_TypeSpec == TypeSpecifier::SPEC_DEFINED) {
    registerScopeDestructor(getSymbolInfo(varAst.m_Name));
  }

  return value;
}
