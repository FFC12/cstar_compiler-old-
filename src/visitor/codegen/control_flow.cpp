#include <visitor/codegen/codegen_private.hpp>

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
  auto *indexSlot =
      llvm::dyn_cast<llvm::AllocaInst>(CreateAlloca("loop.index", indexType));
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

    auto *slot = llvm::dyn_cast<llvm::AllocaInst>(CreateAlloca(name, type));
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

  emitScopeExitDestructors();
  return Builder->CreateBr(m_LoopBreakTargets.back());
}

ValuePtr Visitor::visit(ContinueStmtAST &continueStmtAst) {
  if (m_LoopContinueTargets.empty()) {
    return nullptr;
  }

  emitScopeExitDestructors();
  return Builder->CreateBr(m_LoopContinueTargets.back());
}

ValuePtr Visitor::visit(DropStmtAST &dropStmtAst) {
  return emitDropForSymbol(dropStmtAst.m_SymbolName, true);
}

ValuePtr Visitor::visit(DeferStmtAST &deferStmtAst) {
  if (!m_EmittingDefers) {
    m_ActiveDefers.push_back(&deferStmtAst);
  }
  return nullptr;
}

ValuePtr Visitor::visit(ThrowStmtAST &throwStmtAst) {
  if (throwStmtAst.m_ErrorExpr != nullptr) {
    throwStmtAst.m_ErrorExpr->accept(*this);
  }

  llvm::Function *function = Builder->GetInsertBlock()->getParent();
  llvm::Type *returnType = function->getReturnType();
  emitScopeExitDestructors();
  if (returnType->isVoidTy()) {
    return Builder->CreateRetVoid();
  }
  return Builder->CreateRet(llvm::Constant::getNullValue(returnType));
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
