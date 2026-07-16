#include <visitor/codegen/codegen_private.hpp>

ValuePtr Visitor::visit(BinaryOpAST &binaryOpAst) {
  llvm::Value *value = nullptr;

  if (m_LastVarDecl) {
    llvm::ArrayType *arrayType = nullptr;

    if (m_LastInitializerList) {
      arrayType =
          llvm::ArrayType::get(m_LastType, FlatArrayLength(m_LastArrayDims));
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
          auto symbol = LastGlobVarRef;

          size_t i = 0;
          for (auto &el : elements) {
            value = CreateInitializerValue(*this, el, m_LastType);
            auto *gep =
                CreateArrayElementAddress(GetPointeeType(symbol), symbol, i++);
            Builder->CreateStore(value, gep);
          }

          // value = Visitor::Builder->CreateStore(value, LastGlobVarRef);
          Visitor::Builder->CreateRetVoid();
        } else {
          // there is no binary operation in the initializer list so just
          // initialize the array
          std::vector<llvm::Constant *> values;

          for (auto &el : elements) {
            values.push_back(CreateLiteralConstant(m_LastType, el.value));
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
        auto *ptr = llvm::dyn_cast<llvm::AllocaInst>(
            CreateAlloca("array.init", arrayType));
        std::vector<BinOpOrVal> elements;
        getElementsOfArray(binaryOpAst, elements);

        if (IsAnyBinOpOrSymbolInvolved(elements)) {
          size_t i = 0;
          for (auto &el : elements) {
            value = CreateInitializerValue(*this, el, m_LastType);
            auto *gep = CreateArrayElementAddress(arrayType, ptr, i++);
            Builder->CreateStore(value, gep);
          }

          value = ptr;
        } else {
          std::vector<llvm::Constant *> values;
          for (auto &el : elements) {
            values.push_back(CreateLiteralConstant(m_LastType, el.value));
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
