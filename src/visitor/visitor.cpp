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

static llvm::Type *GetType(TypeSpecifier typeSpecifier, size_t indirectLevel,
                           bool isRef = false) {
  llvm::Type *type = nullptr;
  switch (typeSpecifier) {
    case SPEC_VOID:
      if (indirectLevel == 0)
        type = Visitor::Builder->getVoidTy();
      else
        type = Visitor::Builder->getInt8PtrTy();
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
        type = llvm::IntegerType::getFloatTy(Visitor::Module->getContext());
      break;
    case SPEC_F64:
      if (indirectLevel == 0)
        type = Visitor::Builder->getDoubleTy();
      else
        type = llvm::IntegerType::getDoubleTy(Visitor::Module->getContext());
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
        type = llvm::IntegerType::getDoubleTy(Visitor::Module->getContext());
#else
        type = llvm::IntegerType::getFloatTy(Visitor::Module->getContext());
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
    type = llvm::PointerType::get(type, 0);
    for (int i = 1; i < indirectLevel; i++)
      type = llvm::PointerType::get(type, 0);
  }

  return type;
}

ValuePtr Visitor::visit(VarAST &varAst) {
  llvm::Value *value;

  auto type = GetType(varAst.m_TypeSpec, varAst.m_IndirectLevel);
  if (varAst.m_RHS != nullptr) {
    value = varAst.m_RHS->accept(*this);
  } else {
    // if no data bound
    if(type->isFloatTy() || type->isDoubleTy()) {
      value = llvm::ConstantFP::get(type, 0.0f);
    }  else {
      value = llvm::ConstantInt::get(type, 0);
    }
  }

  if(value != nullptr) {
    llvm::Function *parentFunc =
        Visitor::Builder->GetInsertBlock()->getParent();
    llvm::IRBuilder<> tempBuilder(&(parentFunc->getEntryBlock()),
                                  parentFunc->getEntryBlock().end());

    llvm::AllocaInst *var =
        tempBuilder.CreateAlloca(type, nullptr, llvm::Twine(varAst.m_Name));

    m_LocalVarsOnScope[varAst.m_Name] = var;
    Visitor::Builder->CreateStore(value, var);
  } else {
    assert(false && "'value' is nullptr");
  }
  return value;
}

ValuePtr Visitor::visit(AssignmentAST &assignmentAst) {
  return nullptr;
}
ValuePtr Visitor::visit(BinaryOpAST &binaryOpAst) {}
ValuePtr Visitor::visit(CastOpAST &castOpAst) {}

ValuePtr Visitor::visit(FuncAST &funcAst) {
  auto retType = (TypeAST *)funcAst.m_RetType.get();

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

  llvm::FunctionType *functionType = nullptr;

  if (hasParam) {
    functionType = llvm::FunctionType::get(
        GetType(retType->m_TypeSpec, retType->m_IndirectLevel), paramTypes,
        false);
  } else {
    functionType = llvm::FunctionType::get(
        GetType(retType->m_TypeSpec, retType->m_IndirectLevel),
        std::vector<llvm::Type *>(), false);
  }

  // auto functionType =
  // llvm::FunctionType::get(Visitor::Builder->getVoidTy(),false);

  auto function =
      llvm::Function::Create(functionType, llvm::Function::ExternalLinkage,
                             funcAst.m_FuncName, *Module);
  function->setCallingConv(llvm::CallingConv::C);

  if (!funcAst.m_IsForwardDecl) {
    llvm::BasicBlock *funcBlock = llvm::BasicBlock::Create(
        Visitor::Builder->getContext(), "entry", function);
    Visitor::Builder->SetInsertPoint(funcBlock);

    auto datalayout = Visitor::Module->getDataLayout();
    if (hasParam) {
      for (auto &param : function->args()) {
        size_t paramNo = param.getArgNo();
        std::string paramName = paramNames[paramNo];
        llvm::Type *paramType = paramTypes[paramNo];
        auto paramAst = (ParamAST *)funcAst.m_Params[paramNo].get();
        auto typeAst = (TypeAST *)paramAst->m_TypeNode.get();

        auto type = GetType(typeAst->m_TypeSpec, typeAst->m_IndirectLevel);
        uint64_t typeByte = datalayout.getPrefTypeAlignment(type);

        if (typeAst->m_IsRef) {
          function->addAttribute(paramNo + 1, llvm::Attribute::NonNull);
          function->addDereferenceableAttr(paramNo + 1, typeByte);
        }

        if (typeAst->m_IsRef || typeAst->m_IndirectLevel > 0) {
          llvm::Attribute attribute = llvm::Attribute::getWithAlignment(
              Visitor::Module->getContext(), llvm::Align(typeByte));
          function->addAttribute(paramNo + 1, attribute);
        }

        m_LocalVarsOnScope[paramName] = Visitor::Builder->CreateAlloca(
            paramType, nullptr, llvm::Twine(paramName));
        Visitor::Builder->CreateStore(&param, m_LocalVarsOnScope[paramName]);
      }
    }

    for (auto &localExpr : funcAst.m_Scope) {
      auto expr = localExpr->accept(*this);
    }

    if (function->getReturnType()->isVoidTy()) {
      Visitor::Builder->CreateRetVoid();
    } else {
      auto retType = function->getReturnType();
      //      Visitor::Builder->getInt()
      Visitor::Builder->CreateRet(Visitor::Builder->getInt64(1));
      //      Visitor::Builder->CreateRetVoid();
    }
  }

  return function;
}

ValuePtr Visitor::visit(FuncCallAST &funcCallAst) {}
ValuePtr Visitor::visit(IfStmtAST &ifStmtAst) {}
ValuePtr Visitor::visit(LoopStmtAST &loopStmtAst) {}
ValuePtr Visitor::visit(ParamAST &paramAst) {}
ValuePtr Visitor::visit(RetAST &retAst) {}
ValuePtr Visitor::visit(UnaryOpAST &unaryOpAst) {}
ValuePtr Visitor::visit(TypeAST &typeAst) {}
ValuePtr Visitor::visit(ScalarOrLiteralAST &scalarAst) {}
ValuePtr Visitor::visit(SymbolAST &symbolAst) {
  return nullptr;
}
ValuePtr Visitor::visit(FixAST &fixAst) {}
