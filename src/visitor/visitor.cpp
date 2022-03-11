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

SymbolInfo Visitor::getSymbolInfo(const std::string &symbolName) {
  SymbolInfo symbolInfo;

  for (auto &symbol : m_LocalSymbolTable[m_LastFuncName]) {
    if (symbol.symbolName == symbolName) {
      symbolInfo = symbol.symbolInfo;
      break;
    }
  }

  return symbolInfo;
}

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

ValuePtr Visitor::visit(SymbolAST &symbolAst) {
  llvm::Value *value = nullptr;
  bool isSigned = false;

  llvm::Function *parentFunc = Visitor::Builder->GetInsertBlock()->getParent();
  llvm::IRBuilder<> tempBuilder(&(parentFunc->getEntryBlock()),
                                parentFunc->getEntryBlock().end());
  if (this->m_LastVarDecl) {
    value = m_LocalVarsOnScope[symbolAst.m_SymbolName];
    isSigned = IsSigned(getSymbolInfo(symbolAst.m_SymbolName).type);
    llvm::Type *type = value->getType();

    if (value->getType()->isPointerTy()) {
      type = value->getType()->getPointerElementType();
    }
    auto loadedVal = tempBuilder.CreateLoad(type, value, llvm::Twine(""));

    llvm::Type *lastType = m_LastType;
    if (m_LastType->isPointerTy()) {
      lastType->getPointerElementType();
    }

    if (type->getTypeID() != lastType->getTypeID()) {
      if (m_LastType->isFloatTy() || m_LastType->isDoubleTy()) {
        if (isSigned) {
          value = tempBuilder.CreateSIToFP(loadedVal, m_LastType);
        } else {
          value = tempBuilder.CreateUIToFP(loadedVal, m_LastType);
        }
      }
    } else {
      if (value->getType()->getTypeID() != m_LastType->getTypeID()) {
      }
    }

  } else {
    value = m_LocalVarsOnScope[symbolAst.m_SymbolName];
  }

  return value;
}

ValuePtr Visitor::visit(ScalarOrLiteralAST &scalarAst) {
  llvm::Value *value = nullptr;

  // TODO: scalarast has type info.. perform castings

  if (m_LastType->isFloatTy() || m_LastType->isDoubleTy()) {
    value = llvm::ConstantFP::get(m_LastType, scalarAst.m_Value);
  } else {
    if (scalarAst.m_IsLiteral) {
      value = Visitor::Builder->CreateGlobalStringPtr(
          llvm::StringRef(scalarAst.m_Value));
    } else {
      if (!m_LastSigned) {
        auto scalarVal = std::stoull(scalarAst.m_Value);
        value = llvm::ConstantInt::get(m_LastType, scalarVal);
        /*value = llvm::ConstantInt::get(
            m_LastType, llvm::APInt(128, "-" + scalarAst.m_Value, 10));*/
      } else {
        if (scalarAst.m_IsLetter) {
          char val = scalarAst.m_Value[0];
          value = llvm::ConstantInt::get(m_LastType, (int)val);
        } else {
          value = llvm::ConstantInt::get(m_LastType,
                                         std::stoull(scalarAst.m_Value));
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
  this->m_LastSigned = IsSigned(varAst.m_TypeSpec);
  this->m_LastInitializerList = varAst.m_IsInitializerList;

  if (varAst.m_IsInitializerList) {
    this->m_LastArrayDims.clear();

    for (auto &dim : varAst.m_ArrDim) {
      auto val = (ScalarOrLiteralAST *)dim.get();
      this->m_LastArrayDims.push_back(std::stoull(val->m_Value));
      
      // for the right order
      std::reverse(this->m_LastArrayDims.begin(), this->m_LastArrayDims.end());
    }
  }

  if (varAst.m_RHS != nullptr) {
    value = varAst.m_RHS->accept(*this);

    if (varAst.m_IsInitializerList) {
      value->setName(varAst.m_Name);
    }
  } else {
    // if no data bound
    if (varAst.m_IsInitializerList) {
      auto first = this->m_LastArrayDims[0];
      auto arrayType = llvm::ArrayType::get(type, first);
      for (int i = 1; i < this->m_LastArrayDims.size(); i++) {
        arrayType = llvm::ArrayType::get(arrayType, this->m_LastArrayDims[i]);
      }
      llvm::Function *parentFunc =
          Visitor::Builder->GetInsertBlock()->getParent();
      llvm::IRBuilder<> tempBuilder(&(parentFunc->getEntryBlock()),
                                    parentFunc->getEntryBlock().end());

      value = tempBuilder.CreateAlloca(arrayType, nullptr, varAst.m_Name);
    } else {
      if (type->isFloatTy() || type->isDoubleTy()) {
        value = llvm::ConstantFP::get(type, 0.0f);
      } else {
        value = llvm::ConstantInt::get(type, 0);
      }
    }
  }

  if (!varAst.m_IsLocal) {
    Visitor::Module->getOrInsertGlobal(llvm::StringRef(varAst.m_Name), type);
    auto globVar =
        Visitor::Module->getNamedGlobal(llvm::StringRef(varAst.m_Name));

    auto *constant = llvm::dyn_cast<llvm::Constant>(value);
    if (!constant) {
      assert(false && "Impossible!");
    }

    if (varAst.m_VisibilitySpec == VisibilitySpecifier::VIS_STATIC) {
      globVar->setLinkage(llvm::GlobalValue::InternalLinkage);
      globVar->setInitializer(constant);
    } else if (varAst.m_VisibilitySpec == VisibilitySpecifier::VIS_DEFAULT) {
      // ConstantStruct
      globVar->setDSOLocal(true);
      globVar->setInitializer(constant);
    } else if (varAst.m_VisibilitySpec == VisibilitySpecifier::VIS_EXPORT ||
               varAst.m_VisibilitySpec == VisibilitySpecifier::VIS_IMPORT) {
      globVar->setDSOLocal(true);
      globVar->setLinkage(llvm::GlobalValue::ExternalLinkage);
    }

    this->m_GlobalVars[varAst.m_Name] = globVar;
    value = globVar;
  } else {
    if (value != nullptr) {
      if (varAst.m_IsInitializerList) {
        m_LocalVarsOnScope[varAst.m_Name] =
            llvm::dyn_cast<llvm::AllocaInst>(value);
      } else {
        llvm::Function *parentFunc =
            Visitor::Builder->GetInsertBlock()->getParent();
        llvm::IRBuilder<> tempBuilder(&(parentFunc->getEntryBlock()),
                                      parentFunc->getEntryBlock().end());
        llvm::AllocaInst *var =
            tempBuilder.CreateAlloca(type, nullptr, llvm::Twine(varAst.m_Name));

        m_LocalVarsOnScope[varAst.m_Name] = var;
        Visitor::Builder->CreateStore(value, var);
      }
    } else {
      assert(false && "'value' is nullptr");
    }
  }
  this->m_LastVarDecl = false;
  this->m_LastType = nullptr;
  this->m_LastSigned = false;

  return value;
}

ValuePtr Visitor::visit(AssignmentAST &assignmentAst) { return nullptr; }
ValuePtr Visitor::visit(BinaryOpAST &binaryOpAst) {
  llvm::Value *value = nullptr;

  llvm::Function *parentFunc = Visitor::Builder->GetInsertBlock()->getParent();
  llvm::IRBuilder<> tempBuilder(&(parentFunc->getEntryBlock()),
                                parentFunc->getEntryBlock().end());

  if (m_LastVarDecl && m_LastInitializerList) {
    auto first = this->m_LastArrayDims[0];
    auto arrayType = llvm::ArrayType::get(m_LastType, first);
    for (int i = 1; i < this->m_LastArrayDims.size(); i++) {
      arrayType = llvm::ArrayType::get(arrayType, this->m_LastArrayDims[i]);
    }

    value = tempBuilder.CreateAlloca(arrayType, nullptr);
  } else {
  }

  return value;
}

ValuePtr Visitor::visit(CastOpAST &castOpAst) {}

ValuePtr Visitor::visit(FuncAST &funcAst) {
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
ValuePtr Visitor::visit(UnaryOpAST &unaryOpAst) {
  llvm::Value *value = nullptr;

  if (unaryOpAst.m_UnaryOpKind == UnaryOpKind::U_NEGATIVE) {
    this->m_LastNegConstant = true;
    value = unaryOpAst.m_Node->accept(*this);
    this->m_LastNegConstant = false;
  }

  return value;
}
ValuePtr Visitor::visit(TypeAST &typeAst) {}

ValuePtr Visitor::visit(FixAST &fixAst) {}
