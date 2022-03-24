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

  for (auto &symbol : LocalSymbolTable[m_LastFuncName]) {
    if (symbol.symbolName == symbolName) {
      symbolInfo = symbol.symbolInfo;
      break;
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
  auto align = dl.getPrefTypeAlignment(type);
  globVar->setAlignment(llvm::Align(align));

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
  auto align = dl.getPrefTypeAlignment(type);
  globVar->setAlignment(llvm::Align(align));

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
  auto align = dl.getPrefTypeAlignment(type);
  globVar->setAlignment(llvm::Align(align));

  if (specifier == VisibilitySpecifier::VIS_STATIC) {
    globVar->setLinkage(llvm::GlobalValue::InternalLinkage);
    globVar->setInitializer(value);
  } else if (specifier == VisibilitySpecifier::VIS_DEFAULT) {
    // ConstantStruct
    globVar->setDSOLocal(true);
    globVar->setInitializer(value);
  } else if (specifier == VisibilitySpecifier::VIS_EXPORT ||
             specifier == VisibilitySpecifier::VIS_IMPORT) {
    globVar->setDSOLocal(true);
    globVar->setLinkage(llvm::GlobalValue::ExternalLinkage);
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
  llvm::Value *value = nullptr;

  auto lhs = binaryOpAst.m_LHS->accept(*this);
  auto rhs = binaryOpAst.m_RHS->accept(*this);

  // NSW (No Signed Wrap) and NUW(No Unsigned Wrap)
  switch (binaryOpAst.m_BinOpKind) {
    case B_ADD: {
      if (m_LastSigned)
        value = Builder->CreateNUWAdd(lhs, rhs, "addtmp");
      else
        value = Builder->CreateNSWAdd(lhs, rhs, "addtmp");
      //            Builder->CreateStore(value, alloca);
      break;
    }
    case B_SUB:
      if (m_LastSigned)
        value = Builder->CreateNUWSub(lhs, rhs, "subtmp");
      else
        value = Builder->CreateNSWSub(lhs, rhs, "subtmp");
      break;
    case B_MUL:
      if (m_LastSigned)
        value = Builder->CreateNUWMul(lhs, rhs, "subtmp");
      else
        value = Builder->CreateNSWMul(lhs, rhs, "subtmp");
      break;
    case B_DIV: {
      if (m_LastType->isDoubleTy() || m_LastType->isFloatTy())
        value = Builder->CreateFDiv(lhs, rhs, "subtmp");
      else {
        if (m_LastSigned)
          value = Builder->CreateSDiv(lhs, rhs, "subtmp");
        else
          value = Builder->CreateUDiv(lhs, rhs, "subtmp");
      }
      break;
    }
    case B_MOD:
      if (m_LastType->isDoubleTy() || m_LastType->isFloatTy())
        value = Builder->CreateFRem(lhs, rhs, "sremtmp");
      else {
        if (m_LastSigned)
          value = Builder->CreateSRem(lhs, rhs, "subtmp");
        else
          value = Builder->CreateURem(lhs, rhs, "subtmp");
      }

      break;
    case B_AND:
      value = Builder->CreateAnd(lhs, rhs, "andtemp");
      break;
    case B_LAND:
      value = Builder->CreateLogicalAnd(lhs, rhs, "andtemp");
      break;
    case B_OR:
      value = Builder->CreateOr(lhs, rhs, "andtemp");
      break;
    case B_LOR:
      value = Builder->CreateLogicalOr(lhs, rhs, "andtemp");
      break;
    case B_XOR:
      value = Builder->CreateXor(lhs, rhs, "andtemp");
      break;
    case B_GT:
      if (m_LastType->isFloatTy() || m_LastType->isDoubleTy()) {
        value = Builder->CreateFCmpOGT(lhs, rhs, "fcmpogttemp");
      } else {
        if (m_LastSigned) {
          value = Builder->CreateICmpSGT(lhs, rhs, "cmpsgttemp");
          if (value->getType()->getIntegerBitWidth() == 1) {
            value = Builder->CreateIntCast(value, m_LastType, false);
          }
        } else {
          value = Builder->CreateICmpUGT(lhs, rhs, "cmpugttemp");
          if (value->getType()->getIntegerBitWidth() == 1) {
            value = Builder->CreateIntCast(value, m_LastType, true);
          }
        }
      }
      break;
    case B_GTEQ:
      if (m_LastType->isFloatTy() || m_LastType->isDoubleTy()) {
        value = Builder->CreateFCmpOGE(lhs, rhs, "fcmpogetemp");
      } else {
        if (m_LastSigned) {
          value = Builder->CreateICmpSGE(lhs, rhs, "cmpsgetemp");
          if (value->getType()->getIntegerBitWidth() == 1) {
            value = Builder->CreateIntCast(value, m_LastType, false);
          }
        } else {
          value = Builder->CreateICmpUGE(lhs, rhs, "cmpugetemp");
          if (value->getType()->getIntegerBitWidth() == 1) {
            value = Builder->CreateIntCast(value, m_LastType, true);
          }
        }
      }
      break;
    case B_LT:
      if (m_LastType->isFloatTy() || m_LastType->isDoubleTy()) {
        value = Builder->CreateFCmpOLT(lhs, rhs, "fcmpolttemp");
      } else {
        if (m_LastSigned) {
          value = Builder->CreateICmpSLT(lhs, rhs, "cmpslttemp");
          if (value->getType()->getIntegerBitWidth() == 1) {
            value = Builder->CreateIntCast(value, m_LastType, false);
          }
        } else {
          value = Builder->CreateICmpULT(lhs, rhs, "cmpulttemp");
          if (value->getType()->getIntegerBitWidth() == 1) {
            value = Builder->CreateIntCast(value, m_LastType, true);
          }
        }
      }
      break;
    case B_LTEQ:
      if (m_LastType->isFloatTy() || m_LastType->isDoubleTy()) {
        value = Builder->CreateFCmpOLE(lhs, rhs, "fcmposletemp");
      } else {
        if (m_LastSigned) {
          value = Builder->CreateICmpSLE(lhs, rhs, "cmpsletemp");
          if (value->getType()->getIntegerBitWidth() == 1) {
            value = Builder->CreateIntCast(value, m_LastType, false);
          }
        } else {
          value = Builder->CreateICmpULE(lhs, rhs, "cmpuletemp");
          if (value->getType()->getIntegerBitWidth() == 1) {
            value = Builder->CreateIntCast(value, m_LastType, true);
          }
        }
      }
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
      if (m_LastType->isDoubleTy() || m_LastType->isFloatTy()) {
        value = Builder->CreateFCmp(llvm::CmpInst::Predicate::FCMP_OEQ, lhs,
                                    rhs, "fcmptemp");
      } else {
        value = Builder->CreateICmpEQ(lhs, rhs, "icmptemp");
        if (value->getType()->getIntegerBitWidth() == 1) {
          value = Builder->CreateIntCast(value, m_LastType, false);
        }
      }
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
    case B_ARRS:
      break;
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
      //       value = Builder->CreateInBoundsGEP()
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

  if (!this->m_LastVarDecl)
    m_LastType = value->getType()->getPointerElementType();

  isSigned = IsSigned(getSymbolInfo(symbolAst.m_SymbolName).type);
  llvm::Type *type = value->getType();

  if (value->getType()->isPointerTy()) {
    type = value->getType()->getPointerElementType();
  }

  llvm::LoadInst *loadedVal = nullptr;
  if (m_LastGlobVar) {
    auto dl = Visitor::Module->getDataLayout();
    auto align = dl.getPrefTypeAlignment(type);
    loadedVal = Builder->CreateAlignedLoad(type, value, llvm::MaybeAlign(align),
                                           llvm::Twine(""));
  } else {
    loadedVal = Builder->CreateLoad(type, value, llvm::Twine(""));
  }

  llvm::Type *lastType = m_LastType;
  if (m_LastType->isPointerTy()) {
    m_LastType = lastType->getPointerElementType();
  }

  if (type->getTypeID() != lastType->getTypeID()) {
    if (m_LastType->isFloatTy() || m_LastType->isDoubleTy()) {
      if (isSigned) {
        value = Builder->CreateSIToFP(loadedVal, m_LastType);
      } else {
        value = Builder->CreateUIToFP(loadedVal, m_LastType);
      }
    }
  } else {
    value = loadedVal;
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
      if (!m_LastSigned) {  // unsigned
        auto scalarVal = std::stoull(scalarAst.m_Value);

        int maxVal = 0;
        if (m_LastType->isArrayTy()) {
          auto type = m_LastType->getArrayElementType();
          maxVal = 1 << (type->getIntegerBitWidth() - 1);
        } else {
          maxVal = 1 << (m_LastType->getIntegerBitWidth() - 1);
        }

        // -1 for 0
        maxVal = scalarVal > 0 ? maxVal - 1 : maxVal;

        // two's complement
        if (scalarVal > maxVal) {
          // - 1 for 0
          scalarVal = (0 - ~scalarVal) - ((scalarVal > 0) ? 1 : 0);
        }

        value = llvm::ConstantInt::get(m_LastType, scalarVal);
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
    size_t length = 0;
    for (auto &dim : m_LastArrayDims) {
      length += dim;
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
    }

    value = varAst.m_RHS->accept(*this);

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
          if (value->getType()->getIntegerBitWidth() == 1) {
            value = Builder->CreateBitCast(value, type);
            type = Builder->getInt1Ty();
          }

          this->m_LocalVarsOnScope[varAst.m_Name] =
              llvm::dyn_cast<llvm::AllocaInst>(
                  CreateLocalVariable(varAst.m_Name, type, value));
        }
      } else {
        if (varAst.m_IsInitializerList) {
          globVar = CreateInitConstantGlobalVar(
              varAst.m_Name, varAst.m_VisibilitySpec, arrayType,
              llvm::dyn_cast<llvm::Constant>(value));
        }
        this->m_GlobalVars[varAst.m_Name] = globVar;
      }
    }
  } else {
    // if no data bound
    if (!varAst.m_IsLocal) {
      llvm::GlobalVariable *globVar = nullptr;
      if (varAst.m_IsInitializerList) {
        CreateZeroInitConstantGlobalVar(varAst.m_Name, varAst.m_VisibilitySpec,
                                        arrayType);
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

ValuePtr Visitor::visit(AssignmentAST &assignmentAst) {}

ValuePtr Visitor::visit(BinaryOpAST &binaryOpAst) {
  llvm::Value *value = nullptr;

  if (m_LastVarDecl) {
    llvm::ArrayType *arrayType = nullptr;

    if (m_LastInitializerList) {
      size_t length = 0;
      for (auto &dim : m_LastArrayDims) {
        length += dim;
      }
      arrayType = llvm::ArrayType::get(m_LastType, length);
    }

    if (m_LastGlobVar) {
      if (m_LastInitializerList) {
        std::vector<BinOpOrVal> elements;
        getElementsOfArray(binaryOpAst, elements);
        if (IsAnyBinOpOrSymbolInvolved(elements)) {
          // TODO: Global Init Functionality just like in C++:
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
        value = Builder->CreateAlloca(arrayType, nullptr);
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
                             funcAst.m_FuncName, *Visitor::Module);
  function->setCallingConv(llvm::CallingConv::C);

  if (!funcAst.m_IsForwardDecl) {
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

llvm::BranchInst *Visitor::createBranch(IAST &ifCond, llvm::BasicBlock *thenBB,
                                        llvm::BasicBlock *elseBB,
                                        llvm::BasicBlock *mergeBB,
                                        bool elif = false) {
  auto cond = ifCond.accept(*this);

  // tempBuilder, parentFunc
  // if(!cond->getType()->isFloatTy() && !cond->getType()->isDoubleTy())
  //   cond = Builder->CreateFPCast(cond, Builder->getDoubleTy());
  if (m_LastSigned) {
    cond = Builder->CreateSIToFP(cond, Builder->getDoubleTy());
  } else {
    cond = Builder->CreateUIToFP(cond, Builder->getDoubleTy());
  }

  cond = Builder->CreateFCmpONE(
      cond, llvm::ConstantFP::get(Builder->getContext(), llvm::APFloat(0.0)),
      "ifcond");

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

  // tempBuilder, parentFunc
  // if(!cond->getType()->isFloatTy() && !cond->getType()->isDoubleTy())
  //   cond = Builder->CreateFPCast(cond, Builder->getDoubleTy());
  if (m_LastSigned) {
    cond = Builder->CreateSIToFP(cond, Builder->getDoubleTy());
  } else {
    cond = Builder->CreateUIToFP(cond, Builder->getDoubleTy());
  }

  cond = Builder->CreateFCmpONE(
      cond, llvm::ConstantFP::get(Builder->getContext(), llvm::APFloat(0.0)),
      "ifcond");

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

    Builder->CreateBr(mergeBB);
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

    Builder->CreateBr(mergeBB);
    thenBB = Builder->GetInsertBlock();
    // ---

    // elif
    for (int i = 0; i < elifBBs.size(); i++) {
      Builder->SetInsertPoint(elifBBs[i].second.second);

      auto &elifCond = elifBBs[i].first;
      cond = elifCond->accept(*this);

      if (m_LastSigned) {
        cond = Builder->CreateSIToFP(cond, Builder->getDoubleTy());
      } else {
        cond = Builder->CreateUIToFP(cond, Builder->getDoubleTy());
      }

      cond = Builder->CreateFCmpONE(
          cond,
          llvm::ConstantFP::get(Builder->getContext(), llvm::APFloat(0.0)),
          "elifcond");

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

      Builder->CreateBr(mergeBB);
      thenBB = Builder->GetInsertBlock();
    }

    // else
    if (ifStmtAst.m_HasElse) {
      Builder->SetInsertPoint(elseBB);

      for (auto &el : ifStmtAst.m_Else) {
        elseVal = el->accept(*this);
      }

      Builder->CreateBr(mergeBB);
      elseBB = Builder->GetInsertBlock();
    }

    goto skip_else;
  }

  if (ifStmtAst.m_HasElse) {
    Builder->SetInsertPoint(elseBB);

    for (auto &el : ifStmtAst.m_Else) {
      elseVal = el->accept(*this);
    }
    Builder->CreateBr(mergeBB);
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

  llvm::PHINode *pn = Builder->CreatePHI(Builder->getDoubleTy(), 2);

  if (loopStmtAst.m_RangeLoop) {  // this is basically high level for loop
    auto dataSymbol = (SymbolAST *)loopStmtAst.m_DataSymbol.get();
    auto symbolName = dataSymbol->m_SymbolName;

    // we're keeping the old value for restoring..
    llvm::Value *oldVal = nullptr;
    if (m_LocalVarsOnScope.count(symbolName) > 0) {
      oldVal = m_LocalVarsOnScope[symbolName];
    }

    if (loopStmtAst.m_Indexable) {
    } else {
    }

    if (loopStmtAst.m_HasNumericRange) {
    } else {
      auto iterableSymbol = (SymbolAST *)loopStmtAst.m_IterSymbol.get();
      auto *value = m_LocalVarsOnScope[iterableSymbol->m_SymbolName];
      if (value->isArrayAllocation()) {
      } else {
        assert(false &&
               "Only array type of iterable symbols are supported currently. "
               "Struct types or SequenceTraits will be added later.");
      }
    }
  } else {  // this is basically a while loop.
    // conditions...
  }
}

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

void Visitor::finalizeCodegen() {
  auto function = CreateGlobalFuncSubToMain(this->m_GlobaInitVarFunc);
  llvm::verifyFunction(*function);

  std::vector<llvm::Type *> types;
  types.push_back(Builder->getInt32Ty());
  types.push_back(function->getType());
  types.push_back(Builder->getInt8PtrTy());

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
  values.push_back(llvm::ConstantPointerNull::get(Builder->getInt8PtrTy()));
  auto *ctorVal = llvm::ConstantStruct::get(structType, values);

  std::vector<llvm::Constant *> ctorValues;
  ctorValues.push_back(ctorVal);
  auto init = llvm::ConstantArray::get(type, ctorValues);
  globVar->setInitializer(init);
}