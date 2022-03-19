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

static llvm::Value *CreateAlloca(const std::string &name, llvm::Type *type) {
  llvm::Function *parentFunc = Visitor::Builder->GetInsertBlock()->getParent();
  llvm::IRBuilder<> tempBuilder(&(parentFunc->getEntryBlock()),
                                parentFunc->getEntryBlock().begin());
  llvm::AllocaInst *var =
      Visitor::Builder->CreateAlloca(type, nullptr, llvm::Twine(name));

  return var;
}

static llvm::Value *CreateLocalVariable(const std::string &name,
                                        llvm::Type *type, llvm::Value *value) {
  llvm::Function *parentFunc = Visitor::Builder->GetInsertBlock()->getParent();
  llvm::IRBuilder<> tempBuilder(&(parentFunc->getEntryBlock()),
                                parentFunc->getEntryBlock().end());
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
    case B_MARRS:
      break;
  }

  return value;
}

ValuePtr Visitor::visit(SymbolAST &symbolAst) {
  llvm::Value *value = nullptr;
  bool isSigned = false;

  llvm::Function *parentFunc = Visitor::Builder->GetInsertBlock()->getParent();
  llvm::IRBuilder<> tempBuilder(&(parentFunc->getEntryBlock()),
                                parentFunc->getEntryBlock().end());
  // if (this->m_LastVarDecl) {
  value = m_LocalVarsOnScope[symbolAst.m_SymbolName];
  if (!this->m_LastVarDecl)
    m_LastType = value->getType()->getPointerElementType();

  isSigned = IsSigned(getSymbolInfo(symbolAst.m_SymbolName).type);
  llvm::Type *type = value->getType();

  if (value->getType()->isPointerTy()) {
    type = value->getType()->getPointerElementType();
  }
  auto loadedVal = Builder->CreateLoad(type, value, llvm::Twine(""));

  llvm::Type *lastType = m_LastType;
  if (m_LastType->isPointerTy()) {
    lastType->getPointerElementType();
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

  /* } else {
     value = m_LocalVarsOnScope[symbolAst.m_SymbolName];
     llvm::Type *type = value->getType();
     while (type->isPointerTy()) {
       type = value->getType()->getPointerElementType();
     }
     m_LastType = type;
   }*/

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
        auto maxVal = 1 << m_LastType->getIntegerBitWidth();

        // two's complement
        if (scalarVal > maxVal) {
          scalarVal = 0 - ~scalarVal;
        }

        value = llvm::ConstantInt::get(m_LastType, scalarVal);
      } else {
        if (scalarAst.m_IsLetter) {
          char val = scalarAst.m_Value[0];
          value = llvm::ConstantInt::get(m_LastType, (int)val);
        } else {
          auto scalarVal = std::stoll(scalarAst.m_Value);
          auto maxVal = 1 << (m_LastType->getIntegerBitWidth() - 1);

          // -1 for 0
          maxVal = scalarVal > 0 ? maxVal - 1 : maxVal;

          // two's complement
          if (scalarVal > maxVal) {
            // - 1 for 0
            scalarVal = (0 - ~scalarVal) - ((scalarVal > 0) ? 1 : 0);
          }

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
  llvm::ArrayType *arrayType = nullptr;

  if (varAst.m_IsInitializerList) {
    this->m_LastArrayDims.clear();

    for (auto &dim : varAst.m_ArrDim) {
      auto val = (ScalarOrLiteralAST *)dim.get();
      this->m_LastArrayDims.push_back(std::stoull(val->m_Value));

      // for the right order
      std::reverse(this->m_LastArrayDims.begin(), this->m_LastArrayDims.end());
    }
    auto first = this->m_LastArrayDims[0];
    arrayType = llvm::ArrayType::get(type, first);
    for (int i = 1; i < this->m_LastArrayDims.size(); i++) {
      arrayType = llvm::ArrayType::get(arrayType, this->m_LastArrayDims[i]);
    }
  }

  if (varAst.m_RHS != nullptr) {
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
        auto globVar = CreateConstantGlobalVar(
            varAst.m_Name, varAst.m_VisibilitySpec, type, value);
        this->m_GlobalVars[varAst.m_Name] = globVar;
      }
    } else {
      if (varAst.m_IsLocal) {
        if (varAst.m_IsInitializerList) {
          value->setName(varAst.m_Name);
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
        auto globVar =
            varAst.m_IsInitializerList
                ? CreateInitConstantGlobalVar(
                      varAst.m_Name, varAst.m_VisibilitySpec, arrayType,
                      llvm::dyn_cast<llvm::Constant>(value))
                : CreateConstantGlobalVar(varAst.m_Name,
                                          varAst.m_VisibilitySpec, type, value);
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
        CreateConstantGlobalVar(varAst.m_Name, varAst.m_VisibilitySpec, type,
                                value);
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

ValuePtr Visitor::visit(AssignmentAST &assignmentAst) { return nullptr; }
ValuePtr Visitor::visit(BinaryOpAST &binaryOpAst) {
  llvm::Value *value = nullptr;

  if (m_LastVarDecl) {
    llvm::ArrayType *arrayType = nullptr;

    if (m_LastInitializerList) {
      auto first = this->m_LastArrayDims[0];
      arrayType = llvm::ArrayType::get(m_LastType, first);
      for (int i = 1; i < this->m_LastArrayDims.size(); i++) {
        arrayType = llvm::ArrayType::get(arrayType, this->m_LastArrayDims[i]);
      }
    }

    if (m_LastGlobVar) {
      std::vector<BinOpOrVal> elements;
      getElementsOfArray(binaryOpAst, elements);

      if (IsAnyBinOpOrSymbolInvolved(elements)) {
      } else {
        std::vector<llvm::Constant *> init_list;

        size_t offset = 0;
        // FIXME: It does not work as expected
        for (auto &dim : this->m_LastArrayDims) {
          std::vector<llvm::Constant *> values;
          for (size_t i = offset; i < offset + dim; i++) {
            auto e = elements[i];
            auto scalarVal = std::stoull(e.value);
            auto v = llvm::ConstantInt::get(m_LastType, scalarVal);
            values.push_back(v);
          }
          offset += dim;
          llvm::Constant *init = llvm::ConstantArray::get(arrayType, values);
          init_list.push_back(init);
        }

        if (init_list.size() == 1) {
          value = init_list[0];
        } else {
          llvm::Constant *init = llvm::ConstantArray::get(arrayType, init_list);
          value = init;
        }
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
    elseBB = llvm::BasicBlock::Create(Builder->getContext(), "", parentFunc);

    auto branchInst = Builder->CreateCondBr(cond, thenBB, elseBB);

    Builder->SetInsertPoint(thenBB);
    // if block
    for (auto &el : ifStmtAst.m_Cond.begin()->second.second) {
      thenVal = el->accept(*this);
    }

    Builder->CreateBr(mergeBB);
    thenBB = Builder->GetInsertBlock();
  }

  if (ifStmtAst.m_HasElif) {
    if (ifStmtAst.m_HasElse) {
      elseBB = llvm::BasicBlock::Create(Builder->getContext(), "elifelse",
                                        parentFunc);

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

      if (elifBBs.size() == 1) {
        auto branchInst =
            Builder->CreateCondBr(cond, thenBB, elifBBs[0].second.second);

        Builder->SetInsertPoint(thenBB);
        // if block
        for (auto &el : elifBBs[0].second.first) {
          thenVal = el->accept(*this);
        }

        // br elif
        Builder->CreateBr(elifBBs[0].second.second);
        thenBB = Builder->GetInsertBlock();

        // elif
        Builder->SetInsertPoint(elifBBs[0].second.second);
        auto &elifCond = elifBBs[0].first;
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

        for (auto &el : elifBBs[0].second.first) {
          thenVal = el->accept(*this);
        }

        Builder->CreateBr(elseBB);
        thenBB = Builder->GetInsertBlock();

        // else
        Builder->SetInsertPoint(elseBB);

        for (auto &el : ifStmtAst.m_Else) {
          elseVal = el->accept(*this);
        }

        Builder->CreateBr(mergeBB);
        elseBB = Builder->GetInsertBlock();

      } else {
        // Else which comes after elif
        llvm::BasicBlock *elseBBAfterElif = llvm::BasicBlock::Create(
            Builder->getContext(), "elifelse", parentFunc);
        Builder->SetInsertPoint(elseBBAfterElif);

        for (int i = 0; i < elifBBs.size(); i++) {
          if (i != elifBBs.size()) {
          } else {
            // last else..
          }
        }

        // else which contains all the elifs and their else
        Builder->SetInsertPoint(elseBB);

        for (auto &el : ifStmtAst.m_Else) {
          elseVal = el->accept(*this);
        }

        Builder->CreateBr(elseBB);
        elseBB = Builder->GetInsertBlock();
      }

    } else {
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

  return nullptr;
}

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
