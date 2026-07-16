#include <visitor/codegen/codegen_private.hpp>

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
  auto referenceParam = m_ReferenceParamValueTypes.find(symbolAst.m_SymbolName);
  if (referenceParam != m_ReferenceParamValueTypes.end() && !m_LastArrayIndex) {
    auto *referenceSlotType = GetPointeeType(value);
    auto *referencedAddress = CreateLoad(
        value, referenceSlotType, symbolAst.m_SymbolName + ".ref.addr");
    auto *referencedType = referenceParam->second;
    auto *loadedValue = CreateLoad(referencedAddress, referencedType,
                                   symbolAst.m_SymbolName + ".ref");

    auto symbolInfo = getSymbolInfo(symbolAst.m_SymbolName);
    isSigned = IsSigned(GetEffectiveStorageType(symbolInfo.type,
                                                symbolInfo.definedTypeName));
    if (m_LastType == nullptr) {
      m_LastType = referencedType;
      m_LastSigned = isSigned;
      return loadedValue;
    }

    value = CastValueToType(loadedValue, m_LastType, isSigned);
    m_LastSigned = isSigned;
    return value;
  }

  auto arrayParam = m_ArrayParamValueTypes.find(symbolAst.m_SymbolName);
  if (arrayParam != m_ArrayParamValueTypes.end() && m_LastArrayIndex) {
    auto *slotType = GetPointeeType(value);
    auto *arrayAddress =
        CreateLoad(value, slotType, symbolAst.m_SymbolName + ".array.addr");
    m_LastType = arrayParam->second;
    auto symbolInfo = getSymbolInfo(symbolAst.m_SymbolName);
    m_LastSigned = IsSigned(GetEffectiveStorageType(symbolInfo.type,
                                                    symbolInfo.definedTypeName));
    return arrayAddress;
  }

  if (this->m_LastVarDecl && !this->m_LastArrayIndex) {
    valueType = GetPointeeType(value);

    auto symbolInfo = getSymbolInfo(symbolAst.m_SymbolName);
    isSigned = IsSigned(GetEffectiveStorageType(symbolInfo.type,
                                                symbolInfo.definedTypeName));
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
      auto symbolInfo = getSymbolInfo(symbolAst.m_SymbolName);
      m_LastSigned = IsSigned(GetEffectiveStorageType(symbolInfo.type,
                                                      symbolInfo.definedTypeName));
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
