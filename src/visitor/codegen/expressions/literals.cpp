#include <visitor/codegen/codegen_private.hpp>

ValuePtr Visitor::visit(ScalarOrLiteralAST &scalarAst) {
  llvm::Value *value = nullptr;

  // TODO: m_LastType is always null for the binary operation which is not RHS
  // of any vardecl.
  if (m_LastType == nullptr) {
    m_LastType = scalarAst.m_IsFloat ? Builder->getDoubleTy()
                                     : Builder->getInt64Ty();
  }

  if (m_LastArrayIndex) {
    m_IndicesAsStr.emplace_back(scalarAst.m_Value, scalarAst.m_IsFloat);
    // }
    // if (!m_LastVarDecl) {
    if (scalarAst.m_IsFloat) {
      value = llvm::ConstantFP::get(Builder->getDoubleTy(), scalarAst.m_Value);
    } else {
      if (m_LastType != nullptr) {
        if (m_LastType->isDoubleTy() || m_LastType->isFloatTy()) {
          value = llvm::ConstantFP::get(m_LastType, scalarAst.m_Value);
        } else {
          auto scalarVal = std::stoll(scalarAst.m_Value);
          value = llvm::ConstantInt::get(Builder->getInt64Ty(), scalarVal);
        }
      }
    }
  } else {
    if (m_LastType->isFloatTy() || m_LastType->isDoubleTy()) {
      value = llvm::ConstantFP::get(m_LastType, scalarAst.m_Value);
    } else {
      if (scalarAst.m_IsLiteral) {
        auto literalValue = DecodeCStringEscapes(scalarAst.m_Value);
        value = CreateGlobalStringPointer(llvm::StringRef(literalValue));
      } else if (scalarAst.m_IsBoolean) {
        value = llvm::ConstantInt::get(
            m_LastType, scalarAst.m_Value == "true" ? 1 : 0);
      } else {
        if (!m_LastSigned) {  // unsigned
          auto scalarVal = std::stoull(scalarAst.m_Value);

          llvm::Type *type = m_LastType;
          if (type->isPointerTy()) {
            type = GetPointeeType(value);
          }

          int maxVal = 0;
          if (type->isArrayTy()) {
            type = type->getArrayElementType();
            maxVal = 1 << (type->getIntegerBitWidth() - 1);
          } else {
            maxVal = 1 << (type->getIntegerBitWidth() - 1);
          }

          // -1 for 0
          maxVal = scalarVal > 0 ? maxVal - 1 : maxVal;

          // two's complement
          if (scalarVal > maxVal) {
            // - 1 for 0
            scalarVal = (0 - ~scalarVal) - ((scalarVal > 0) ? 1 : 0);
          }

          value = llvm::ConstantInt::get(type, scalarVal);
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
  }

  return value;
}
