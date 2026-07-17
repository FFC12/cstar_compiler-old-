#include <visitor/semantic/semantic_private.hpp>

SymbolInfo Visitor::preVisit(ScalarOrLiteralAST &scalarAst) {
  SymbolInfo symbolInfo;

  symbolInfo.begin = scalarAst.m_SemLoc.begin;
  symbolInfo.end = scalarAst.m_SemLoc.end;
  symbolInfo.line = scalarAst.m_SemLoc.line;

  if (scalarAst.m_IsNil) {
    symbolInfo.type = TypeSpecifier::SPEC_NIL;
    symbolInfo.value = scalarAst.m_Value;
    if (this->m_TypeChecking) {
      this->m_TypeErrorMessages.emplace_back(
          "`nil` requires an explicitly nullable pointer type (`T*?` or "
          "`T^?`); plain `T*`, `T^` and `T&` are non-null by default",
          symbolInfo, DiagnosticCode::SemanticQualifierMismatch);
    }
    return symbolInfo;
  }

  if (this->m_TypeChecking) {
    if (this->m_LastReferenced) {
      this->m_TypeErrorMessages.emplace_back(
          "Constant value cannot be referenced", symbolInfo);
    } else {
      if (!this->m_DefinedTypeFlag && !this->m_LastCondExpr) {
        if (this->m_ExpectedType == TypeSpecifier::SPEC_BOOL &&
            scalarAst.m_IsBoolean) {
        } else if ((this->m_ExpectedType == TypeSpecifier::SPEC_FLOAT ||
                    this->m_ExpectedType == TypeSpecifier::SPEC_F32 ||
                    this->m_ExpectedType == TypeSpecifier::SPEC_F64) &&
                   scalarAst.m_IsIntegral && scalarAst.m_IsFloat) {
        } else if (this->m_ExpectedType == TypeSpecifier::SPEC_VOID &&
                   this->m_LastSymbolInfo.indirectionLevel == 0) {
          this->m_TypeErrorMessages.emplace_back(
              "'void' is an incomplete type and cannot be used as a "
              "declaration type",
              m_LastSymbolInfo);
        } else if ((this->m_ExpectedType == TypeSpecifier::SPEC_CHAR ||
                    this->m_ExpectedType == TypeSpecifier::SPEC_UCHAR) &&
                   scalarAst.m_IsLetter &&
                   this->m_LastSymbolInfo.indirectionLevel == 0) {
        } else if ((this->m_ExpectedType == TypeSpecifier::SPEC_CHAR ||
                    this->m_ExpectedType == TypeSpecifier::SPEC_UCHAR) &&
                   scalarAst.m_IsLiteral &&
                   this->m_LastSymbolInfo.indirectionLevel > 0 &&
                   !this->m_LastSymbolInfo.isUnique &&
                   (this->m_LastSymbolInfo.isConstVal ||
                    this->m_LastSymbolInfo.isReadOnly)) {
        } else if ((this->m_ExpectedType == TypeSpecifier::SPEC_U8 ||
                    this->m_ExpectedType == TypeSpecifier::SPEC_U16 ||
                    this->m_ExpectedType == TypeSpecifier::SPEC_U32 ||
                    this->m_ExpectedType == TypeSpecifier::SPEC_U64 ||
                    this->m_ExpectedType == TypeSpecifier::SPEC_U128 ||
                    this->m_ExpectedType == TypeSpecifier::SPEC_UINT ||
                    this->m_ExpectedType == TypeSpecifier::SPEC_USIZE) &&
                   scalarAst.m_IsIntegral &&
                   this->m_LastSymbolInfo.indirectionLevel == 0) {
          if (scalarAst.m_IsFloat) {
            this->m_TypeWarningMessages.emplace_back(
                "A 'float' type is casting to '" +
                    GetTypeStr(this->m_ExpectedType) +
                    "'. Potential data loss might be occured!",
                symbolInfo);
          }
        } else if ((this->m_ExpectedType == TypeSpecifier::SPEC_I8 ||
                    this->m_ExpectedType == TypeSpecifier::SPEC_I16 ||
                    this->m_ExpectedType == TypeSpecifier::SPEC_I32 ||
                    this->m_ExpectedType == TypeSpecifier::SPEC_I64 ||
                    this->m_ExpectedType == TypeSpecifier::SPEC_INT ||
                    this->m_ExpectedType == TypeSpecifier::SPEC_ISIZE) &&
                   scalarAst.m_IsIntegral &&
                   (this->m_LastSymbolInfo.indirectionLevel == 0 ||
                    this->m_LastSymbolInfo.isConstRef &&
                        this->m_LastSymbolInfo.isRef)) {
          if (scalarAst.m_IsFloat) {
            this->m_TypeWarningMessages.emplace_back(
                "A 'float' type is casting to '" +
                    GetTypeStr(this->m_ExpectedType) +
                    "'. Potential data loss might be occured!",
                symbolInfo);
          }
        } else {
          symbolInfo.begin = scalarAst.m_SemLoc.begin;
          symbolInfo.end = scalarAst.m_SemLoc.end;
          symbolInfo.line = scalarAst.m_SemLoc.line;

          if (this->m_LastBinOp) {
            // it's okay
          } else {
            this->accumulateIncompatiblePtrErrMesg(symbolInfo);
          }
        }
      } else {
        if (!m_LastCondExpr && !m_LastLoop) {
          this->accumulateIncompatiblePtrErrMesg(symbolInfo);
        }
      }

      // checking mutability
      if (m_LastSymbolInfo.isConstRef) {
        // okay
      } else if (m_LastSymbolInfo.isConstPtr) {
        // wrong
        //        this->m_TypeErrorMessages.emplace_back(
        //            "The value is not suitable with reference type",
        //            symbolInfo);
      } else if (m_LastSymbolInfo.isConstVal) {
        // okay
      } else if (m_LastSymbolInfo.isReadOnly) {
        // partially okay ( actually not really qualifying to the address of
        // the value. )
      }
    }
  } else {
    symbolInfo.value = scalarAst.m_Value;
  }

  return symbolInfo;
}
