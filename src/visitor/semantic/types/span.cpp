#include <visitor/semantic/semantic_private.hpp>
#include <ast/span_ast.hpp>

SymbolInfo Visitor::preVisit(SpanAST &spanAst) {
  SymbolInfo spanInfo;
  spanInfo.begin = spanAst.m_SemLoc.begin;
  spanInfo.end = spanAst.m_SemLoc.end;
  spanInfo.line = spanAst.m_SemLoc.line;
  spanInfo.isSubscriptable = true;
  spanInfo.isRuntimeSizedArray = true;

  if (!m_TypeChecking) {
    return spanInfo;
  }

  if (spanAst.m_Base == nullptr ||
      spanAst.m_Base->m_ExprKind != ExprKind::SymbolExpr) {
    this->m_TypeErrorMessages.emplace_back(
        "`span` expects a fixed array symbol", spanInfo);
    return spanInfo;
  }

  auto *symbol = static_cast<SymbolAST *>(spanAst.m_Base.get());
  SymbolInfo lookup;
  SymbolInfo baseInfo;
  auto symbolName = symbol->m_SymbolName;
  if (!symbolValidation(symbolName, lookup, baseInfo)) {
    return spanInfo;
  }

  if (!baseInfo.isSubscriptable || baseInfo.isRuntimeSizedArray ||
      baseInfo.arrayDimensions.empty()) {
    this->m_TypeErrorMessages.emplace_back(
        "`span` expects a fixed-size array value", baseInfo);
    return spanInfo;
  }

  spanInfo.type = baseInfo.type;
  spanInfo.definedTypeName = baseInfo.definedTypeName;
  spanInfo.indirectionLevel = baseInfo.indirectionLevel;
  spanInfo.isUnique = baseInfo.isUnique;
  spanInfo.isRef = false;
  spanInfo.symbolName = baseInfo.symbolName;

  if (spanAst.m_HasRange) {
    int64_t begin = 0;
    int64_t end = 0;
    const bool beginIsConst =
        spanAst.m_Start != nullptr &&
        tryGetConstantIntegerLiteral(spanAst.m_Start.get(), begin);
    const bool endIsConst =
        spanAst.m_End != nullptr && tryGetConstantIntegerLiteral(spanAst.m_End.get(), end);
    const auto fixedLength = baseInfo.arrayDimensions.front();

    if (beginIsConst && begin < 0) {
      this->m_TypeErrorMessages.emplace_back(
          "`span` range begin cannot be negative", spanInfo);
    }
    if (endIsConst && end < 0) {
      this->m_TypeErrorMessages.emplace_back(
          "`span` range end cannot be negative", spanInfo);
    }
    if (beginIsConst && endIsConst && begin > end) {
      this->m_TypeErrorMessages.emplace_back(
          "`span` range begin must be less than or equal to range end",
          spanInfo);
    }
    if (endIsConst && static_cast<uint64_t>(end) > fixedLength) {
      this->m_TypeErrorMessages.emplace_back(
          "`span` range end is out of bounds for fixed array size " +
              std::to_string(fixedLength),
          spanInfo);
    }
  }

  return spanInfo;
}
