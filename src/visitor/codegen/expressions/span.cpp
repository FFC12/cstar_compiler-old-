#include <visitor/codegen/codegen_private.hpp>
#include <ast/span_ast.hpp>

ValuePtr Visitor::visit(SpanAST &spanAst) {
  if (spanAst.m_Base == nullptr ||
      spanAst.m_Base->m_ExprKind != ExprKind::SymbolExpr) {
    assert(false && "`span` base must be a symbol after semantic checking.");
  }

  auto *symbol = static_cast<SymbolAST *>(spanAst.m_Base.get());
  auto symbolInfo = getSymbolInfo(symbol->m_SymbolName);
  auto *storage = FindStorage(m_LocalVarsOnScope, m_GlobalVars,
                              symbol->m_SymbolName);
  if (storage == nullptr) {
    assert(false && "`span` source storage was not found.");
  }

  auto *arrayType = GetPointeeType(storage);
  if (!arrayType->isArrayTy()) {
    auto arrayParamType = m_ArrayParamValueTypes.find(symbol->m_SymbolName);
    if (arrayParamType != m_ArrayParamValueTypes.end()) {
      auto *slotType = GetPointeeType(storage);
      storage = CreateLoad(storage, slotType, symbol->m_SymbolName + ".array");
      arrayType = arrayParamType->second;
    }
  }

  if (!arrayType->isArrayTy()) {
    assert(false && "`span` source must be a fixed array.");
  }

  auto *i64 = Builder->getInt64Ty();
  llvm::Value *begin = llvm::ConstantInt::get(i64, 0);
  llvm::Value *end = llvm::ConstantInt::get(i64, arrayType->getArrayNumElements());

  if (spanAst.m_HasRange) {
    auto *previousType = m_LastType;
    auto previousSigned = m_LastSigned;
    m_LastType = i64;
    m_LastSigned = true;
    begin = spanAst.m_Start->accept(*this);
    begin = CastValueToType(begin, i64, true);
    end = spanAst.m_End->accept(*this);
    end = CastValueToType(end, i64, true);
    m_LastType = previousType;
    m_LastSigned = previousSigned;

    auto *zero = llvm::ConstantInt::get(i64, 0);
    auto *arrayLength =
        llvm::ConstantInt::get(i64, arrayType->getArrayNumElements());
    auto *beginNonNegative = Builder->CreateICmpSGE(begin, zero,
                                                    "span.begin.nonneg");
    auto *endAfterBegin = Builder->CreateICmpSGE(end, begin,
                                                 "span.end.after.begin");
    auto *endInBounds = Builder->CreateICmpSLE(end, arrayLength,
                                               "span.end.in.bounds");
    auto *rangeOk = Builder->CreateAnd(beginNonNegative, endAfterBegin,
                                       "span.range.lower.ok");
    rangeOk = Builder->CreateAnd(rangeOk, endInBounds, "span.range.ok");
    EmitRuntimeCheck(rangeOk, symbol->m_SymbolName + ".span.range");
  }

  std::vector<llvm::Value *> idxList = {llvm::ConstantInt::get(i64, 0), begin};
  auto *data = Builder->CreateInBoundsGEP(arrayType, storage, idxList,
                                          symbol->m_SymbolName + ".span.data");
  auto *length = Builder->CreateSub(end, begin, "span.length");
  auto *span = CreateSpanValue(data, length);
  m_LastType = GetSpanTy();
  m_LastSigned = false;
  return span;
}
