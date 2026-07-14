#include <codegen/native_runtime.hpp>

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/GlobalVariable.h>

namespace cstar::codegen {

NativeRuntime::NativeRuntime(llvm::Module& module, llvm::IRBuilder<>& builder)
    : m_Module(module), m_Builder(builder) {}

llvm::PointerType* NativeRuntime::i8PtrTy() const {
  return llvm::PointerType::get(m_Builder.getContext(), 0);
}

llvm::FunctionCallee NativeRuntime::printf() {
  auto* type = llvm::FunctionType::get(m_Builder.getInt32Ty(), {i8PtrTy()},
                                       true);
  return m_Module.getOrInsertFunction("printf", type);
}

llvm::FunctionCallee NativeRuntime::scanf() {
  auto* type = llvm::FunctionType::get(m_Builder.getInt32Ty(), {i8PtrTy()},
                                       true);
  return m_Module.getOrInsertFunction("scanf", type);
}

llvm::FunctionCallee NativeRuntime::atoll() {
  auto* type = llvm::FunctionType::get(m_Builder.getInt64Ty(), {i8PtrTy()},
                                       false);
  return m_Module.getOrInsertFunction("atoll", type);
}

llvm::FunctionCallee NativeRuntime::usleep() {
  auto* type = llvm::FunctionType::get(m_Builder.getInt32Ty(),
                                       {m_Builder.getInt32Ty()}, false);
  return m_Module.getOrInsertFunction("usleep", type);
}

llvm::FunctionCallee NativeRuntime::fflush() {
  auto* type = llvm::FunctionType::get(m_Builder.getInt32Ty(), {i8PtrTy()},
                                       false);
  return m_Module.getOrInsertFunction("fflush", type);
}

llvm::FunctionCallee NativeRuntime::system() {
  auto* type = llvm::FunctionType::get(m_Builder.getInt32Ty(), {i8PtrTy()},
                                       false);
  return m_Module.getOrInsertFunction("system", type);
}

llvm::FunctionCallee NativeRuntime::getchar() {
  auto* type = llvm::FunctionType::get(m_Builder.getInt32Ty(), {}, false);
  return m_Module.getOrInsertFunction("getchar", type);
}

llvm::Constant* NativeRuntime::globalStringPtr(llvm::StringRef value,
                                               const llvm::Twine& name) {
  auto* globalString = m_Builder.CreateGlobalString(value, name);
  auto* zero = llvm::ConstantInt::get(m_Builder.getInt32Ty(), 0);
  llvm::Constant* indices[] = {zero, zero};
  return llvm::ConstantExpr::getInBoundsGetElementPtr(
      globalString->getValueType(), globalString, indices);
}

llvm::Value* NativeRuntime::inputTokenBuffer(llvm::StringRef name) {
  auto* bufferType = llvm::ArrayType::get(m_Builder.getInt8Ty(), 256);
  auto* buffer = m_Builder.CreateAlloca(bufferType, nullptr, name);
  auto* zero = llvm::ConstantInt::get(m_Builder.getInt64Ty(), 0);
  return m_Builder.CreateInBoundsGEP(bufferType, buffer, {zero, zero},
                                     llvm::Twine(name) + ".ptr");
}

llvm::Value* NativeRuntime::emitPrintValue(llvm::Value* value, bool isSigned) {
  if (value == nullptr) {
    return nullptr;
  }

  const char* format = "%s";
  if (value->getType()->isIntegerTy()) {
    format = "%lld";
    value = m_Builder.CreateIntCast(value, m_Builder.getInt64Ty(), isSigned);
  } else if (value->getType()->isFloatTy()) {
    format = "%f";
    value = m_Builder.CreateFPExt(value, m_Builder.getDoubleTy());
  } else if (value->getType()->isDoubleTy()) {
    format = "%f";
  }

  return m_Builder.CreateCall(printf(), {globalStringPtr(format), value});
}

llvm::Value* NativeRuntime::emitInputInt() {
  auto* bufferPtr = inputTokenBuffer("input.int.token");
  m_Builder.CreateCall(scanf(), {globalStringPtr("%255s"), bufferPtr});
  return m_Builder.CreateCall(atoll(), {bufferPtr}, "input.int");
}

llvm::Value* NativeRuntime::emitInputString() {
  auto* bufferPtr = inputTokenBuffer("input.string");
  m_Builder.CreateCall(scanf(), {globalStringPtr("%255s"), bufferPtr});
  return bufferPtr;
}

llvm::Value* NativeRuntime::emitClearScreen() {
  m_Builder.CreateCall(printf(), {globalStringPtr("\033[2J\033[H")});
  return emitFlushOutput();
}

llvm::Value* NativeRuntime::emitFlushOutput() {
  return m_Builder.CreateCall(
      fflush(), {llvm::ConstantPointerNull::get(i8PtrTy())});
}

llvm::Value* NativeRuntime::emitSleepMs(llvm::Value* milliseconds,
                                        bool isSigned) {
  if (milliseconds->getType()->isIntegerTy()) {
    milliseconds =
        m_Builder.CreateIntCast(milliseconds, m_Builder.getInt32Ty(), isSigned);
  }
  auto* micros = m_Builder.CreateMul(
      milliseconds, llvm::ConstantInt::get(m_Builder.getInt32Ty(), 1000),
      "sleep.us");
  return m_Builder.CreateCall(usleep(), {micros});
}

llvm::Value* NativeRuntime::emitEnableRawInput() {
  return m_Builder.CreateCall(
      system(), {globalStringPtr("stty -icanon -echo min 0 time 0")});
}

llvm::Value* NativeRuntime::emitDisableRawInput() {
  return m_Builder.CreateCall(system(), {globalStringPtr("stty sane")});
}

llvm::Value* NativeRuntime::emitReadKey() {
  return m_Builder.CreateCall(getchar(), {}, "read.key");
}

}  // namespace cstar::codegen
