#ifndef NATIVE_RUNTIME_HPP
#define NATIVE_RUNTIME_HPP

#include <llvm/ADT/StringRef.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Value.h>

namespace cstar::codegen {

class NativeRuntime {
 public:
  NativeRuntime(llvm::Module& module, llvm::IRBuilder<>& builder);

  llvm::Value* emitPrintValue(llvm::Value* value, bool isSigned);
  llvm::Value* emitInputInt();
  llvm::Value* emitInputString();
  llvm::Value* emitClearScreen();
  llvm::Value* emitSleepMs(llvm::Value* milliseconds, bool isSigned);
  llvm::Value* emitEnableRawInput();
  llvm::Value* emitDisableRawInput();
  llvm::Value* emitReadKey();

 private:
  llvm::Module& m_Module;
  llvm::IRBuilder<>& m_Builder;

  llvm::PointerType* i8PtrTy() const;
  llvm::FunctionCallee printf();
  llvm::FunctionCallee scanf();
  llvm::FunctionCallee atoll();
  llvm::FunctionCallee usleep();
  llvm::FunctionCallee system();
  llvm::FunctionCallee getchar();
  llvm::Constant* globalStringPtr(llvm::StringRef value,
                                  const llvm::Twine& name = "");
  llvm::Value* inputTokenBuffer(llvm::StringRef name);
};

}  // namespace cstar::codegen

#endif  // NATIVE_RUNTIME_HPP
