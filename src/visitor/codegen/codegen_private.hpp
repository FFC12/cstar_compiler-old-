#ifndef CSTAR_CODEGEN_PRIVATE_HPP
#define CSTAR_CODEGEN_PRIVATE_HPP

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>

#include <ast/assignment_ast.hpp>
#include <ast/ast.hpp>
#include <ast/attribute_ast.hpp>
#include <ast/binary_op_ast.hpp>
#include <ast/cast_op_ast.hpp>
#include <ast/control_flow_ast.hpp>
#include <ast/enum_ast.hpp>
#include <ast/fix_ast.hpp>
#include <ast/func_ast.hpp>
#include <ast/func_call_ast.hpp>
#include <ast/if_stmt.hpp>
#include <ast/loop_stmt.hpp>
#include <ast/macro_ast.hpp>
#include <ast/new_ast.hpp>
#include <ast/option_stmt.hpp>
#include <ast/param_ast.hpp>
#include <ast/ret_ast.hpp>
#include <ast/scalar_ast.hpp>
#include <ast/struct_ast.hpp>
#include <ast/symbol_ast.hpp>
#include <ast/trait_ast.hpp>
#include <ast/type_ast.hpp>
#include <ast/unary_op_ast.hpp>
#include <ast/var_ast.hpp>
#include <codegen/native_runtime.hpp>
#include <visitor/visitor.hpp>

#include <cstdint>
#include <map>
#include <string>
#include <vector>

struct AssignmentTarget {
  llvm::Value* address = nullptr;
  llvm::Type* valueType = nullptr;
};

llvm::PointerType* GetI8PtrTy();
llvm::Type* GetType(TypeSpecifier typeSpecifier, size_t indirectLevel,
                    bool isRef = false);
llvm::StructType* GetSharedPointerTy();
std::string DefinedTypeNameFromTypeAst(TypeAST* typeAst);
std::string DefinedTypeNameFromVarAst(VarAST& varAst);
std::string ValueOperatorMethodName(BinOpKind kind);
llvm::Type* GetStorageType(TypeSpecifier typeSpecifier, size_t indirectLevel,
                           bool isUnique, bool isRef,
                           const std::string& definedTypeName);
llvm::Type* GetStorageType(TypeSpecifier typeSpecifier, size_t indirectLevel,
                           bool isUnique, bool isRef);
llvm::Type* GetStorageType(TypeSpecifier typeSpecifier, size_t indirectLevel,
                           bool isUnique, bool isRef,
                           const SymbolInfo& symbolInfo);
llvm::Type* GetStructFieldLLVMType(const StructFieldInfo& field);
llvm::Type* GetSymbolLLVMType(const SymbolInfo& symbolInfo);
llvm::StructType* GetDefinedStructTy(const std::string& name);
TypeSpecifier GetDefinedStorageType(const std::string& definedTypeName);
TypeSpecifier GetEffectiveStorageType(TypeSpecifier typeSpecifier,
                                      const std::string& definedTypeName);
const EnumMemberInfo* FindEnumMember(const std::string& enumName,
                                     const std::string& memberName);
bool IsSharedPointerTy(llvm::Type* type);
bool IsSharedPointerSymbol(const SymbolInfo& symbolInfo);
llvm::ArrayType* GetArrayType(llvm::Type* elementType,
                              const std::vector<ASTNode>& dimensions);

bool TryParseUnsignedIntegerLiteral(const std::string& literal,
                                    uint64_t& value);
bool TryParseSignedIntegerLiteral(const std::string& literal, int64_t& value);
llvm::Constant* CreateLiteralConstant(llvm::Type* type,
                                      const std::string& literal);
size_t FlatArrayLength(const std::vector<size_t>& dimensions);
llvm::Value* CastArrayIndex(llvm::Value* index);
llvm::Value* NormalizeArrayIndex(llvm::Value* index, size_t dimension);
llvm::Value* CreateLinearArrayIndex(const std::vector<llvm::Value*>& indexes,
                                    const std::vector<size_t>& dimensions);
llvm::Type* GetPointeeType(llvm::Value* value);
bool IsSigned(TypeSpecifier typeSpecifier);
llvm::Value* CreateOneValue(llvm::Type* type);
std::string DecodeCStringEscapes(const std::string& value);
llvm::Constant* CreateGlobalStringPointer(llvm::StringRef value,
                                          const llvm::Twine& name = ".str");
llvm::Value* CastValueToType(llvm::Value* value, llvm::Type* targetType,
                             bool isSigned);
llvm::Value* UnsafeCastValueToType(llvm::Value* value, llvm::Type* targetType,
                                   bool isSigned);
llvm::Value* CastValueToBranchCondition(llvm::Value* value, bool isSigned);

AssignmentTarget ResolveAssignmentTarget(llvm::Value* storage,
                                         const SymbolInfo& symbolInfo,
                                         size_t derefLevel);
llvm::Value* CreateShortcutAssignmentValue(llvm::Value* currentValue,
                                           llvm::Value* rhs,
                                           llvm::Type* targetType,
                                           ShortcutOp shortcutOp,
                                           bool isSigned);
llvm::GlobalVariable* CreateConstantGlobalVar(
    const std::string& name, VisibilitySpecifier specifier, llvm::Type* type,
    llvm::Value* value);
llvm::GlobalVariable* CreateZeroInitConstantGlobalVar(
    const std::string& name, VisibilitySpecifier specifier, llvm::Type* type);
llvm::GlobalVariable* CreateInitConstantGlobalVar(
    const std::string& name, VisibilitySpecifier specifier, llvm::Type* type,
    llvm::Constant* value);
llvm::Function* CreateGlobalVarInitFunc();
llvm::Function* CreateGlobalFuncSubToMain(
    std::vector<llvm::StringRef>& initFuncs);
llvm::Value* CreateAlloca(const std::string& name, llvm::Type* type);
llvm::Value* CreateLocalVariable(const std::string& name, llvm::Type* type,
                                 llvm::Value* value);
llvm::FunctionCallee GetMallocFunction();
llvm::FunctionCallee GetFreeFunction();
llvm::Value* CreateDefaultHeapAlloc(llvm::Type* type,
                                    const llvm::Twine& name);
void CreateDefaultHeapFree(llvm::Value* ptr);
llvm::Value* CreateLoad(llvm::Value* address, llvm::Type* type,
                        const llvm::Twine& name = "");
llvm::Value* CreateAtomicLoad(llvm::Value* address, llvm::Type* type,
                              const llvm::Twine& name = "");
llvm::Value* CreateSharedPointerHandle(llvm::Value* data,
                                       llvm::Value* refCount);
llvm::Value* CreateNullSharedPointerHandle();
llvm::Value* ExtractSharedPointerData(llvm::Value* handle);
llvm::Value* ExtractSharedPointerCount(llvm::Value* handle);
llvm::Value* CreateSharedPointerCounter(const std::string& name,
                                        uint64_t initialValue);
void AtomicBumpSharedPointer(llvm::Value* handle, int64_t delta);
void RetainSharedPointer(llvm::Value* handle);
void ReleaseSharedPointer(llvm::Value* handle);
llvm::Value* LoadSharedPointerStrongCount(llvm::Value* handle);
llvm::Value* FindStorage(std::map<std::string, llvm::AllocaInst*>& locals,
                         std::map<std::string, llvm::GlobalVariable*>& globals,
                         const std::string& name);

llvm::Value* CreateIntegerOrPointerCompare(
    llvm::CmpInst::Predicate signedPredicate,
    llvm::CmpInst::Predicate unsignedPredicate, llvm::Value* lhs,
    llvm::Value* rhs, bool isSigned, const llvm::Twine& name);
llvm::Value* CreateOrderedCompare(llvm::CmpInst::Predicate floatPredicate,
                                  llvm::CmpInst::Predicate signedPredicate,
                                  llvm::CmpInst::Predicate unsignedPredicate,
                                  llvm::Value* lhs, llvm::Value* rhs,
                                  llvm::Type* valueType, bool isSigned,
                                  const llvm::Twine& name);
llvm::Value* CreateEqualityCompare(bool equals, llvm::Value* lhs,
                                   llvm::Value* rhs, llvm::Type* valueType,
                                   const llvm::Twine& name);
bool IsFloatingPointType(llvm::Type* type);
llvm::Value* CreateArithmeticValue(BinOpKind op, llvm::Value* lhs,
                                   llvm::Value* rhs, llvm::Type* valueType,
                                   bool isSigned);
llvm::Value* CreateLogicalValue(BinOpKind op, llvm::Value* lhs,
                                llvm::Value* rhs, bool isSigned);
void CreateBranchIfNeeded(llvm::BasicBlock* target);
void EmitScope(Visitor& visitor, std::vector<ASTNode>& scope);
bool IsAnyBinOpOrSymbolInvolved(std::vector<BinOpOrVal>& v);
llvm::Value* CreateInitializerValue(Visitor& visitor,
                                    const BinOpOrVal& initializer,
                                    llvm::Type* elementType);
llvm::Value* CreateArrayElementAddress(llvm::Type* arrayType,
                                       llvm::Value* storage, size_t index);

#endif
