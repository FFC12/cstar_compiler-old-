#ifndef CSTAR_SEMANTIC_PRIVATE_HPP
#define CSTAR_SEMANTIC_PRIVATE_HPP

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
#include <visitor/visitor.hpp>

#include <cstdint>
#include <string>
#include <vector>

using DiagnosticCode = cstar::diagnostics::DiagnosticCode;

std::string SymbolStateKey(const SymbolInfo& symbolInfo);
bool SplitStructMethodName(const std::string& name, std::string& owner,
                           std::string& method);
std::string SemValueOperatorMethodName(BinOpKind kind);
bool IsFixableScalarType(TypeSpecifier type);
std::vector<TypeQualifier> BuildQualifierLevels(
    TypeQualifier baseQualifier, size_t indirectionLevel, bool isRef);
size_t GetBitSize(TypeSpecifier typeSpecifier);
inline constexpr uint64_t kMaxLocalStackArrayBytes = 1024 * 1024;
bool TryGetNonNegativeIntegerLiteral(IAST* node, uint64_t& value);
uint64_t ProductOfDimensions(const std::vector<size_t>& dimensions);
uint64_t PrimitiveTypeStorageBytes(TypeSpecifier typeSpecifier);
std::string GetTypeStr(TypeSpecifier typeSpecifier);
bool IsPrimitiveType(TypeSpecifier typeSpecifier);
bool IsIntegerType(TypeSpecifier typeSpecifier);
bool IsFloatingType(TypeSpecifier typeSpecifier);
bool IsNumericPrimitiveType(TypeSpecifier typeSpecifier);
bool IsVoidValue(const SymbolInfo& symbolInfo);
bool IsPointerLike(const SymbolInfo& symbolInfo);
bool IsConstantArrayIndexOutOfBounds(int64_t index, size_t dimension);
bool SameQualifierShape(const SymbolInfo& lhs, const SymbolInfo& rhs);
bool SameTypeShape(const SymbolInfo& lhs, const SymbolInfo& rhs);
SymbolInfo InferScalarLiteralType(ScalarOrLiteralAST* scalar);
bool ContainsTernarySelectSideEffect(IAST* node);
const EnumMemberInfo* SemFindEnumMember(const std::string& enumName,
                                        const std::string& memberName);
bool IsEnumBitwiseOperator(BinOpKind kind);
bool IsEnumEqualityOperator(BinOpKind kind);
bool IsLogicalOperator(BinOpKind kind);
bool IsComparisonOperator(BinOpKind kind);
bool IsOrderedComparisonOperator(BinOpKind kind);
bool IsValueOperator(BinOpKind kind);
SymbolInfo MakeBoolInfo(const SemanticLoc& loc);
bool LosslessCasting(TypeSpecifier target, TypeSpecifier source);

#endif
