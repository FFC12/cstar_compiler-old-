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

#include <limits>

llvm::BasicBlock *Visitor::MainFuncBB = nullptr;
llvm::GlobalVariable *Visitor::LastGlobVarRef = nullptr;

