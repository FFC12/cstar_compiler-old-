#include <visitor/visitor.hpp>
#include <ast/assignment_ast.hpp>
#include <ast/ast.hpp>
#include <ast/binary_op_ast.hpp>
#include <ast/cast_op_ast.hpp>
#include <ast/func_ast.hpp>
#include <ast/func_call_ast.hpp>
#include <ast/if_stmt.hpp>
#include <ast/loop_stmt.hpp>
#include <ast/param_ast.hpp>
#include <ast/ret_ast.hpp>
#include <ast/scalar_ast.hpp>
#include <ast/symbol_ast.hpp>
#include <ast/type_ast.hpp>
#include <ast/unary_op_ast.hpp>
#include <ast/var_ast.hpp>

ValuePtr Visitor::visit(VarAST &varAst) {}
ValuePtr Visitor::visit(AssignmentAST &assignmentAst) {}
ValuePtr Visitor::visit(BinaryOpAST &binaryOpAst) {}
ValuePtr Visitor::visit(CastOpAST &castOpAst) {}
ValuePtr Visitor::visit(FuncAST &funcAst) {}
ValuePtr Visitor::visit(FuncCallAST &funcCallAst) {}
ValuePtr Visitor::visit(IfStmtAST &ifStmtAst) {}
ValuePtr Visitor::visit(LoopStmtAST &loopStmtAst) {}
ValuePtr Visitor::visit(ParamAST &paramAst) {}
ValuePtr Visitor::visit(RetAST &retAst) {}
ValuePtr Visitor::visit(UnaryOpAST &unaryOpAst) {}
ValuePtr Visitor::visit(TypeAST &typeAst) {}
ValuePtr Visitor::visit(ScalarOrLiteralAST &scalarAst) {}
ValuePtr Visitor::visit(SymbolAST &symbolAst) {}
ValuePtr Visitor::visit(FixAST& fixAst){}
