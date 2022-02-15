#ifndef VISITOR_HPP
#define VISITOR_HPP
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

class Visitor {
  friend VarAST;

 public:
  Visitor() = default;

  void visit(VarAST& varAst);
  void visit(AssignmentAST& assignmentAst);
  void visit(BinaryOpAST& binaryOpAst);
  void visit(CastOpAST& castOpAst);
  void visit(FuncAST& funcAst);
  void visit(FuncCallAST& funcCallAst);
  void visit(IfStmtAST& ifStmtAst);
  void visit(LoopStmtAST& loopStmtAst);
  void visit(ParamAST& paramAst);
  void visit(RetAST& retAst);
  void visit(UnaryOpAST& unaryOpAst);
  void visit(TypeAST& typeAst);
  void visit(ScalarAST& scalarAst);
  void visit(SymbolAST& symbolAst);

};

#endif