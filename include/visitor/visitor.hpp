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

  void previsit(VarAST& varAst);
  void previsit(AssignmentAST& assignmentAst);
  void previsit(BinaryOpAST& binaryOpAst);
  void previsit(CastOpAST& castOpAst);
  void previsit(FuncAST& funcAst);
  void previsit(FuncCallAST& funcCallAst);
  void previsit(IfStmtAST& ifStmtAst);
  void previsit(LoopStmtAST& loopStmtAst);
  void previsit(ParamAST& paramAst);
  void previsit(RetAST& retAst);
  void previsit(UnaryOpAST& unaryOpAst);
  void previsit(TypeAST& typeAst);
  void previsit(ScalarAST& scalarAst);
  void previsit(SymbolAST& symbolAst);
};

#endif