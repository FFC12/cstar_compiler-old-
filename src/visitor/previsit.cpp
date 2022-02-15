#include <visitor/visitor.hpp>

void Visitor::previsit(VarAST &varAst) {
  if(varAst.m_IsLocal){

  } else {

  }
}

void Visitor::previsit(AssignmentAST &assignmentAst) {}
void Visitor::previsit(SymbolAST &symbolAst) {}
void Visitor::previsit(BinaryOpAST &binaryOpAst) {}
void Visitor::previsit(CastOpAST &castOpAst) {}
void Visitor::previsit(FuncAST &funcAst) {}
void Visitor::previsit(FuncCallAST &funcCallAst) {}
void Visitor::previsit(IfStmtAST &ifStmtAst) {}
void Visitor::previsit(LoopStmtAST &loopStmtAst) {}
void Visitor::previsit(ParamAST &paramAst) {}
void Visitor::previsit(RetAST &retAst) {}
void Visitor::previsit(UnaryOpAST &unaryOpAst) {}
void Visitor::previsit(TypeAST &typeAst) {}
void Visitor::previsit(ScalarAST &scalarAst) {}