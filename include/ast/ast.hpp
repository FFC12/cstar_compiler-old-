#ifndef AST_HPP
#define AST_HPP
#include <iostream>

enum ASTKind {
  Expr,
  Stmt,
  Decl,
};

enum ExprKind {
  BinOp,
  UnaryOp,
  CastOp,
  DeclaratorExpr,
  ScalarExpr,
  TypeExpr,
  IndirectExpr,
  RefExpr,
  AssignmentExpr
};

enum DeclKind {
  VarDecl,
  // not sure that we really need those?
  GlobVarDecl,
  ExportVarDecl,
  ImportVarDecl,

  FuncDecl,
  ProtoDecl,
  AttribDecl,
  MacroDecl,
  DirectiveDecl,
  PackageDecl,
  PackageInvDecl,
};

enum StmtKind {

};

class IAST {
 protected:
  ASTKind m_ASTKind;
  ExprKind m_ExprKind;
  StmtKind m_StmtKind;
  DeclKind m_DeclKind;

 public:
  virtual void debugNode() {
      std::cout << "Root\n";
  };
  // we'll change the return-type by LLVM types
  // or our self codegenerator type
};

#include <memory>
using ASTNode = std::unique_ptr<IAST>;

#endif
