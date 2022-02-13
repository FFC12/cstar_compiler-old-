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
  DeclaratorExpr,
  ScalarExpr,
  TypeExpr,
  AssignmentExpr,
  ParamExpr,
  RetExpr,
  FuncCallExpr,
  SymbolExpr,
};

enum DeclKind {
  VarDecl,
  // not sure that we really need those?
  GlobVarDecl,
  ExportVarDecl,
  ImportVarDecl,

  FuncDecl,
  ImportFuncDecl,
  ExportFuncDecl,
  ProtoDecl,
  AttribDecl,
  MacroDecl,
  DirectiveDecl,
  PackageDecl,
  PackageInvDecl,
};

enum StmtKind {
  IfStmt,
  LoopStmt,
};

struct SemanticLoc {
  size_t begin;
  size_t end;
  size_t line;

  SemanticLoc(size_t begin, size_t end, size_t line)
      : begin(begin), end(end), line(line) {}
};

class IAST {
 protected:
  ASTKind m_ASTKind;
  ExprKind m_ExprKind;
  StmtKind m_StmtKind;
  DeclKind m_DeclKind;
  SemanticLoc m_SemLoc;

 public:
  explicit IAST(SemanticLoc semLoc) : m_SemLoc(semLoc){};

  virtual void debugNode() { std::cout << "Root\n"; };

  ExprKind getExprKind() const { return this->m_ExprKind; }
  // we'll change the return-type by LLVM types
  // or our self codegenerator type
};

#include <memory>
using ASTNode = std::unique_ptr<IAST>;

#endif
