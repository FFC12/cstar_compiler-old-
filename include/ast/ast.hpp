#ifndef AST_HPP
#define AST_HPP
#include <iostream>
#include <parser/visibility_specifiers.hpp>
#include <visitor/visitor.hpp>

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
  CastExpr,
  SymbolExpr,
  FixExpr,
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
  BreakStmt,
  ContinueStmt,
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
  friend Visitor;
  ASTKind m_ASTKind;
  ExprKind m_ExprKind;
  StmtKind m_StmtKind;
  DeclKind m_DeclKind;
  AccessSpecifier m_AccessSpecifier = ACCESS_PRIVATE;
  bool m_IsStaticDecl = false;
  SemanticLoc m_SemLoc;

 public:
  explicit IAST(SemanticLoc semLoc) : m_SemLoc(semLoc){};
  virtual ~IAST() = default;

  [[nodiscard]] SemanticLoc getSemLoc() const { return m_SemLoc; }

  virtual void debugNode() { std::cout << "Root\n"; };

  virtual SymbolInfo acceptBefore(Visitor& visitor) = 0;

  virtual ValuePtr accept(Visitor& visitor) = 0;

  [[nodiscard]] ExprKind getExprKind() const { return this->m_ExprKind; }
  [[nodiscard]] ASTKind getASTKind() const { return this->m_ASTKind; }
  [[nodiscard]] DeclKind getDeclKind() const { return this->m_DeclKind; }
  [[nodiscard]] AccessSpecifier getAccessSpecifier() const {
    return this->m_AccessSpecifier;
  }
  [[nodiscard]] bool isPublicDecl() const {
    return this->m_AccessSpecifier == ACCESS_PUBLIC;
  }
  [[nodiscard]] bool isStaticDecl() const { return this->m_IsStaticDecl; }
};

#include <memory>
using ASTNode = std::unique_ptr<IAST>;

#endif
