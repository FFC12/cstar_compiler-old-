#ifndef FUNC_AST_HPP
#define FUNC_AST_HPP
#include <ast/ast.hpp>
#include <vector>
#include "parser/type_qualifier.hpp"

class FuncAST : public IAST {
  friend Visitor;
  std::string m_FuncName;
  ASTNode m_RetType;
  std::vector<ASTNode> m_Params;
  std::vector<ASTNode> m_Scope;
  TypeQualifier m_RetTypeQualifier;
  bool m_IsForwardDecl;
  bool m_IsExported;
  bool m_IsStatic;
  bool m_IsVariadic;
  bool m_HasExplicitReturnType;
  bool m_CanThrow;
  std::string m_ErrorTypeName;
  AccessSpecifier m_Access;

 public:
  FuncAST(std::string funcName, ASTNode retType, std::vector<ASTNode>&& params,
          std::vector<ASTNode>&& scope, TypeQualifier retTypeQualifier,
          bool isForwardDecl, bool isExported, bool isStatic,
          bool isVariadic, bool hasExplicitReturnType, AccessSpecifier access,
          SemanticLoc semLoc, bool canThrow = false,
          std::string errorTypeName = {})
      : IAST(semLoc),
        m_FuncName(std::move(funcName)),
        m_RetType(std::move(retType)),
        m_Params(std::move(params)),
        m_Scope(std::move(scope)),
        m_RetTypeQualifier(retTypeQualifier),
        m_IsExported(isExported),
        m_IsStatic(isStatic),
        m_IsVariadic(isVariadic),
        m_HasExplicitReturnType(hasExplicitReturnType),
        m_CanThrow(canThrow),
        m_ErrorTypeName(std::move(errorTypeName)),
        m_Access(access),
        m_IsForwardDecl(isForwardDecl) {
    this->m_ASTKind = ASTKind::Decl;
    this->m_DeclKind = isExported ? DeclKind::ExportFuncDecl
                                  : (isForwardDecl ? DeclKind::ImportFuncDecl
                                                   : DeclKind::FuncDecl);
    this->m_AccessSpecifier = access;
    this->m_IsStaticDecl = isStatic;
  }

  void debugNode() override {
    if (m_Access == ACCESS_PUBLIC) std::cout << "public ";
    if (m_IsStatic) std::cout << "static ";
    if (m_IsForwardDecl) std::cout << "import ";
    if (m_IsExported) std::cout << "export ";

    std::cout << m_FuncName << "(";
    if (!this->m_Params.empty()) {
      for (auto& p : m_Params) {
        p->debugNode();
        std::cout << ",";
      }
    }
    if (m_IsVariadic) std::cout << "...";
    std::cout << ")";
    if (m_RetType) {
      std::cout << " :: ";
      m_RetType->debugNode();
    }

    if (m_IsForwardDecl) {
      std::cout << ";";
    } else {
      std::cout << "{\n";
      for (auto& d : m_Scope) {
        d->debugNode();
        if (d->getASTKind() != ASTKind::Stmt) std::cout << ";\n";
      }
      std::cout << "}\n";
    }
  }

  SymbolInfo acceptBefore(Visitor& visitor) override {
    return visitor.preVisit(*this);
  }

  bool isVariadic() const { return m_IsVariadic; }
  bool canThrow() const { return m_CanThrow; }
  const std::string& errorTypeName() const { return m_ErrorTypeName; }

  ValuePtr accept(Visitor& visitor) override { return visitor.visit(*this); }
};

#endif
