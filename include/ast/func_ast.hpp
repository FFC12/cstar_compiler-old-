#ifndef FUNC_AST_HPP
#define FUNC_AST_HPP
#include <ast/ast.hpp>
#include <vector>

class FuncAST : public IAST {
  std::string m_FuncName;
  ASTNode m_RetType;
  std::vector<ASTNode> m_Params;
  std::vector<ASTNode> m_Scope;
  bool m_IsForwardDecl;

 public:
  FuncAST(std::string funcName, ASTNode retType, std::vector<ASTNode>&& params,
          std::vector<ASTNode>&& scope, bool isForwardDecl, bool isExported,
          SemanticLoc semLoc)
      : IAST(semLoc),
        m_FuncName(std::move(funcName)),
        m_RetType(std::move(retType)),
        m_Params(std::move(params)),
        m_Scope(std::move(scope)),
        m_IsForwardDecl(isForwardDecl) {
    this->m_ASTKind = ASTKind::Decl;
    this->m_DeclKind = isForwardDecl ? DeclKind::ImportFuncDecl
                                     : (isExported ? DeclKind::ExportFuncDecl
                                                   : DeclKind::FuncDecl);
  }

  void debugNode() override {}
};

#endif