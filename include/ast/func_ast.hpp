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
  bool m_IsExported;

 public:
  FuncAST(std::string funcName, ASTNode retType, std::vector<ASTNode>&& params,
          std::vector<ASTNode>&& scope, bool isForwardDecl, bool isExported,
          SemanticLoc semLoc)
      : IAST(semLoc),
        m_FuncName(std::move(funcName)),
        m_RetType(std::move(retType)),
        m_Params(std::move(params)),
        m_Scope(std::move(scope)),
        m_IsExported(isExported),
        m_IsForwardDecl(isForwardDecl) {
    this->m_ASTKind = ASTKind::Decl;
    this->m_DeclKind = isForwardDecl ? DeclKind::ImportFuncDecl
                                     : (isExported ? DeclKind::ExportFuncDecl
                                                   : DeclKind::FuncDecl);
  }

  void debugNode() override {
    if(m_IsForwardDecl)
      std::cout << "import ";
    if(m_IsExported)
      std::cout << "export ";

    std::cout << m_FuncName << "(";
    if(!this->m_Params.empty()) {
      for(auto& p: m_Params){
        p->debugNode();
        std::cout << ",";
      }
    }
    std::cout << ")";
    if(m_RetType) {
      std::cout << " :: ";
      m_RetType->debugNode();
    }

    if(m_IsForwardDecl) {
      std::cout << ";";
    } else {
      std::cout << "{\n";
      for(auto& d: m_Scope){
        d->debugNode();
        if(d->getASTKind() != ASTKind::Stmt)
          std::cout << ";\n";
      }
      std::cout << "}\n";
    }
  }
};

#endif