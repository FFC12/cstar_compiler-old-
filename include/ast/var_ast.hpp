#ifndef VAR_AST_HPP
#define VAR_AST_HPP
#include <ast/ast.hpp>
#include <memory>
#include <vector>
#include <parser/type_specifiers.hpp>
#include <parser/visibility_specifiers.hpp>
#include <string>

class VarAST : public IAST {
  friend Visitor;
  TypeSpecifier m_TypeSpec;
  VisibilitySpecifier m_VisibilitySpec;
  std::string m_Name;
  std::vector<ASTNode> m_ArrDim;
  size_t m_IndirectLevel;
  ASTNode m_RHS;
  bool m_IsLocal;
  bool m_IsUniquePtr;
  bool m_IsInitializerList;

 public:
  VarAST(std::string name, std::unique_ptr<IAST> RHS, TypeSpecifier type_spec,
         VisibilitySpecifier visibility_spec, size_t indirectLevel, bool isUnique, bool isLocal,
         bool isInitializerList, std::vector<ASTNode> arrayDim,
         SemanticLoc& semanticLoc)
      : IAST(semanticLoc),
        m_Name(std::move(name)),
        m_RHS(std::move(RHS)),
        m_IndirectLevel(indirectLevel),
        m_TypeSpec(type_spec),
        m_VisibilitySpec(visibility_spec),
        m_IsUniquePtr(isUnique),
        m_IsLocal(isLocal),
        m_IsInitializerList(isInitializerList),
        m_ArrDim(std::move(arrayDim)) {
    this->m_ASTKind = ASTKind::Decl;
    setDeclKind(DeclKind::VarDecl);
    //'    this->m_DeclKind = DeclKind:
  }

  void debugNode() override {
    std::cout << "type as " << m_TypeSpec;
    for(int i = 0; i < m_IndirectLevel; i++) {
      if(m_IsUniquePtr)
        std::cout << "^";
      else
        std::cout << "*";
    }

    if(!m_ArrDim.empty()) {
      std::cout << "[";
      for(int i = 0; i < m_ArrDim.size(); i++) {
        m_ArrDim[i]->debugNode();
        if(i != m_ArrDim.size() - 1){
          std::cout << ":";
        }
      }
      std::cout << "]";
    }

    std::cout << " " << m_Name;
    if(m_RHS) {
      std::cout << " = ";
      m_RHS->debugNode();
    }
  }

  void setDeclKind(DeclKind kind) { this->m_DeclKind = kind; }

  void setVisibility(VisibilitySpecifier& vis) { this->m_VisibilitySpec = vis; }
};

#endif