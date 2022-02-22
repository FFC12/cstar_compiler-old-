#ifndef VAR_AST_HPP
#define VAR_AST_HPP
#include <ast/ast.hpp>
#include <memory>
#include <parser/type_specifiers.hpp>
#include <parser/visibility_specifiers.hpp>
#include <string>
#include <vector>

class VarAST : public IAST {
  friend Visitor;
  TypeSpecifier m_TypeSpec;
  ASTNode m_Typename;
  TypeQualifier m_TypeQualifier;
  VisibilitySpecifier m_VisibilitySpec;
  std::string m_Name;
  std::vector<ASTNode> m_ArrDim;
  size_t m_IndirectLevel;
  ASTNode m_RHS;
  bool m_IsLocal;
  bool m_IsUniquePtr;
  bool m_IsRef;
  bool m_IsInitializerList;

 public:
  VarAST(std::string name, ASTNode typeName, std::unique_ptr<IAST> RHS, TypeSpecifier type_spec,
         TypeQualifier typeQualifier, VisibilitySpecifier visibility_spec,
         size_t indirectLevel, bool isRef, bool isUnique, bool isLocal,
         bool isInitializerList, std::vector<ASTNode> arrayDim,
         SemanticLoc& semanticLoc)
      : IAST(semanticLoc),
        m_Name(std::move(name)),
        m_Typename(std::move(typeName)),
        m_RHS(std::move(RHS)),
        m_IndirectLevel(indirectLevel),
        m_TypeSpec(type_spec),
        m_TypeQualifier(typeQualifier),
        m_VisibilitySpec(visibility_spec),
        m_IsRef(isRef),
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
    for (int i = 0; i < m_IndirectLevel; i++) {
      if (m_IsUniquePtr)
        std::cout << "^";
      else
        std::cout << "*";
    }

    if (!m_ArrDim.empty()) {
      std::cout << "[";
      for (int i = 0; i < m_ArrDim.size(); i++) {
        m_ArrDim[i]->debugNode();
        if (i != m_ArrDim.size() - 1) {
          std::cout << ":";
        }
      }
      std::cout << "]";
    }

    std::cout << " " << m_Name;
    if (m_RHS) {
      std::cout << " = ";
      m_RHS->debugNode();
    }
  }

  void setDeclKind(DeclKind kind) { this->m_DeclKind = kind; }

  void setVisibility(VisibilitySpecifier& vis) { this->m_VisibilitySpec = vis; }

  SymbolInfo acceptBefore(Visitor& visitor) override {
    return visitor.preVisit(*this);
  }

  ValuePtr accept(Visitor& visitor) override { return visitor.visit(*this); }
};

#endif