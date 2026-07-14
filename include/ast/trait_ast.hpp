#ifndef TRAIT_AST_HPP
#define TRAIT_AST_HPP

#include <ast/ast.hpp>
#include <string>
#include <utility>
#include <vector>
#include <visitor/symbols.hpp>

class TraitAST : public IAST {
  friend Visitor;
  std::string m_Name;
  std::vector<TraitRequirementInfo> m_Requirements;

 public:
  TraitAST(std::string name, std::vector<TraitRequirementInfo> requirements,
           AccessSpecifier access, SemanticLoc semLoc)
      : IAST(semLoc),
        m_Name(std::move(name)),
        m_Requirements(std::move(requirements)) {
    this->m_ASTKind = ASTKind::Decl;
    this->m_DeclKind = DeclKind::TraitDecl;
    this->m_AccessSpecifier = access;
  }

  [[nodiscard]] const std::string& name() const { return m_Name; }
  [[nodiscard]] const std::vector<TraitRequirementInfo>& requirements() const {
    return m_Requirements;
  }

  void debugNode() override {
    std::cout << "trait " << m_Name << " {";
    for (const auto& requirement : m_Requirements) {
      std::cout << requirement.name << ";";
    }
    std::cout << "}";
  }

  SymbolInfo acceptBefore(Visitor& visitor) override {
    return visitor.preVisit(*this);
  }

  ValuePtr accept(Visitor& visitor) override { return visitor.visit(*this); }
};

#endif
