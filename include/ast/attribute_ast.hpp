#ifndef ATTRIBUTE_AST_HPP
#define ATTRIBUTE_AST_HPP

#include <ast/ast.hpp>
#include <string>
#include <utility>

enum class AttributeTargetKind {
  Struct,
};

class AttributeAST : public IAST {
  friend Visitor;
  std::string m_Name;
  AttributeTargetKind m_TargetKind;

 public:
  AttributeAST(std::string name, AttributeTargetKind targetKind,
               SemanticLoc semLoc)
      : IAST(semLoc),
        m_Name(std::move(name)),
        m_TargetKind(targetKind) {
    this->m_ASTKind = ASTKind::Decl;
    this->m_DeclKind = DeclKind::AttribDecl;
  }

  [[nodiscard]] const std::string& name() const { return m_Name; }
  [[nodiscard]] AttributeTargetKind targetKind() const {
    return m_TargetKind;
  }

  void debugNode() override {
    std::cout << "attribute " << m_Name << " for struct { ... }";
  }

  SymbolInfo acceptBefore(Visitor& visitor) override {
    return visitor.preVisit(*this);
  }

  ValuePtr accept(Visitor& visitor) override { return visitor.visit(*this); }
};

#endif
