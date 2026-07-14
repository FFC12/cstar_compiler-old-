#ifndef STRUCT_AST_HPP
#define STRUCT_AST_HPP

#include <ast/ast.hpp>
#include <string>
#include <utility>
#include <vector>
#include <visitor/symbols.hpp>

class StructAST : public IAST {
  friend Visitor;
  std::string m_Name;
  std::vector<StructFieldInfo> m_Fields;
  std::vector<std::string> m_Traits;

 public:
  StructAST(std::string name, std::vector<StructFieldInfo> fields,
            std::vector<std::string> traits, AccessSpecifier access,
            SemanticLoc semLoc)
      : IAST(semLoc),
        m_Name(std::move(name)),
        m_Fields(std::move(fields)),
        m_Traits(std::move(traits)) {
    this->m_ASTKind = ASTKind::Decl;
    this->m_DeclKind = DeclKind::StructDecl;
    this->m_AccessSpecifier = access;
  }

  [[nodiscard]] const std::string& name() const { return m_Name; }
  [[nodiscard]] const std::vector<StructFieldInfo>& fields() const {
    return m_Fields;
  }
  [[nodiscard]] const std::vector<std::string>& traits() const {
    return m_Traits;
  }

  void debugNode() override {
    std::cout << "struct " << m_Name << " {";
    for (const auto& field : m_Fields) {
      std::cout << field.name << ";";
    }
    std::cout << "}";
  }

  SymbolInfo acceptBefore(Visitor& visitor) override {
    return visitor.preVisit(*this);
  }

  ValuePtr accept(Visitor& visitor) override { return visitor.visit(*this); }
};

#endif
