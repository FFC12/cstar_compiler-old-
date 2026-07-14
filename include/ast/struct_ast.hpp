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

 public:
  StructAST(std::string name, std::vector<StructFieldInfo> fields,
            AccessSpecifier access, SemanticLoc semLoc)
      : IAST(semLoc),
        m_Name(std::move(name)),
        m_Fields(std::move(fields)) {
    this->m_ASTKind = ASTKind::Decl;
    this->m_DeclKind = DeclKind::StructDecl;
    this->m_AccessSpecifier = access;
  }

  [[nodiscard]] const std::string& name() const { return m_Name; }
  [[nodiscard]] const std::vector<StructFieldInfo>& fields() const {
    return m_Fields;
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
