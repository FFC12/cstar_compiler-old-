#ifndef ENUM_AST_HPP
#define ENUM_AST_HPP

#include <ast/ast.hpp>
#include <parser/type_specifiers.hpp>
#include <string>
#include <utility>
#include <vector>
#include <visitor/symbols.hpp>

class EnumAST : public IAST {
  friend Visitor;
  std::string m_Name;
  TypeSpecifier m_UnderlyingType;
  bool m_IsFlags;
  std::vector<EnumMemberInfo> m_Members;

 public:
  EnumAST(std::string name, TypeSpecifier underlyingType,
          std::vector<EnumMemberInfo> members, AccessSpecifier access,
          SemanticLoc semLoc, bool isFlags = false)
      : IAST(semLoc),
        m_Name(std::move(name)),
        m_UnderlyingType(underlyingType),
        m_IsFlags(isFlags),
        m_Members(std::move(members)) {
    this->m_ASTKind = ASTKind::Decl;
    this->m_DeclKind = DeclKind::EnumDecl;
    this->m_AccessSpecifier = access;
  }

  [[nodiscard]] const std::string& name() const { return m_Name; }
  [[nodiscard]] TypeSpecifier underlyingType() const {
    return m_UnderlyingType;
  }
  [[nodiscard]] bool isFlags() const { return m_IsFlags; }
  [[nodiscard]] const std::vector<EnumMemberInfo>& members() const {
    return m_Members;
  }

  void debugNode() override {
    if (m_IsFlags) {
      std::cout << "flags ";
    }
    std::cout << "enum " << m_Name << " {";
    for (const auto& member : m_Members) {
      std::cout << member.name << ";";
    }
    std::cout << "}";
  }

  SymbolInfo acceptBefore(Visitor& visitor) override {
    return visitor.preVisit(*this);
  }

  ValuePtr accept(Visitor& visitor) override { return visitor.visit(*this); }
};

#endif
