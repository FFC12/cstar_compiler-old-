#ifndef MACRO_AST_HPP
#define MACRO_AST_HPP

#include <ast/ast.hpp>
#include <string>
#include <utility>
#include <vector>

enum class MacroParamKind {
  Expr,
  Stmt,
  Item,
  Type,
  Ident,
  Tokens,
};

enum class MacroReturnKind {
  Expr,
  Stmt,
  Item,
  Type,
};

struct MacroParamInfo {
  std::string name;
  MacroParamKind kind;
};

class MacroAST : public IAST {
  friend Visitor;
  std::string m_Name;
  std::vector<MacroParamInfo> m_Params;
  MacroReturnKind m_ReturnKind;

 public:
  MacroAST(std::string name, std::vector<MacroParamInfo> params,
           MacroReturnKind returnKind, SemanticLoc semLoc)
      : IAST(semLoc),
        m_Name(std::move(name)),
        m_Params(std::move(params)),
        m_ReturnKind(returnKind) {
    this->m_ASTKind = ASTKind::Decl;
    this->m_DeclKind = DeclKind::MacroDecl;
  }

  [[nodiscard]] const std::string& name() const { return m_Name; }
  [[nodiscard]] const std::vector<MacroParamInfo>& params() const {
    return m_Params;
  }
  [[nodiscard]] MacroReturnKind returnKind() const { return m_ReturnKind; }

  void debugNode() override { std::cout << "macro " << m_Name << "(...)"; }

  SymbolInfo acceptBefore(Visitor& visitor) override {
    return visitor.preVisit(*this);
  }

  ValuePtr accept(Visitor& visitor) override { return visitor.visit(*this); }
};

class DirectiveAST : public IAST {
  friend Visitor;
  std::string m_Name;

 public:
  DirectiveAST(std::string name, SemanticLoc semLoc)
      : IAST(semLoc), m_Name(std::move(name)) {
    this->m_ASTKind = ASTKind::Decl;
    this->m_DeclKind = DeclKind::DirectiveDecl;
  }

  [[nodiscard]] const std::string& name() const { return m_Name; }

  void debugNode() override { std::cout << "#" << m_Name; }

  SymbolInfo acceptBefore(Visitor& visitor) override {
    return visitor.preVisit(*this);
  }

  ValuePtr accept(Visitor& visitor) override { return visitor.visit(*this); }
};

#endif
