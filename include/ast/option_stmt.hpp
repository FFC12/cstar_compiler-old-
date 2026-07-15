#ifndef OPTION_STMT_HPP
#define OPTION_STMT_HPP

#include <ast/ast.hpp>
#include <vector>

struct OptionCase {
  ASTNode pattern;
  Scope scope;
  bool isDefault = false;
  SemanticLoc loc;

  OptionCase(ASTNode patternNode, Scope&& caseScope, bool defaultCase,
             SemanticLoc semLoc)
      : pattern(std::move(patternNode)),
        scope(std::move(caseScope)),
        isDefault(defaultCase),
        loc(semLoc) {}
};

class OptionStmtAST : public IAST {
  friend Visitor;
  ASTNode m_Value;
  std::vector<OptionCase> m_Cases;

 public:
  OptionStmtAST(ASTNode value, std::vector<OptionCase>&& cases,
                SemanticLoc& semLoc)
      : IAST(semLoc), m_Value(std::move(value)), m_Cases(std::move(cases)) {
    this->m_ASTKind = ASTKind::Stmt;
    this->m_StmtKind = StmtKind::OptionStmt;
  }

  void debugNode() override {
    std::cout << "option(";
    m_Value->debugNode();
    std::cout << "){\n";
    for (auto& optionCase : m_Cases) {
      if (optionCase.isDefault) {
        std::cout << "_";
      } else {
        optionCase.pattern->debugNode();
      }
      std::cout << ":{\n";
      for (auto& node : optionCase.scope) {
        node->debugNode();
      }
      std::cout << "}\n";
    }
    std::cout << "}\n";
  }

  SymbolInfo acceptBefore(Visitor& visitor) override {
    return visitor.preVisit(*this);
  }

  ValuePtr accept(Visitor& visitor) override { return visitor.visit(*this); }
};

#endif
