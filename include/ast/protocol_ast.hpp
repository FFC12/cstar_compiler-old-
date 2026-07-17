#ifndef PROTOCOL_AST_HPP
#define PROTOCOL_AST_HPP

#include <ast/ast.hpp>

#include <string>
#include <utility>
#include <vector>

struct ProtocolTransitionASTInfo {
  std::string fromState;
  std::string toState;
  std::string methodName;
};

struct ProtocolForbiddenCallASTInfo {
  std::string methodName;
  std::string forbiddenState;
};

class ProtocolAST : public IAST {
  friend Visitor;
  std::string m_Name;
  std::string m_TargetTypeName;
  std::vector<std::string> m_States;
  std::string m_DefaultState;
  std::vector<ProtocolTransitionASTInfo> m_Transitions;
  std::vector<ProtocolForbiddenCallASTInfo> m_ForbiddenCalls;
  std::vector<ProtocolTransitionASTInfo> m_ScopeExitTransitions;
  bool m_IsDynamic;

 public:
  ProtocolAST(std::string name, std::string targetTypeName,
              std::vector<std::string> states, std::string defaultState,
              std::vector<ProtocolTransitionASTInfo> transitions,
              std::vector<ProtocolForbiddenCallASTInfo> forbiddenCalls,
              std::vector<ProtocolTransitionASTInfo> scopeExitTransitions,
              bool isDynamic, SemanticLoc semLoc)
      : IAST(semLoc),
        m_Name(std::move(name)),
        m_TargetTypeName(std::move(targetTypeName)),
        m_States(std::move(states)),
        m_DefaultState(std::move(defaultState)),
        m_Transitions(std::move(transitions)),
        m_ForbiddenCalls(std::move(forbiddenCalls)),
        m_ScopeExitTransitions(std::move(scopeExitTransitions)),
        m_IsDynamic(isDynamic) {
    this->m_ASTKind = ASTKind::Decl;
    this->m_DeclKind = DeclKind::ProtocolDecl;
  }

  [[nodiscard]] const std::string& name() const { return m_Name; }

  void debugNode() override {
    if (m_IsDynamic) std::cout << "dynamic ";
    std::cout << "protocol " << m_Name << " for " << m_TargetTypeName
              << " { ... }";
  }

  SymbolInfo acceptBefore(Visitor& visitor) override {
    return visitor.preVisit(*this);
  }

  ValuePtr accept(Visitor& visitor) override { return visitor.visit(*this); }
};

#endif
