#include <algorithm>
#include <visitor/semantic/semantic_private.hpp>

SymbolInfo Visitor::preVisit(ProtocolAST& protocolAst) {
  SymbolInfo symbolInfo;
  symbolInfo.begin = protocolAst.m_SemLoc.begin;
  symbolInfo.end = protocolAst.m_SemLoc.end;
  symbolInfo.line = protocolAst.m_SemLoc.line;
  symbolInfo.definedTypeName = protocolAst.m_Name;

  ProtocolInfo info;
  info.name = protocolAst.m_Name;
  info.targetTypeName = protocolAst.m_TargetTypeName;
  info.states = protocolAst.m_States;
  info.defaultState = protocolAst.m_DefaultState;
  info.isDynamic = protocolAst.m_IsDynamic;

  auto hasState = [&](const std::string& state) {
    return std::find(info.states.begin(), info.states.end(), state) !=
           info.states.end();
  };

  if (info.states.empty()) {
    m_TypeErrorMessages.emplace_back(
        "protocol '" + info.name + "' must declare at least one state",
        symbolInfo);
  }
  if (info.defaultState.empty()) {
    m_TypeErrorMessages.emplace_back(
        "protocol '" + info.name + "' must declare a default state",
        symbolInfo);
  } else if (!hasState(info.defaultState)) {
    m_TypeErrorMessages.emplace_back(
        "protocol '" + info.name + "' default state '" + info.defaultState +
            "' is not in its state set",
        symbolInfo);
  }

  if (m_TypeChecking && StructTable.count(info.targetTypeName) == 0) {
    m_TypeErrorMessages.emplace_back(
        "protocol '" + info.name + "' targets unknown struct type '" +
            info.targetTypeName + "'",
        symbolInfo);
  }

  auto checkMethodExists = [&](const std::string& methodName,
                               bool mustBeScopeExitSafe) {
    if (!m_TypeChecking || info.targetTypeName.empty() ||
        methodName.empty()) {
      return;
    }

    const auto fullName = info.targetTypeName + "." + methodName;
    auto signatureIt = FunctionTable.find(fullName);
    if (signatureIt == FunctionTable.end()) {
      m_TypeErrorMessages.emplace_back(
          "protocol '" + info.name + "' references unknown method '" +
              fullName + "'",
          symbolInfo);
      return;
    }

    if (mustBeScopeExitSafe && signatureIt->second.params.size() > 1) {
      m_TypeErrorMessages.emplace_back(
          "protocol scope_exit method '" + fullName +
              "' must be callable without user arguments",
          symbolInfo);
    }

    if (mustBeScopeExitSafe && signatureIt->second.canThrow) {
      m_TypeErrorMessages.emplace_back(
          "protocol scope_exit method '" + fullName +
              "' cannot be fallible in the current cleanup model",
          symbolInfo);
    }
  };

  for (const auto& transition : protocolAst.m_Transitions) {
    if (!hasState(transition.fromState) || !hasState(transition.toState)) {
      m_TypeErrorMessages.emplace_back(
          "protocol transition '" + transition.fromState + " -> " +
              transition.toState + "' references an unknown state",
          symbolInfo);
    }
    checkMethodExists(transition.methodName, false);
    info.transitions.push_back(
        {transition.fromState, transition.toState, transition.methodName});
  }

  for (const auto& forbidden : protocolAst.m_ForbiddenCalls) {
    if (!hasState(forbidden.forbiddenState)) {
      m_TypeErrorMessages.emplace_back(
          "protocol forbidden-call rule for '" + forbidden.methodName +
              "' references unknown state '" + forbidden.forbiddenState + "'",
          symbolInfo);
    }
    checkMethodExists(forbidden.methodName, false);
    info.forbiddenCalls.push_back(
        {forbidden.methodName, forbidden.forbiddenState});
  }

  for (const auto& transition : protocolAst.m_ScopeExitTransitions) {
    if (!hasState(transition.fromState) || !hasState(transition.toState)) {
      m_TypeErrorMessages.emplace_back(
          "protocol scope_exit transition '" + transition.fromState + " -> " +
              transition.toState + "' references an unknown state",
          symbolInfo);
    }
    checkMethodExists(transition.methodName, true);
    info.scopeExitTransitions.push_back(
        {transition.fromState, transition.toState, transition.methodName});
  }

  if (info.isDynamic && m_TypeChecking) {
    m_TypeErrorMessages.emplace_back(
        "dynamic protocol runtime tag lowering is not implemented yet; use a "
        "static/provable protocol for this MVP",
        symbolInfo);
  }

  auto existing = ProtocolTable.find(info.name);
  if (existing != ProtocolTable.end() && !m_TypeChecking) {
    m_TypeErrorMessages.emplace_back(
        "Redefinition of protocol '" + info.name + "'", symbolInfo);
  }
  ProtocolTable[info.name] = std::move(info);
  return symbolInfo;
}
