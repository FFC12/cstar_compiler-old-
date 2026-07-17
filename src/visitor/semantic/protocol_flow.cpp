#include <algorithm>
#include <visitor/semantic/semantic_private.hpp>

const ProtocolInfo* Visitor::protocolForType(
    const std::string& typeName) const {
  for (const auto& entry : ProtocolTable) {
    if (entry.second.targetTypeName == typeName && !entry.second.isDynamic) {
      return &entry.second;
    }
  }
  return nullptr;
}

void Visitor::applyProtocolMethodCall(const std::string& funcName,
                                      FuncCallAST& funcCallAst,
                                      SymbolInfo& symbolInfo) {
  std::string owner;
  std::string method;
  if (!SplitStructMethodName(funcName, owner, method)) {
    return;
  }

  auto* receiver = methodCallReceiver(funcCallAst.m_FuncSymbol.get());
  if (receiver == nullptr || receiver->m_ExprKind != ExprKind::SymbolExpr) {
    return;
  }

  auto* receiverSymbol = static_cast<SymbolAST*>(receiver);
  SymbolInfo lookupInfo;
  SymbolInfo receiverInfo;
  auto receiverName = receiverSymbol->m_SymbolName;
  if (!symbolValidation(receiverName, lookupInfo, receiverInfo, true)) {
    return;
  }

  const auto* protocol = protocolForType(receiverInfo.definedTypeName);
  if (protocol == nullptr) {
    return;
  }

  auto currentState = protocol->defaultState;
  auto flowIt = m_LocalProtocolStates.find(receiverName);
  if (flowIt != m_LocalProtocolStates.end()) {
    auto stateIt = flowIt->second.find(protocol->name);
    if (stateIt != flowIt->second.end()) {
      currentState = stateIt->second;
    }
  } else {
    auto declaredIt = receiverInfo.protocolStates.find(protocol->name);
    if (declaredIt != receiverInfo.protocolStates.end()) {
      currentState = declaredIt->second;
    }
  }

  for (const auto& forbidden : protocol->forbiddenCalls) {
    if (forbidden.methodName == method &&
        forbidden.forbiddenState == currentState) {
      this->m_TypeErrorMessages.emplace_back(
          "protocol '" + protocol->name + "' forbids method '" + method +
              "' while '" + receiverName + "' is in state '" + currentState +
              "'",
          symbolInfo);
      return;
    }
  }

  for (const auto& transition : protocol->transitions) {
    if (transition.methodName == method && transition.fromState == currentState) {
      m_LocalProtocolStates[receiverName][protocol->name] = transition.toState;
      symbolInfo.protocolStates[protocol->name] = transition.toState;
      return;
    }
  }
}
