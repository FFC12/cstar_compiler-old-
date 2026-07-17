#include <visitor/codegen/codegen_private.hpp>

#include <algorithm>

namespace {

bool splitStructMethodName(const std::string& name, std::string& owner,
                           std::string& method) {
  const auto dot = name.find('.');
  if (dot == std::string::npos || dot == 0 || dot + 1 >= name.size()) {
    return false;
  }

  owner = name.substr(0, dot);
  method = name.substr(dot + 1);
  return true;
}

const ProtocolInfo* protocolByName(const std::string& name) {
  auto it = Visitor::ProtocolTable.find(name);
  return it == Visitor::ProtocolTable.end() ? nullptr : &it->second;
}

}  // namespace

ValuePtr Visitor::visit(ProtocolAST& protocolAst) { return nullptr; }

void Visitor::registerCodegenProtocolStates(const SymbolInfo& symbolInfo) {
  if (symbolInfo.type != TypeSpecifier::SPEC_DEFINED ||
      symbolInfo.definedTypeName.empty() || symbolInfo.isGlob) {
    return;
  }

  std::map<std::string, std::string> states = symbolInfo.protocolStates;
  if (states.empty()) {
    if (const auto* protocol = protocolForType(symbolInfo.definedTypeName)) {
      if (!protocol->defaultState.empty()) {
        states[protocol->name] = protocol->defaultState;
      }
    }
  }

  if (states.empty()) {
    return;
  }

  if (m_CodegenProtocolStates.count(symbolInfo.symbolName) == 0) {
    m_CodegenProtocolStateOrder.push_back(symbolInfo.symbolName);
  }
  m_CodegenProtocolStates[symbolInfo.symbolName] = std::move(states);
}

void Visitor::applyCodegenProtocolMethodCall(const std::string& funcName,
                                             FuncCallAST& funcCallAst) {
  std::string owner;
  std::string method;
  if (!splitStructMethodName(funcName, owner, method)) {
    return;
  }

  auto* receiver = methodCallReceiver(funcCallAst.m_FuncSymbol.get());
  if (receiver == nullptr || receiver->m_ExprKind != ExprKind::SymbolExpr) {
    return;
  }

  auto* receiverSymbol = static_cast<SymbolAST*>(receiver);
  auto receiverInfo = getSymbolInfo(receiverSymbol->m_SymbolName);
  const auto* protocol = protocolForType(receiverInfo.definedTypeName);
  if (protocol == nullptr) {
    return;
  }

  auto& states = m_CodegenProtocolStates[receiverSymbol->m_SymbolName];
  if (std::find(m_CodegenProtocolStateOrder.begin(),
                m_CodegenProtocolStateOrder.end(),
                receiverSymbol->m_SymbolName) ==
      m_CodegenProtocolStateOrder.end()) {
    m_CodegenProtocolStateOrder.push_back(receiverSymbol->m_SymbolName);
  }

  auto currentState = protocol->defaultState;
  auto stateIt = states.find(protocol->name);
  if (stateIt != states.end()) {
    currentState = stateIt->second;
  } else if (auto declaredIt = receiverInfo.protocolStates.find(protocol->name);
             declaredIt != receiverInfo.protocolStates.end()) {
    currentState = declaredIt->second;
  }

  for (const auto& transition : protocol->transitions) {
    if (transition.methodName == method &&
        transition.fromState == currentState) {
      states[protocol->name] = transition.toState;
      return;
    }
  }
}

void Visitor::emitProtocolScopeExitCleanups() {
  if (m_EmittingProtocolScopeExit) {
    return;
  }

  auto* insertBlock = Builder->GetInsertBlock();
  if (insertBlock == nullptr || insertBlock->getTerminator() != nullptr) {
    return;
  }

  m_EmittingProtocolScopeExit = true;
  for (auto symbolIt = m_CodegenProtocolStateOrder.rbegin();
       symbolIt != m_CodegenProtocolStateOrder.rend(); ++symbolIt) {
    const auto stateMapIt = m_CodegenProtocolStates.find(*symbolIt);
    if (stateMapIt == m_CodegenProtocolStates.end()) {
      continue;
    }

    auto symbolInfo = getSymbolInfo(*symbolIt);
    if (symbolInfo.type != TypeSpecifier::SPEC_DEFINED ||
        symbolInfo.isGlob || m_CodegenDroppedSymbols.count(*symbolIt) != 0) {
      continue;
    }

    for (const auto& stateEntry : stateMapIt->second) {
      const auto* protocol = protocolByName(stateEntry.first);
      if (protocol == nullptr || protocol->targetTypeName !=
                                      symbolInfo.definedTypeName) {
        continue;
      }

      for (const auto& transition : protocol->scopeExitTransitions) {
        if (transition.fromState != stateEntry.second) {
          continue;
        }

        auto* cleanupFunction = Module->getFunction(
            symbolInfo.definedTypeName + "." + transition.methodName);
        auto* storage =
            FindStorage(m_LocalVarsOnScope, m_GlobalVars, *symbolIt);
        if (cleanupFunction == nullptr || storage == nullptr) {
          continue;
        }

        llvm::Value* self = storage;
        if (symbolInfo.indirectionLevel > 0 && !symbolInfo.isRef) {
          if (IsSharedPointerSymbol(symbolInfo)) {
            auto* handle = Builder->CreateLoad(GetSharedPointerTy(), storage,
                                               *symbolIt + ".scope_exit.sp");
            self = ExtractSharedPointerData(handle);
          } else {
            auto* pointerType =
                GetType(symbolInfo.type, symbolInfo.indirectionLevel);
            self = Builder->CreateLoad(pointerType, storage,
                                       *symbolIt + ".scope_exit.ptr");
          }
        } else if (symbolInfo.isRef &&
                   m_ReferenceParamValueTypes.count(*symbolIt) > 0) {
          auto* slotType = GetPointeeType(storage);
          self = CreateLoad(storage, slotType, *symbolIt + ".scope_exit.ref");
        }

        Builder->CreateCall(cleanupFunction, {self});
      }
    }
  }
  m_EmittingProtocolScopeExit = false;
}
