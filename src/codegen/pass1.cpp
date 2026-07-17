#include <codegen/codegen.hpp>

GlobalSymbolInfoList Visitor::GlobalSymbolTable{};
LocalSymbolInfoList Visitor::LocalSymbolTable{};
FunctionSignatureTable Visitor::FunctionTable{};
std::set<std::string> Visitor::ModuleAliases{};
std::map<std::string, StructInfo> Visitor::StructTable{};
std::map<std::string, TraitInfo> Visitor::TraitTable{};
std::map<std::string, EnumInfo> Visitor::EnumTable{};
std::map<std::string, ProtocolInfo> Visitor::ProtocolTable{};
std::map<std::string, llvm::StructType*> Visitor::LLVMStructTypes{};

// Type checking..
// Every type can be checked from symbolLists
// but must be checked firstly from globalSymbols
void CStarCodegen::pass1() {
  std::string funcName;
  std::vector<SemanticErrorMessage> localSymbolMessages;
  Visitor::SymbolId = 0;
  Visitor::ScopeId = 0;

  Visitor::GlobalSymbolTable = this->m_GlobalSymbols;
  Visitor::LocalSymbolTable = this->m_LocalSymbols;
  Visitor preVisitor(this->m_DefinedTypes, true);

  for (auto& ast : m_AST) {
    if (ast->getASTKind() == ASTKind::Decl) {
      if (ast->getDeclKind() == DeclKind::FuncDecl ||
          ast->getDeclKind() == DeclKind::ImportFuncDecl ||
          ast->getDeclKind() == DeclKind::ExportFuncDecl) {
        auto tempSymbolInfo = ast->acceptBefore(preVisitor);
        funcName = tempSymbolInfo.assocFuncName;

        for (auto& symbolInfo : preVisitor.getSymbolInfoList()) {
        }
      } else if (ast->getDeclKind() == DeclKind::VarDecl ||
                 ast->getDeclKind() == DeclKind::ImportVarDecl ||
                 ast->getDeclKind() == DeclKind::GlobVarDecl ||
                 ast->getDeclKind() == DeclKind::ExportVarDecl) {
        Visitor::SymbolId++;

        auto symbolInfo = ast->acceptBefore(preVisitor);
      } else if (ast->getDeclKind() == DeclKind::StructDecl ||
                 ast->getDeclKind() == DeclKind::TraitDecl ||
                 ast->getDeclKind() == DeclKind::EnumDecl ||
                 ast->getDeclKind() == DeclKind::ProtocolDecl) {
        ast->acceptBefore(preVisitor);
      }
    }
  }

  for (const auto& protocolEntry : Visitor::ProtocolTable) {
    const auto& protocol = protocolEntry.second;
    SymbolInfo protocolSymbol;
    protocolSymbol.symbolName = protocol.name;
    protocolSymbol.definedTypeName = protocol.name;
    protocolSymbol.type = TypeSpecifier::SPEC_DEFINED;

    if (Visitor::StructTable.count(protocol.targetTypeName) == 0) {
      localSymbolMessages.emplace_back(
          "protocol '" + protocol.name + "' targets unknown struct type '" +
              protocol.targetTypeName + "'",
          protocolSymbol);
    }

    auto checkProtocolMethod = [&](const std::string& methodName,
                                   bool scopeExitMethod) {
      const auto fullName = protocol.targetTypeName + "." + methodName;
      auto signatureIt = Visitor::FunctionTable.find(fullName);
      if (signatureIt == Visitor::FunctionTable.end()) {
        localSymbolMessages.emplace_back(
            "protocol '" + protocol.name + "' references unknown method '" +
                fullName + "'",
            protocolSymbol);
        return;
      }

      if (scopeExitMethod && signatureIt->second.params.size() != 1) {
        localSymbolMessages.emplace_back(
            "protocol scope_exit method '" + fullName +
                "' must be an instance method callable without user arguments",
            protocolSymbol);
      }

      if (scopeExitMethod && signatureIt->second.canThrow) {
        localSymbolMessages.emplace_back(
            "protocol scope_exit method '" + fullName +
                "' cannot be fallible in the current cleanup model",
            protocolSymbol);
      }
    };

    for (const auto& transition : protocol.transitions) {
      checkProtocolMethod(transition.methodName, false);
    }
    for (const auto& forbidden : protocol.forbiddenCalls) {
      checkProtocolMethod(forbidden.methodName, false);
    }
    for (const auto& transition : protocol.scopeExitTransitions) {
      checkProtocolMethod(transition.methodName, true);
    }
  }

  auto warnMessages = preVisitor.getTypeWarningMessages();
  for (auto it = warnMessages.begin(); it != warnMessages.end(); ++it) {
    SemanticHint(it->message, it->symbolInfo);
  }

  auto messages = preVisitor.getUnknownTypeErrorMessages();
  for (auto it = messages.rbegin(); it != messages.rend(); ++it) {
    SemanticError(it->message, it->symbolInfo, it->code);
  }

  for (auto it = localSymbolMessages.rbegin(); it != localSymbolMessages.rend();
       ++it) {
    SemanticError(it->message, it->symbolInfo, it->code);
  }

  return;
}
