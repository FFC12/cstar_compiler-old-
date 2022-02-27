#include <codegen/codegen.hpp>

// Type checking..
// Every type can be checked from symbolLists
// but must be checked firstly from globalSymbols
void CStarCodegen::pass1() {
  std::string funcName;
  std::vector<SemanticErrorMessage> localSymbolMessages;
  Visitor::SymbolId = 0;

  for (auto& ast : m_AST) {
    if (ast->getASTKind() == ASTKind::Decl) {
      if (ast->getDeclKind() == DeclKind::FuncDecl) {
        Visitor preVisitor(this->m_DefinedTypes, this->m_GlobalSymbols,
                           this->m_LocalSymbols, true);

        auto tempSymbolInfo = ast->acceptBefore(preVisitor);
        funcName = tempSymbolInfo.assocFuncName;

        for (auto& symbolInfo : preVisitor.getSymbolInfoList()) {
        }

        auto warnMessages = preVisitor.getTypeWarningMessages();
        for (auto it = warnMessages.rbegin(); it != warnMessages.rend(); ++it) {
          SemanticHint(it->message, it->symbolInfo);
        }

        auto messages = preVisitor.getUnknownTypeErrorMessages();
        for (auto it = messages.begin(); it != messages.end(); ++it) {
          SemanticError(it->message, it->symbolInfo);
        }

        for (auto it = localSymbolMessages.rbegin();
             it != localSymbolMessages.rend(); ++it) {
          SemanticError(it->message, it->symbolInfo);
        }
      } else if (ast->getDeclKind() == DeclKind::VarDecl ||
                 ast->getDeclKind() == DeclKind::ImportVarDecl ||
                 ast->getDeclKind() == DeclKind::GlobVarDecl ||
                 ast->getDeclKind() == DeclKind::ExportVarDecl) {
        Visitor preVisitor(this->m_DefinedTypes, this->m_GlobalSymbols,
                           this->m_LocalSymbols, true);
        Visitor::SymbolId++;

        auto symbolInfo = ast->acceptBefore(preVisitor);

        auto warnMessages = preVisitor.getTypeWarningMessages();
        for (auto it = warnMessages.rbegin(); it != warnMessages.rend(); ++it) {
          SemanticHint(it->message, it->symbolInfo);
        }

        auto messages = preVisitor.getUnknownTypeErrorMessages();
        for (auto it = messages.rbegin(); it != messages.rend(); ++it) {
          SemanticError(it->message, it->symbolInfo);
        }
      }
    }
  }

  return;
}
