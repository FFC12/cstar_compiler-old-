#include <algorithm>
#include <codegen/codegen.hpp>
#include <utility>

size_t Visitor::SymbolId = 0;

void CStarCodegen::pass0() {
  std::string funcName;
  Visitor::SymbolId = 0;

  // TODO: Will be revised
  // this->m_DefinedTypes["Type"] = 1;

  std::vector<SemanticErrorMessage> localSymbolMessages;

  for (auto& ast : m_AST) {
    if (ast->getASTKind() == ASTKind::Decl) {
      if (ast->getDeclKind() == DeclKind::FuncDecl) {
        Visitor preVisitor(this->m_DefinedTypes);

        auto tempSymbolInfo = ast->acceptBefore(preVisitor);
        funcName = tempSymbolInfo.assocFuncName;

        SymbolInfoList symbolInfoList;
        for (auto& symbolInfo : preVisitor.getSymbolInfoList()) {
          if (!redefinitionCheck(symbolInfoList, symbolInfo)) {
            symbolInfoList.push_back(
                SymbolInfoEntry(symbolInfo.symbolName, symbolInfo));
          } else {
            localSymbolMessages.emplace_back(
                "Redefinition of the local symbol '" + symbolInfo.symbolName +
                    "'",
                symbolInfo);
          }
        }

        auto messages = preVisitor.getUnknownTypeErrorMessages();
        for (auto it = messages.rbegin(); it != messages.rend(); ++it) {
          SemanticError(it->message, it->symbolInfo);
        }

        for (auto it = localSymbolMessages.rbegin();
             it != localSymbolMessages.rend(); ++it) {
          SemanticError(it->message, it->symbolInfo);
        }

        if (this->m_LocalSymbols.count(funcName) != 0) {
          SemanticError("Redefinition of the function '" + funcName + '"',
                        tempSymbolInfo);
        }
        this->m_LocalSymbols[funcName] = std::move(symbolInfoList);
      } else if (ast->getDeclKind() == DeclKind::VarDecl ||
                 ast->getDeclKind() == DeclKind::ImportVarDecl ||
                 ast->getDeclKind() == DeclKind::GlobVarDecl ||
                 ast->getDeclKind() == DeclKind::ExportVarDecl) {
        Visitor preVisitor(this->m_DefinedTypes);
        Visitor::SymbolId++;

        auto symbolInfo = ast->acceptBefore(preVisitor);

        if (redefinitionCheck(symbolInfo)) {
          SemanticError("Redefinition of the global symbol '" +
                            symbolInfo.symbolName + "'",
                        symbolInfo);
        }

        this->m_GlobalSymbols.push_back(
            SymbolInfoEntry(symbolInfo.symbolName, symbolInfo));
      }
    }
  }

  return;
}


bool CStarCodegen::redefinitionCheck(SymbolInfo& symbol) {
  return redefinitionCheck(m_GlobalSymbols, symbol);
}

bool CStarCodegen::redefinitionCheck(SymbolInfoList& symbols,
                                     SymbolInfo& symbol) {
  bool redefinationFlag = false;

  auto entries = std::find(symbols.begin(), symbols.end(), symbol);
  if(entries != symbols.end()) {
    redefinationFlag = true;
  }

  return redefinationFlag;
}

void CStarCodegen::SemanticError(std::string message, SymbolInfo& symbolInfo) {
  this->m_Parser.ParserError(std::move(message), symbolInfo.begin,
                             symbolInfo.end, symbolInfo.line);
  this->m_SemAnalysisFailure = true;
  this->m_ErrorCount += 1;
}

void CStarCodegen::SemanticHint(std::string message, SymbolInfo& symbolInfo) {
  this->m_Parser.ParserWarning(std::move(message), symbolInfo.begin,
                               symbolInfo.end, symbolInfo.line);
  this->m_WarningCount += 1;
}