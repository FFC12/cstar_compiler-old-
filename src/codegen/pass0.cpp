#include <codegen/codegen.hpp>
#include <utility>

void CStarCodegen::pass0() {
  std::string funcName;

  for (auto& ast : m_AST) {
    if (ast->getASTKind() == ASTKind::Decl) {
      if (ast->getDeclKind() == DeclKind::FuncDecl) {
        Visitor preVisitor{};
        auto tempSymbolInfo = ast->acceptBefore(preVisitor);
        funcName = tempSymbolInfo.assocFuncName;

        SymbolInfoList symbolInfoList;
        for (auto& symbolInfo : preVisitor.getSymbolInfoList()) {
          if (!redefinitionCheck(symbolInfoList, symbolInfo)) {
            symbolInfoList.insert({symbolInfo.symbolName, symbolInfo});
          } else {
            RedefinitionError(
                "Redefination of local symbol '" + symbolInfo.symbolName + "'",
                symbolInfo);
          }
        }

        this->m_LocalSymbols.insert({funcName, std::move(symbolInfoList)});
      } else if (ast->getDeclKind() == DeclKind::VarDecl ||
                 ast->getDeclKind() == DeclKind::ImportVarDecl ||
                 ast->getDeclKind() == DeclKind::GlobVarDecl ||
                 ast->getDeclKind() == DeclKind::ExportVarDecl) {
        Visitor preVisitor{};
        auto symbolInfo = ast->acceptBefore(preVisitor);

        if (redefinitionCheck(symbolInfo)) {
          RedefinitionError(
              "Redefination of global symbol '" + symbolInfo.symbolName + "'",
              symbolInfo);
        }

        this->m_GlobalSymbols.insert({symbolInfo.symbolName, symbolInfo});
      }
    }
  }

  return;
}

bool CStarCodegen::redefinitionCheck(SymbolInfo& symbol) {
  return redefinitionCheck(m_GlobalSymbols, symbol);
}

bool CStarCodegen::redefinitionCheck(SymbolInfoList& funcName,
                                     SymbolInfo& symbol) {
  bool redefinationFlag = false;

  auto entries = funcName.equal_range(symbol.symbolName);

  for (auto it = entries.first; it != entries.second; ++it) {
    if (it->first == symbol.symbolName) {
      if (it->second.scopeLevel <= symbol.scopeLevel) {
        if (it->second.scopeLevel == symbol.scopeLevel &&
            it->second.scopeId != symbol.scopeId) {
          break;
        }
        redefinationFlag = true;
        break;
      }
    }
  }

  return redefinationFlag;
}

void CStarCodegen::RedefinitionError(std::string message,
                                     SymbolInfo& symbolInfo) {
  m_Parser.ParserError(std::move(message), symbolInfo.begin, symbolInfo.end,
                       symbolInfo.line);
  m_SemAnalysisFailure = true;
  m_ErrorCount += 1;
}