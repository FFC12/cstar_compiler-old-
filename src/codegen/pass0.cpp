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
          if (!redefinationCheck(symbolInfoList, symbolInfo)) {
            symbolInfoList.insert({symbolInfo.symbolName, symbolInfo});
          } else {
            RedefinationError(
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

        if (redefinationCheck(symbolInfo)) {
          RedefinationError(
              "Redefination of global symbol '" + symbolInfo.symbolName + "'",
              symbolInfo);
        }

        this->m_GlobalSymbols.insert({symbolInfo.symbolName, symbolInfo});
      }
    }
  }

  return;
}

bool CStarCodegen::redefinationCheck(SymbolInfo& symbolInfo) {
  return redefinationCheck(m_GlobalSymbols, symbolInfo);
}

bool CStarCodegen::redefinationCheck(SymbolInfoList& symbolInfoList,
                                     SymbolInfo& symbolInfo) {
  bool redefinationFlag = false;

  auto entries = symbolInfoList.equal_range(symbolInfo.symbolName);

  for (auto it = entries.first; it != entries.second; ++it) {
    if (it->first == symbolInfo.symbolName) {
      if (it->second.scopeLevel <= symbolInfo.scopeLevel) {
        if (it->second.scopeLevel == symbolInfo.scopeLevel &&
            it->second.scopeId != symbolInfo.scopeId) {
          break;
        }
        redefinationFlag = true;
        break;
      }
    }
  }

  return redefinationFlag;
}

void CStarCodegen::RedefinationError(std::string message,
                                     SymbolInfo& symbolInfo) {
  m_Parser.ParserError(std::move(message), symbolInfo.begin, symbolInfo.end,
                       symbolInfo.line);
  m_SemAnalysisFailure = true;
  m_ErrorCount += 1;
}