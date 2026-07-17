#include <parser/parser_private.hpp>

#include <cstring>
#include <cstdlib>
#include <fstream>
#include <memory>
#include <sstream>
#include <vector>

using namespace cstar::parser_private;

void CStarParser::collectPublicMacrosFromSourceIncludes() {
  for (size_t index = 0; index < m_TokenStream.size(); ++index) {
    if (m_TokenStream[index].getTokenKind() != TokenKind::INCLUDE) {
      continue;
    }

    size_t cursor = index + 1;
    while (cursor < m_TokenStream.size() &&
           (m_TokenStream[cursor].getTokenKind() == TokenKind::LINEFEED ||
            m_TokenStream[cursor].getTokenKind() == TokenKind::COMMENT)) {
      cursor += 1;
    }

    if (cursor >= m_TokenStream.size() ||
        (m_TokenStream[cursor].getTokenKind() != TokenKind::LITERAL &&
         m_TokenStream[cursor].getTokenKind() != TokenKind::LETTER)) {
      continue;
    }

    const auto includeName = m_TokenStream[cursor].getTokenAsStr();
    auto includePath = ResolveLogicalIncludeSource(includeName);
    if (includePath.empty()) {
      continue;
    }
    cursor += 1;

    while (cursor < m_TokenStream.size() &&
           (m_TokenStream[cursor].getTokenKind() == TokenKind::LINEFEED ||
            m_TokenStream[cursor].getTokenKind() == TokenKind::COMMENT)) {
      cursor += 1;
    }

    if (cursor >= m_TokenStream.size() ||
        m_TokenStream[cursor].getTokenKind() != TokenKind::AS) {
      continue;
    }
    cursor += 1;

    while (cursor < m_TokenStream.size() &&
           (m_TokenStream[cursor].getTokenKind() == TokenKind::LINEFEED ||
            m_TokenStream[cursor].getTokenKind() == TokenKind::COMMENT)) {
      cursor += 1;
    }

    if (cursor >= m_TokenStream.size() ||
        m_TokenStream[cursor].getTokenKind() != TokenKind::IDENT) {
      continue;
    }

    auto sourcePath = std::filesystem::path(m_Lexer.getFilepath().get());
    includePath = ResolveSourceIncludePath(includePath, sourcePath);
    collectPublicMacrosFromIncludedFile(includePath,
                                        m_TokenStream[cursor].getTokenAsStr());
  }
}

void CStarParser::collectPublicMacrosFromIncludedFile(
    const std::filesystem::path& path, const std::string& alias) {
  if (m_PreprocessedMacroIncludes.count(path) != 0) {
    return;
  }
  m_PreprocessedMacroIncludes.insert(path);

  std::ifstream file(path);
  if (!file.is_open()) {
    return;
  }

  std::stringstream ss;
  ss << file.rdbuf();

  auto realPathStr = path.string();
  auto realPath = static_cast<char*>(std::malloc(realPathStr.size() + 1));
  std::memcpy(realPath, realPathStr.c_str(), realPathStr.size() + 1);
  const std::shared_ptr<char> realPathPtr(realPath, std::free);

  CStarLexer lexer(ss.str(), realPathPtr);
  auto tokens = lexer.perform();
  std::vector<TokenInfo> ignoredOutput;

  for (size_t index = 0; index < tokens.size();) {
    if (tokens[index].getTokenKind() == TokenKind::INCLUDE) {
      size_t cursor = index + 1;
      while (cursor < tokens.size() &&
             (tokens[cursor].getTokenKind() == TokenKind::LINEFEED ||
              tokens[cursor].getTokenKind() == TokenKind::COMMENT)) {
        cursor += 1;
      }
      if (cursor < tokens.size() &&
          (tokens[cursor].getTokenKind() == TokenKind::LITERAL ||
           tokens[cursor].getTokenKind() == TokenKind::LETTER)) {
        const auto nestedName = tokens[cursor].getTokenAsStr();
        auto nestedPath = ResolveLogicalIncludeSource(nestedName);
        if (!nestedPath.empty()) {
          collectPublicMacrosFromIncludedFile(
              ResolveSourceIncludePath(nestedPath, path), alias);
        }
      }
    }

    if (tokens[index].getTokenKind() == TokenKind::PUBLIC) {
      size_t cursor = index + 1;
      while (cursor < tokens.size() &&
             (tokens[cursor].getTokenKind() == TokenKind::LINEFEED ||
              tokens[cursor].getTokenKind() == TokenKind::COMMENT)) {
        cursor += 1;
      }
      if (cursor < tokens.size() &&
          tokens[cursor].getTokenKind() == TokenKind::MACRO &&
          cursor + 1 < tokens.size() &&
          tokens[cursor + 1].getTokenKind() == TokenKind::IDENT) {
        const auto qualifiedName =
            alias + "." + tokens[cursor + 1].getTokenAsStr();
        index = collectIncludedPublicMacroDefinition(cursor, tokens,
                                                     qualifiedName);
        continue;
      }
    }
    index += 1;
  }
}

size_t CStarParser::collectIncludedPublicMacroDefinition(
    size_t index, std::vector<TokenInfo>& tokens,
    const std::string& qualifiedName) {
  auto savedTokens = std::move(m_TokenStream);
  m_TokenStream = std::move(tokens);

  std::vector<TokenInfo> ignoredOutput;
  const size_t nextIndex = collectMacroDefinition(index, ignoredOutput);
  auto macro = m_CompileTimeMacros[m_TokenStream[index + 1].getTokenAsStr()];
  m_CompileTimeMacros.erase(m_TokenStream[index + 1].getTokenAsStr());
  macro.name = qualifiedName;
  m_CompileTimeMacros[qualifiedName] = std::move(macro);

  tokens = std::move(m_TokenStream);
  m_TokenStream = std::move(savedTokens);
  return nextIndex;
}

void CStarParser::skipTopLevelTrivia() {
  while (is(TokenKind::COMMENT) || is(TokenKind::LINEFEED)) {
    this->advance();
  }
}

size_t CStarParser::skipBalancedInTokenStream(size_t index, TokenKind open,
                                              TokenKind close) const {
  size_t depth = 0;
  while (index < m_TokenStream.size()) {
    const auto kind = m_TokenStream[index].getTokenKind();
    if (kind == open) {
      depth += 1;
    } else if (kind == close) {
      if (depth == 0) {
        return index;
      }
      depth -= 1;
      if (depth == 0) {
        return index + 1;
      }
    }
    index += 1;
  }
  return index;
}

size_t CStarParser::collectMacroDefinition(
    size_t index, std::vector<TokenInfo>& output) {
  auto macroToken = m_TokenStream[index];
  index += 1;
  if (index >= m_TokenStream.size() ||
      m_TokenStream[index].getTokenKind() != TokenKind::IDENT) {
    ParserError("Expected macro name", macroToken);
  }

  CompileTimeMacroDefinition macro;
  macro.name = m_TokenStream[index].getTokenAsStr();
  if (m_CompileTimeMacros.count(macro.name) != 0) {
    ParserError("Redefinition of macro '" + macro.name + "'",
                m_TokenStream[index]);
  }
  index += 1;

  if (index >= m_TokenStream.size() ||
      m_TokenStream[index].getTokenKind() != TokenKind::LPAREN) {
    ParserError("Expected macro parameter list", macroToken);
  }
  index += 1;

  std::set<std::string> paramNames;
  while (index < m_TokenStream.size() &&
         m_TokenStream[index].getTokenKind() != TokenKind::RPAREN) {
    if (m_TokenStream[index].getTokenKind() == TokenKind::LINEFEED ||
        m_TokenStream[index].getTokenKind() == TokenKind::COMMA) {
      index += 1;
      continue;
    }
    if (m_TokenStream[index].getTokenKind() != TokenKind::DOLLAR) {
      ParserError("Expected '$' before macro parameter", m_TokenStream[index]);
    }
    index += 1;
    if (index >= m_TokenStream.size() ||
        m_TokenStream[index].getTokenKind() != TokenKind::IDENT) {
      ParserError("Expected macro parameter name", macroToken);
    }
    const auto paramName = m_TokenStream[index].getTokenAsStr();
    if (paramNames.count(paramName) != 0) {
      ParserError("Redefinition of macro parameter '" + paramName + "'",
                  m_TokenStream[index]);
    }
    paramNames.insert(paramName);
    macro.params.push_back(paramName);
    index += 1;
    if (index >= m_TokenStream.size() ||
        m_TokenStream[index].getTokenKind() != TokenKind::COLON) {
      ParserError("Expected macro parameter kind", macroToken);
    }
    index += 1;
    if (index >= m_TokenStream.size() ||
        m_TokenStream[index].getTokenKind() != TokenKind::IDENT) {
      ParserError("Expected macro parameter kind", macroToken);
    }
    if (!IsMacroParamKindName(m_TokenStream[index].getTokenAsStr())) {
      ParserError("Unknown macro parameter kind '" +
                      m_TokenStream[index].getTokenAsStr() +
                      "'. Expected expr, stmt, item, type, ident or tokens",
                  m_TokenStream[index]);
    }
    index += 1;
  }

  if (index >= m_TokenStream.size()) {
    ParserError("Unterminated macro parameter list", macroToken);
  }
  index += 1;

  if (index >= m_TokenStream.size() ||
      m_TokenStream[index].getTokenKind() != TokenKind::ARROW) {
    ParserError("Expected macro return kind", macroToken);
  }
  index += 1;
  if (index >= m_TokenStream.size() ||
      m_TokenStream[index].getTokenKind() != TokenKind::IDENT) {
    ParserError("Expected macro return kind", macroToken);
  }
  if (!IsMacroReturnKindName(m_TokenStream[index].getTokenAsStr())) {
    ParserError("Unknown macro return kind '" +
                    m_TokenStream[index].getTokenAsStr() +
                    "'. Expected expr, stmt, item or type",
                m_TokenStream[index]);
  }
  index += 1;

  while (index < m_TokenStream.size() &&
         m_TokenStream[index].getTokenKind() == TokenKind::LINEFEED) {
    index += 1;
  }
  if (index >= m_TokenStream.size() ||
      m_TokenStream[index].getTokenKind() != TokenKind::LBRACK) {
    ParserError("Expected macro body", macroToken);
  }

  const size_t bodyBegin = index + 1;
  const size_t bodyEndExclusive =
      skipBalancedInTokenStream(index, TokenKind::LBRACK, TokenKind::RBRACK);
  if (bodyEndExclusive <= bodyBegin) {
    ParserError("Unterminated macro body", macroToken);
  }
  for (size_t i = bodyBegin; i + 1 < bodyEndExclusive; ++i) {
    if (m_TokenStream[i].getTokenKind() == TokenKind::LINEFEED ||
        m_TokenStream[i].getTokenKind() == TokenKind::COMMENT) {
      continue;
    }
    macro.body.push_back(m_TokenStream[i]);
  }
  m_CompileTimeMacros[macro.name] = std::move(macro);

  if (!output.empty() &&
      output.back().getTokenKind() != TokenKind::LINEFEED) {
    output.emplace_back(TokenKind::LINEFEED,
                        macroToken.getTokenPositionInfo(), "\n", false);
  }
  return bodyEndExclusive;
}

size_t CStarParser::copyAttributeDefinition(
    size_t index, std::vector<TokenInfo>& output) {
  const size_t start = index;
  while (index < m_TokenStream.size() &&
         m_TokenStream[index].getTokenKind() != TokenKind::LBRACK) {
    index += 1;
  }
  if (index >= m_TokenStream.size()) {
    ParserError("Expected attribute body", m_TokenStream[start]);
  }

  const size_t end = skipBalancedInTokenStream(index, TokenKind::LBRACK,
                                               TokenKind::RBRACK);
  for (size_t i = start; i < end && i < m_TokenStream.size(); ++i) {
    output.push_back(m_TokenStream[i]);
  }
  return end;
}

bool CStarParser::evaluateDirectiveCondition(size_t begin, size_t end) {
  while (begin < end &&
         m_TokenStream[begin].getTokenKind() == TokenKind::LINEFEED) {
    begin += 1;
  }

  bool negate = false;
  if (begin < end && m_TokenStream[begin].getTokenKind() == TokenKind::NOT) {
    negate = true;
    begin += 1;
  }

  bool value = false;
  if (begin < end && m_TokenStream[begin].getTokenKind() == TokenKind::TRUE) {
    value = true;
  } else if (begin < end &&
             m_TokenStream[begin].getTokenKind() == TokenKind::FALSE) {
    value = false;
  } else if (begin + 2 < end &&
             m_TokenStream[begin].getTokenKind() == TokenKind::IDENT &&
             m_TokenStream[begin].getTokenAsStr() == "target" &&
             m_TokenStream[begin + 1].getTokenKind() == TokenKind::DOT &&
             m_TokenStream[begin + 2].getTokenKind() == TokenKind::IDENT) {
    const auto property = m_TokenStream[begin + 2].getTokenAsStr();
    std::string actual;
    if (property == "os") {
#if defined(_WIN32)
      actual = "windows";
#elif defined(__APPLE__)
      actual = "macos";
#elif defined(__linux__)
      actual = "linux";
#else
      actual = "unknown";
#endif
    } else if (property == "arch") {
#if defined(__aarch64__) || defined(_M_ARM64)
      actual = "arm64";
#elif defined(__x86_64__) || defined(_M_X64)
      actual = "x64";
#else
      actual = "unknown";
#endif
    } else {
      ParserError("Unknown target property '" + property + "'",
                  m_TokenStream[begin + 2]);
    }

    if (begin + 4 < end &&
        m_TokenStream[begin + 3].getTokenKind() == TokenKind::EQUALEQUAL &&
        m_TokenStream[begin + 4].getTokenKind() == TokenKind::LITERAL) {
      value = actual == m_TokenStream[begin + 4].getTokenAsStr();
    } else if (begin + 4 < end &&
               m_TokenStream[begin + 3].getTokenKind() ==
                   TokenKind::NOTEQUAL &&
               m_TokenStream[begin + 4].getTokenKind() == TokenKind::LITERAL) {
      value = actual != m_TokenStream[begin + 4].getTokenAsStr();
    } else {
      ParserError("Expected target comparison in #if", m_TokenStream[begin]);
    }
  } else if (begin + 3 < end &&
             m_TokenStream[begin].getTokenKind() == TokenKind::IDENT &&
             (m_TokenStream[begin].getTokenAsStr() == "feature" ||
              m_TokenStream[begin].getTokenAsStr() == "cfg") &&
             m_TokenStream[begin + 1].getTokenKind() == TokenKind::LPAREN &&
             m_TokenStream[begin + 2].getTokenKind() == TokenKind::LITERAL &&
             m_TokenStream[begin + 3].getTokenKind() == TokenKind::RPAREN) {
    value = false;
  } else {
    ParserError("Unsupported #if condition", m_TokenStream[begin]);
  }

  return negate ? !value : value;
}

size_t CStarParser::handleDirective(size_t index,
                                    std::vector<TokenInfo>& output) {
  auto hashToken = m_TokenStream[index];
  const size_t directiveIndex = index + 1;
  if (directiveIndex >= m_TokenStream.size()) {
    ParserError("Expected directive name after '#'", hashToken);
  }

  const auto directiveKind = m_TokenStream[directiveIndex].getTokenKind();
  const auto directiveName = m_TokenStream[directiveIndex].getTokenAsStr();
  if (directiveKind == TokenKind::IF) {
    size_t conditionEnd = directiveIndex + 1;
    while (conditionEnd < m_TokenStream.size() &&
           m_TokenStream[conditionEnd].getTokenKind() != TokenKind::LBRACK &&
           m_TokenStream[conditionEnd].getTokenKind() != TokenKind::_EOF) {
      conditionEnd += 1;
    }
    if (conditionEnd >= m_TokenStream.size() ||
        m_TokenStream[conditionEnd].getTokenKind() != TokenKind::LBRACK) {
      ParserError("Expected block after #if condition", hashToken);
    }

    const bool condition =
        evaluateDirectiveCondition(directiveIndex + 1, conditionEnd);
    const size_t ifEnd = skipBalancedInTokenStream(
        conditionEnd, TokenKind::LBRACK, TokenKind::RBRACK);
    if (ifEnd <= conditionEnd) {
      ParserError("Unterminated #if block", hashToken);
    }

    size_t elseStart = ifEnd;
    while (elseStart < m_TokenStream.size() &&
           m_TokenStream[elseStart].getTokenKind() == TokenKind::LINEFEED) {
      elseStart += 1;
    }
    size_t elseEnd = elseStart;
    bool hasElse = false;
    if (elseStart < m_TokenStream.size() &&
        m_TokenStream[elseStart].getTokenKind() == TokenKind::ELSE) {
      hasElse = true;
      elseStart += 1;
      while (elseStart < m_TokenStream.size() &&
             m_TokenStream[elseStart].getTokenKind() == TokenKind::LINEFEED) {
        elseStart += 1;
      }
      if (elseStart >= m_TokenStream.size() ||
          m_TokenStream[elseStart].getTokenKind() != TokenKind::LBRACK) {
        ParserError("Expected block after #else", hashToken);
      }
      elseEnd = skipBalancedInTokenStream(elseStart, TokenKind::LBRACK,
                                          TokenKind::RBRACK);
    }

    const size_t selectedOpen = condition ? conditionEnd : elseStart;
    const size_t selectedEnd = condition ? ifEnd : elseEnd;
    if (condition || hasElse) {
      for (size_t i = selectedOpen + 1; i + 1 < selectedEnd; ++i) {
        output.push_back(m_TokenStream[i]);
      }
      if (!output.empty() &&
          output.back().getTokenKind() != TokenKind::LINEFEED) {
        output.emplace_back(TokenKind::LINEFEED,
                            hashToken.getTokenPositionInfo(), "\n", false);
      }
    }
    return hasElse ? elseEnd : ifEnd;
  }

  size_t lineEnd = directiveIndex + 1;
  while (lineEnd < m_TokenStream.size() &&
         m_TokenStream[lineEnd].getTokenKind() != TokenKind::LINEFEED &&
         m_TokenStream[lineEnd].getTokenKind() != TokenKind::_EOF) {
    lineEnd += 1;
  }

  if (directiveKind == TokenKind::IDENT && directiveName == "warning") {
    std::string message = "compile-time warning";
    if (directiveIndex + 1 < lineEnd) {
      message = m_TokenStream[directiveIndex + 1].getTokenAsStr();
    }
    auto pos = hashToken.getTokenPositionInfo();
    ParserWarning(message, pos.begin, pos.end, pos.line);
  } else if (directiveKind == TokenKind::IDENT && directiveName == "error") {
    std::string message = "compile-time error";
    if (directiveIndex + 1 < lineEnd) {
      message = m_TokenStream[directiveIndex + 1].getTokenAsStr();
    }
    ParserError(message, hashToken);
  } else {
    ParserError("Unsupported compile-time directive '#" + directiveName + "'",
                hashToken);
  }

  if (lineEnd < m_TokenStream.size() &&
      m_TokenStream[lineEnd].getTokenKind() == TokenKind::LINEFEED) {
    output.push_back(m_TokenStream[lineEnd]);
    return lineEnd + 1;
  }
  return lineEnd;
}

bool CStarParser::tryExpandMacroCall(size_t& index,
                                     std::vector<TokenInfo>& output) {
  if (m_TokenStream[index].getTokenKind() != TokenKind::IDENT) {
    return false;
  }

  std::string macroName = m_TokenStream[index].getTokenAsStr();
  size_t callNameEnd = index;
  if (index + 3 < m_TokenStream.size() &&
      m_TokenStream[index + 1].getTokenKind() == TokenKind::DOT &&
      m_TokenStream[index + 2].getTokenKind() == TokenKind::IDENT &&
      m_TokenStream[index + 3].getTokenKind() == TokenKind::LPAREN) {
    macroName += "." + m_TokenStream[index + 2].getTokenAsStr();
    callNameEnd = index + 2;
  }

  auto macroIt = m_CompileTimeMacros.find(macroName);
  if (macroIt == m_CompileTimeMacros.end()) {
    return false;
  }

  if (callNameEnd + 1 >= m_TokenStream.size() ||
      m_TokenStream[callNameEnd + 1].getTokenKind() != TokenKind::LPAREN) {
    return false;
  }

  const auto callToken = m_TokenStream[index];
  size_t cursor = callNameEnd + 2;
  std::vector<std::vector<TokenInfo>> args;
  std::vector<TokenInfo> currentArg;
  size_t parenDepth = 0;
  size_t bracketDepth = 0;
  size_t squareDepth = 0;
  while (cursor < m_TokenStream.size()) {
    const auto kind = m_TokenStream[cursor].getTokenKind();
    if (kind == TokenKind::LPAREN) {
      parenDepth += 1;
    } else if (kind == TokenKind::RPAREN) {
      if (parenDepth == 0 && bracketDepth == 0 && squareDepth == 0) {
        if (!currentArg.empty() || !args.empty()) {
          args.push_back(std::move(currentArg));
        }
        break;
      }
      parenDepth -= 1;
    } else if (kind == TokenKind::LBRACK) {
      bracketDepth += 1;
    } else if (kind == TokenKind::RBRACK && bracketDepth > 0) {
      bracketDepth -= 1;
    } else if (kind == TokenKind::LSQPAR) {
      squareDepth += 1;
    } else if (kind == TokenKind::RSQPAR && squareDepth > 0) {
      squareDepth -= 1;
    } else if (kind == TokenKind::COMMA && parenDepth == 0 &&
               bracketDepth == 0 && squareDepth == 0) {
      args.push_back(std::move(currentArg));
      currentArg = {};
      cursor += 1;
      continue;
    }

    currentArg.push_back(m_TokenStream[cursor]);
    cursor += 1;
  }

  if (cursor >= m_TokenStream.size() ||
      m_TokenStream[cursor].getTokenKind() != TokenKind::RPAREN) {
    ParserError("Unterminated macro call", callToken);
  }

  const auto& macro = macroIt->second;
  if (args.size() != macro.params.size()) {
    ParserError("Macro '" + macro.name + "' expects " +
                    std::to_string(macro.params.size()) + " argument(s), got " +
                    std::to_string(args.size()),
                callToken);
  }

  for (size_t i = 0; i < macro.body.size(); ++i) {
    if (macro.body[i].getTokenKind() == TokenKind::DOLLAR &&
        i + 1 < macro.body.size() &&
        macro.body[i + 1].getTokenKind() == TokenKind::IDENT) {
      const auto paramName = macro.body[i + 1].getTokenAsStr();
      auto paramIt =
          std::find(macro.params.begin(), macro.params.end(), paramName);
      if (paramIt != macro.params.end()) {
        const auto paramIndex =
            static_cast<size_t>(std::distance(macro.params.begin(), paramIt));
        output.insert(output.end(), args[paramIndex].begin(),
                      args[paramIndex].end());
        i += 1;
        continue;
      }
    }
    output.push_back(macro.body[i]);
  }

  index = cursor + 1;
  return true;
}

void CStarParser::preprocessCompileTimeSurface() {
  for (size_t pass = 0; pass < 8; ++pass) {
    std::vector<TokenInfo> output;
    output.reserve(m_TokenStream.size());
    bool changed = false;

    for (size_t i = 0; i < m_TokenStream.size();) {
      if (m_TokenStream[i].getTokenKind() == TokenKind::ATTRIB) {
        i = copyAttributeDefinition(i, output);
        continue;
      }
      if (m_TokenStream[i].getTokenKind() == TokenKind::PUBLIC) {
        size_t cursor = i + 1;
        while (cursor < m_TokenStream.size() &&
               (m_TokenStream[cursor].getTokenKind() == TokenKind::LINEFEED ||
                m_TokenStream[cursor].getTokenKind() == TokenKind::COMMENT)) {
          cursor += 1;
        }
        if (cursor < m_TokenStream.size() &&
            m_TokenStream[cursor].getTokenKind() == TokenKind::MACRO) {
          i = collectMacroDefinition(cursor, output);
          changed = true;
          continue;
        }
      }
      if (m_TokenStream[i].getTokenKind() == TokenKind::MACRO) {
        i = collectMacroDefinition(i, output);
        changed = true;
        continue;
      }
      if (m_TokenStream[i].getTokenKind() == TokenKind::HASH) {
        if (i + 1 < m_TokenStream.size() &&
            m_TokenStream[i + 1].getTokenKind() == TokenKind::LSQPAR) {
          output.push_back(m_TokenStream[i]);
          i += 1;
          continue;
        }
        i = handleDirective(i, output);
        changed = true;
        continue;
      }
      if (tryExpandMacroCall(i, output)) {
        changed = true;
        continue;
      }

      output.push_back(m_TokenStream[i]);
      i += 1;
    }

    m_TokenStream = std::move(output);
    if (!changed) {
      break;
    }
  }
}
