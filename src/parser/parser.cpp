#include <parser/parser.hpp>
#include <cstar_config.hpp>

#include <algorithm>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <filesystem>
#include <fstream>
#include <limits>
#include <set>
#include <sstream>
#include <string>

static bool IsProposalOnlyTopLevel(TokenKind kind) {
  switch (kind) {
    case DYNAMIC:
    case PROTOCOL:
    case STATE:
    case DYN:
    case CONSTRUCTOR:
    case DESTRUCTOR:
    case ALLOCATOR:
    case EXCEPT:
    case THROW:
    case DEFER:
    case ASYNC:
    case AWAIT:
    case SELF:
    case IS:
    case PROTO:
      return true;
    default:
      return false;
  }
}

static bool IsMacroParamKindName(const std::string& name) {
  return name == "expr" || name == "stmt" || name == "item" ||
         name == "type" || name == "ident" || name == "tokens";
}

static bool IsMacroReturnKindName(const std::string& name) {
  return name == "expr" || name == "stmt" || name == "item" ||
         name == "type";
}

static std::filesystem::path ResolveLogicalIncludeSource(
    const std::string& includeName) {
  if (includeName.size() >= 6 &&
      includeName.substr(includeName.size() - 6) == ".cstar") {
    return std::filesystem::path(includeName);
  }

  constexpr std::string_view kStdPrefix = "std:";
  if (includeName.rfind(std::string(kStdPrefix), 0) == 0) {
    auto logical = includeName.substr(kStdPrefix.size());
    const auto memberSeparator = logical.find(':');
    if (memberSeparator != std::string::npos) {
      logical = logical.substr(0, memberSeparator);
    }
    if (logical.empty()) {
      logical = "core";
    }
    return std::filesystem::path("std") / (logical + ".cstar");
  }

  return {};
}

static bool IsStdSourceIncludePath(const std::filesystem::path& path) {
  return path.begin() != path.end() && *path.begin() == "std";
}

static std::filesystem::path ResolveConfiguredStdlibRoot() {
  if (const char* stdlibPath = std::getenv("CSTAR_STDLIB_PATH")) {
    if (stdlibPath[0] != '\0') {
      return std::filesystem::path(stdlibPath);
    }
  }

  if (const char* sourceRoot = std::getenv("CSTAR_SOURCE_ROOT")) {
    if (sourceRoot[0] != '\0') {
      return std::filesystem::path(sourceRoot) / "std";
    }
  }

  return std::filesystem::path(CSTAR_SOURCE_ROOT) / "std";
}

static std::filesystem::path ResolveSourceIncludePath(
    const std::filesystem::path& includePath,
    const std::filesystem::path& includingFile) {
  if (!includePath.is_relative()) {
    return std::filesystem::absolute(includePath).lexically_normal();
  }

  std::filesystem::path resolved;
  if (IsStdSourceIncludePath(includePath)) {
    auto relativeInsideStd = includePath;
    relativeInsideStd = relativeInsideStd.lexically_relative("std");
    resolved = ResolveConfiguredStdlibRoot() / relativeInsideStd;
  } else {
    resolved = includingFile.parent_path() / includePath;
  }

  return std::filesystem::absolute(resolved).lexically_normal();
}

void CStarParser::parse() {
  m_TokenStream = this->m_Lexer.perform();
  collectPublicMacrosFromSourceIncludes();
  preprocessCompileTimeSurface();
  if (m_StatsEnabled) {
    this->m_Lexer.lexerStats();
  }
  this->m_StartTime = time(nullptr);

  if (!this->m_TokenStream.empty()) {
    // auto eof = std::move(m_TokenStream[-1]);
    this->m_CurrToken = m_TokenStream[m_TokenIndex];  //[0]
    this->translationUnit();
  } else {
    assert(false && "Parser token stream is not enough to parse!\n");
  }

  /*for(auto &node: this->m_AST) {
    node->debugNode();
  }*/

  this->parserStats();
}

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

void CStarParser::registerNativeLinkLibrary(const std::string& library) {
  if (library.empty()) {
    return;
  }

  if (std::find(m_NativeLinkLibraries.begin(), m_NativeLinkLibraries.end(),
                library) == m_NativeLinkLibraries.end()) {
    m_NativeLinkLibraries.push_back(library);
  }
}

void CStarParser::skipBalanced(TokenKind open, TokenKind close) {
  expected(open);
  size_t depth = 0;
  do {
    if (is(open)) {
      depth += 1;
    } else if (is(close)) {
      if (depth == 0) {
        ParserError("Unexpected closing token while parsing balanced block",
                    currentTokenInfo());
      }
      depth -= 1;
    }

    this->advance();
  } while (depth > 0 && !is(TokenKind::_EOF));

  if (depth != 0) {
    ParserError("Unterminated balanced block", prevTokenInfo());
  }
}

void CStarParser::parseAttributeDecl(
    DeclarationModifiers declarationModifiers) {
  if (declarationModifiers.linkage == VisibilitySpecifier::VIS_IMPORT ||
      declarationModifiers.linkage == VisibilitySpecifier::VIS_EXPORT) {
    ParserError("attribute is a compile-time transform, not import/export "
                "linkage",
                currentTokenInfo());
  }
  if (declarationModifiers.isStatic) {
    ParserError("static attribute declarations are not valid",
                currentTokenInfo());
  }

  auto attributeToken = currentTokenInfo();
  auto posInfo = attributeToken.getTokenPositionInfo();
  const size_t begin = posInfo.begin;
  const size_t line = posInfo.line;

  this->advance();
  expected(TokenKind::IDENT);
  const auto attributeName = currentTokenStr();
  if (m_AttributeDefinitions.count(attributeName) != 0) {
    ParserError("Redefinition of attribute '" + attributeName + "'",
                currentTokenInfo());
  }
  m_AttributeDefinitions.insert(attributeName);
  this->advance();

  expected(TokenKind::FOR);
  this->advance();
  skipTopLevelTrivia();

  if (!is(TokenKind::STRUCT)) {
    ParserError("attribute MVP only supports `for struct`",
                currentTokenInfo());
  }
  this->advance();
  skipTopLevelTrivia();

  expected(TokenKind::LBRACK);
  skipBalanced(TokenKind::LBRACK, TokenKind::RBRACK);
  posInfo = prevTokenInfo().getTokenPositionInfo();
  SemanticLoc semLoc(begin, posInfo.end, line);

  this->m_AST.emplace_back(std::make_unique<AttributeAST>(
      attributeName, AttributeTargetKind::Struct, semLoc));
}

void CStarParser::parseAttributeAnnotation() {
  auto atToken = currentTokenInfo();
  this->advance();
  expected(TokenKind::IDENT);
  const auto attributeName = currentTokenStr();
  if (m_AttributeDefinitions.count(attributeName) == 0) {
    ParserError("Unknown attribute '" + attributeName + "'",
                currentTokenInfo());
  }
  this->advance();

  if (is(TokenKind::LPAREN)) {
    skipBalanced(TokenKind::LPAREN, TokenKind::RPAREN);
  }

  (void)atToken;
  (void)attributeName;
}

MacroParamKind CStarParser::parseMacroParamKind() {
  expected(TokenKind::IDENT);
  const auto kindName = currentTokenStr();
  if (kindName == "expr") {
    this->advance();
    return MacroParamKind::Expr;
  }
  if (kindName == "stmt") {
    this->advance();
    return MacroParamKind::Stmt;
  }
  if (kindName == "item") {
    this->advance();
    return MacroParamKind::Item;
  }
  if (kindName == "type") {
    this->advance();
    return MacroParamKind::Type;
  }
  if (kindName == "ident") {
    this->advance();
    return MacroParamKind::Ident;
  }
  if (kindName == "tokens") {
    this->advance();
    return MacroParamKind::Tokens;
  }

  ParserError("Unknown macro parameter kind '" + kindName +
                  "'. Expected expr, stmt, item, type, ident or tokens",
              currentTokenInfo());
  return MacroParamKind::Tokens;
}

MacroReturnKind CStarParser::parseMacroReturnKind() {
  expected(TokenKind::IDENT);
  const auto kindName = currentTokenStr();
  if (kindName == "expr") {
    this->advance();
    return MacroReturnKind::Expr;
  }
  if (kindName == "stmt") {
    this->advance();
    return MacroReturnKind::Stmt;
  }
  if (kindName == "item") {
    this->advance();
    return MacroReturnKind::Item;
  }
  if (kindName == "type") {
    this->advance();
    return MacroReturnKind::Type;
  }

  ParserError("Unknown macro return kind '" + kindName +
                  "'. Expected expr, stmt, item or type",
              currentTokenInfo());
  return MacroReturnKind::Expr;
}

void CStarParser::parseMacroDecl(
    DeclarationModifiers declarationModifiers) {
  if (declarationModifiers.linkage == VisibilitySpecifier::VIS_IMPORT ||
      declarationModifiers.linkage == VisibilitySpecifier::VIS_EXPORT) {
    ParserError("macro is a compile-time transform, not import/export linkage",
                currentTokenInfo());
  }
  if (declarationModifiers.isStatic) {
    ParserError("static macro declarations are not valid",
                currentTokenInfo());
  }

  auto macroToken = currentTokenInfo();
  auto posInfo = macroToken.getTokenPositionInfo();
  const size_t begin = posInfo.begin;
  const size_t line = posInfo.line;

  this->advance();
  expected(TokenKind::IDENT);
  const auto macroName = currentTokenStr();
  this->advance();

  expected(TokenKind::LPAREN);
  this->advance();

  std::vector<MacroParamInfo> params;
  while (!is(TokenKind::RPAREN) && !is(TokenKind::_EOF)) {
    skipTopLevelTrivia();
    if (is(TokenKind::RPAREN)) {
      break;
    }

    expected(TokenKind::DOLLAR);
    this->advance();
    expected(TokenKind::IDENT);
    MacroParamInfo param;
    param.name = currentTokenStr();
    this->advance();
    expected(TokenKind::COLON);
    this->advance();
    param.kind = parseMacroParamKind();
    params.push_back(std::move(param));

    skipTopLevelTrivia();
    if (is(TokenKind::COMMA)) {
      this->advance();
      continue;
    }
    if (!is(TokenKind::RPAREN)) {
      expected({TokenKind::COMMA, TokenKind::RPAREN});
    }
  }

  expected(TokenKind::RPAREN);
  this->advance();
  skipTopLevelTrivia();
  expected(TokenKind::ARROW);
  this->advance();
  skipTopLevelTrivia();
  const auto returnKind = parseMacroReturnKind();
  skipTopLevelTrivia();

  expected(TokenKind::LBRACK);
  skipBalanced(TokenKind::LBRACK, TokenKind::RBRACK);
  posInfo = prevTokenInfo().getTokenPositionInfo();
  SemanticLoc semLoc(begin, posInfo.end, line);

  this->m_AST.emplace_back(std::make_unique<MacroAST>(
      macroName, std::move(params), returnKind, semLoc));

  ParserHint(
      "Macro declarations are parsed as typed C* metaprogramming surface, "
      "but hygienic expansion/source mapping is not implemented yet.",
      macroToken);
  ParserError("macro expansion is proposal-only in this compiler phase",
              macroToken);
}

void CStarParser::parseDirective() {
  auto hashToken = currentTokenInfo();
  auto posInfo = hashToken.getTokenPositionInfo();
  this->advance();

  std::string directiveName;
  if (is(TokenKind::IF)) {
    directiveName = "if";
  } else if (is(TokenKind::IDENT)) {
    directiveName = currentTokenStr();
  } else {
    ParserError("Expected directive name after '#'", currentTokenInfo());
  }

  SemanticLoc semLoc(posInfo.begin,
                     currentTokenInfo().getTokenPositionInfo().end,
                     posInfo.line);
  this->m_AST.emplace_back(
      std::make_unique<DirectiveAST>(directiveName, semLoc));

  ParserHint(
      "Directive syntax is parsed as C* compile-time config surface, "
      "but directive evaluation is not implemented yet.",
      hashToken);
  ParserError("directive '#" + directiveName +
                  "' is proposal-only in this compiler phase",
              hashToken);
}

std::string CStarParser::parseLinkSource() {
  expected(TokenKind::FROM);
  this->advance();
  skipTopLevelTrivia();

  expected(TokenKind::LITERAL);
  auto source = currentTokenStr();
  this->advance();
  return source;
}

void CStarParser::parseLinkageBlock(
    VisibilitySpecifier visibilitySpecifier) {
  std::string library;
  if (is(TokenKind::FROM)) {
    library = parseLinkSource();
    registerNativeLinkLibrary(library);
    skipTopLevelTrivia();
  }

  expected(TokenKind::LBRACK);
  this->advance();

  while (!is(TokenKind::RBRACK)) {
    skipTopLevelTrivia();
    if (is(TokenKind::RBRACK)) {
      break;
    }
    expected(TokenKind::IDENT);
    DeclarationModifiers modifiers;
    modifiers.linkage = visibilitySpecifier;
    funcDecl(modifiers, true);
    if (!library.empty()) {
      registerNativeLinkLibrary(library);
    }
    skipTopLevelTrivia();
  }

  expected(TokenKind::RBRACK);
  this->advance();
  skipTopLevelTrivia();
  if (is(TokenKind::SEMICOLON)) {
    this->advance();
  }
}

StructFieldInfo CStarParser::parseStructField(
    DeclarationModifiers modifiers) {
  if (modifiers.isStatic) {
    ParserError("static data members are not part of the current struct model; "
                "use module-level static state",
                prevTokenInfo());
  }

  StructFieldInfo field;
  field.isPublic = modifiers.access == ACCESS_PUBLIC;

  if (!isType(currentTokenInfo()) && !is(TokenKind::IDENT)) {
    ParserError("Expected field type in struct declaration",
                currentTokenInfo());
  }

  if (is(TokenKind::IDENT)) {
    field.type = TypeSpecifier::SPEC_DEFINED;
    field.definedTypeName = advanceDefinedTypeName();
  } else {
    field.type = typeSpecifierOf(currentTokenInfo());
    this->advance();
  }

  while (is(TokenKind::STAR) || is(TokenKind::XOR)) {
    const bool currentPointerIsUnique =
        this->currentTokenKind() == TokenKind::XOR;
    field.indirectionLevel = advancePointerType(currentPointerIsUnique);
    if (currentPointerIsUnique) {
      field.isUnique = true;
    }
  }

  if (is(TokenKind::AND)) {
    field.indirectionLevel = 1;
    field.isRef = true;
    this->advance();
  }

  expected(TokenKind::IDENT);
  field.name = currentTokenStr();
  this->advance();

  if (is(TokenKind::EQUAL) || is(TokenKind::TYPEINF)) {
    ParserError("struct field initializers are not implemented yet",
                currentTokenInfo());
  }

  expected(TokenKind::SEMICOLON);
  this->advance();
  return field;
}

TraitRequirementInfo CStarParser::parseTraitRequirement() {
  TraitRequirementInfo requirement;

  if (is(TokenKind::OPERATOR)) {
    ParserError(
        "operator requirements are not implemented yet; lifecycle allocation "
        "operators are compiler-reserved",
        currentTokenInfo());
  }

  expected(TokenKind::IDENT);
  requirement.name = currentTokenStr();
  this->advance();

  if (is(TokenKind::LPAREN)) {
    size_t depth = 0;
    do {
      if (is(TokenKind::LPAREN)) {
        depth += 1;
      } else if (is(TokenKind::RPAREN)) {
        if (depth == 0) {
          ParserError("Unexpected ')' in trait requirement",
                      currentTokenInfo());
        }
        depth -= 1;
      }
      this->advance();
    } while (depth > 0 && !is(TokenKind::_EOF));
  }

  if (is(TokenKind::COLONCOLON)) {
    this->advance();
    if (!isType(currentTokenInfo()) && !is(TokenKind::IDENT) &&
        !is(TokenKind::VOID)) {
      ParserError("Expected return type in trait requirement",
                  currentTokenInfo());
    }
    this->advance();
    while (is(TokenKind::STAR) || is(TokenKind::XOR) ||
           is(TokenKind::AND)) {
      this->advance();
    }
  }

  expected(TokenKind::SEMICOLON);
  this->advance();
  return requirement;
}

void CStarParser::parseTraitDecl(DeclarationModifiers declarationModifiers) {
  if (declarationModifiers.linkage == VisibilitySpecifier::VIS_IMPORT ||
      declarationModifiers.linkage == VisibilitySpecifier::VIS_EXPORT) {
    ParserError("trait visibility uses public/private, not import/export",
                currentTokenInfo());
  }
  if (declarationModifiers.isStatic) {
    ParserError("static trait declarations are not valid", currentTokenInfo());
  }

  auto posInfo = currentTokenInfo().getTokenPositionInfo();
  size_t begin = posInfo.begin;
  size_t line = posInfo.line;

  this->advance();
  expected(TokenKind::IDENT);
  auto traitName = currentTokenStr();
  this->advance();

  if (is(TokenKind::LT)) {
    ParserError("generic traits are proposal-only in this compiler phase",
                currentTokenInfo());
  }

  expected(TokenKind::LBRACK);
  this->advance();

  std::vector<TraitRequirementInfo> requirements;
  while (!is(TokenKind::RBRACK) && !is(TokenKind::_EOF)) {
    skipTopLevelTrivia();
    if (is(TokenKind::RBRACK)) {
      break;
    }
    requirements.push_back(parseTraitRequirement());
  }

  expected(TokenKind::RBRACK);
  posInfo = currentTokenInfo().getTokenPositionInfo();
  SemanticLoc semLoc(begin, posInfo.end, line);
  this->advance();

  this->m_AST.emplace_back(std::make_unique<TraitAST>(
      traitName, std::move(requirements), declarationModifiers.access, semLoc));

  skipTopLevelTrivia();
  if (is(TokenKind::SEMICOLON)) {
    this->advance();
  }
}

static bool IsEnumUnderlyingType(TypeSpecifier type) {
  switch (type) {
    case TypeSpecifier::SPEC_I8:
    case TypeSpecifier::SPEC_I16:
    case TypeSpecifier::SPEC_I32:
    case TypeSpecifier::SPEC_I64:
    case TypeSpecifier::SPEC_INT:
    case TypeSpecifier::SPEC_U8:
    case TypeSpecifier::SPEC_U16:
    case TypeSpecifier::SPEC_U32:
    case TypeSpecifier::SPEC_U64:
    case TypeSpecifier::SPEC_UINT:
    case TypeSpecifier::SPEC_ISIZE:
    case TypeSpecifier::SPEC_USIZE:
      return true;
    default:
      return false;
  }
}

static uint64_t EnumUnderlyingMax(TypeSpecifier type) {
  switch (type) {
    case TypeSpecifier::SPEC_I8:
      return static_cast<uint64_t>(std::numeric_limits<int8_t>::max());
    case TypeSpecifier::SPEC_I16:
      return static_cast<uint64_t>(std::numeric_limits<int16_t>::max());
    case TypeSpecifier::SPEC_I32:
      return static_cast<uint64_t>(std::numeric_limits<int32_t>::max());
    case TypeSpecifier::SPEC_I64:
    case TypeSpecifier::SPEC_INT:
    case TypeSpecifier::SPEC_ISIZE:
      return static_cast<uint64_t>(std::numeric_limits<int64_t>::max());
    case TypeSpecifier::SPEC_U8:
      return std::numeric_limits<uint8_t>::max();
    case TypeSpecifier::SPEC_U16:
      return std::numeric_limits<uint16_t>::max();
    case TypeSpecifier::SPEC_U32:
      return std::numeric_limits<uint32_t>::max();
    case TypeSpecifier::SPEC_U64:
    case TypeSpecifier::SPEC_U128:
    case TypeSpecifier::SPEC_UINT:
    case TypeSpecifier::SPEC_USIZE:
      return std::numeric_limits<uint64_t>::max();
    default:
      return 0;
  }
}

static bool IsZeroOrPowerOfTwo(uint64_t value) {
  return value == 0 || (value & (value - 1)) == 0;
}

void CStarParser::parseEnumDecl(DeclarationModifiers declarationModifiers,
                                bool isFlags) {
  if (declarationModifiers.linkage == VisibilitySpecifier::VIS_IMPORT ||
      declarationModifiers.linkage == VisibilitySpecifier::VIS_EXPORT) {
    ParserError("enum visibility uses public/private, not import/export",
                currentTokenInfo());
  }
  if (declarationModifiers.isStatic) {
    ParserError("static enum declarations are not valid", currentTokenInfo());
  }

  auto posInfo = currentTokenInfo().getTokenPositionInfo();
  size_t begin = posInfo.begin;
  size_t line = posInfo.line;

  if (isFlags) {
    expected(TokenKind::FLAGS);
    this->advance();
    skipTopLevelTrivia();
    expected(TokenKind::ENUM);
  }

  this->advance();
  expected(TokenKind::IDENT);
  auto enumName = currentTokenStr();
  this->advance();

  expected(TokenKind::COLON);
  this->advance();
  if (!isType(currentTokenInfo())) {
    ParserError("Expected enum underlying integer type", currentTokenInfo());
  }

  auto underlyingType = typeSpecifierOf(currentTokenInfo());
  if (!IsEnumUnderlyingType(underlyingType)) {
    ParserError("Enum underlying type must be an integer type",
                currentTokenInfo());
  }
  const auto maxValue = EnumUnderlyingMax(underlyingType);
  this->advance();

  expected(TokenKind::LBRACK);
  this->advance();

  uint64_t nextValue = 0;
  bool implicitValueOverflow = false;
  std::set<std::string> memberNames;
  std::set<uint64_t> memberValues;
  std::vector<EnumMemberInfo> members;
  while (!is(TokenKind::RBRACK) && !is(TokenKind::_EOF)) {
    skipTopLevelTrivia();
    if (is(TokenKind::RBRACK)) {
      break;
    }

    expected(TokenKind::IDENT);
    EnumMemberInfo member;
    member.name = currentTokenStr();
    member.value = nextValue;
    if (memberNames.count(member.name) != 0) {
      ParserError("Redefinition of enum member '" + member.name + "'",
                  currentTokenInfo());
    }
    memberNames.insert(member.name);
    this->advance();

    if (is(TokenKind::EQUAL)) {
      this->advance();
      expected(TokenKind::SCALARI);
      try {
        member.value = std::stoull(currentTokenStr());
      } catch (const std::exception&) {
        ParserError("Enum member value '" + currentTokenStr() +
                        "' is not a valid unsigned integer literal",
                    currentTokenInfo());
      }
      this->advance();
    } else if (isFlags) {
      ParserError("Flags enum member '" + member.name +
                      "' must declare an explicit bit value",
                  prevTokenInfo());
    } else if (implicitValueOverflow) {
      ParserError("Implicit enum member value for '" + member.name +
                      "' overflows the enum underlying type",
                  prevTokenInfo());
    }

    if (member.value > maxValue) {
      ParserError("Enum member '" + member.name + "' value '" +
                      std::to_string(member.value) +
                      "' overflows the enum underlying type",
                  prevTokenInfo());
    }

    if (isFlags && !IsZeroOrPowerOfTwo(member.value)) {
      ParserError("Flags enum member '" + member.name +
                      "' must be 0 or a power-of-two bit value",
                  prevTokenInfo());
    }

    if (memberValues.count(member.value) != 0) {
      ParserError("Duplicate enum member value '" +
                      std::to_string(member.value) + "' in enum '" +
                      enumName + "'",
                  prevTokenInfo());
    }
    memberValues.insert(member.value);

    members.push_back(std::move(member));
    if (members.back().value == maxValue) {
      implicitValueOverflow = true;
    } else {
      nextValue = members.back().value + 1;
      implicitValueOverflow = false;
    }
    skipTopLevelTrivia();

    if (is(TokenKind::COMMA)) {
      this->advance();
      continue;
    }

    if (!is(TokenKind::RBRACK)) {
      expected({TokenKind::COMMA, TokenKind::RBRACK});
    }
  }

  expected(TokenKind::RBRACK);
  posInfo = currentTokenInfo().getTokenPositionInfo();
  SemanticLoc semLoc(begin, posInfo.end, line);
  this->advance();

  if (members.empty()) {
    ParserError("Enum must declare at least one member", currentTokenInfo());
  }

  this->m_AST.emplace_back(std::make_unique<EnumAST>(
      enumName, underlyingType, std::move(members),
      declarationModifiers.access, semLoc, isFlags));

  skipTopLevelTrivia();
  if (is(TokenKind::SEMICOLON)) {
    this->advance();
  }
}

void CStarParser::parseStructDecl(DeclarationModifiers declarationModifiers) {
  if (declarationModifiers.linkage == VisibilitySpecifier::VIS_IMPORT ||
      declarationModifiers.linkage == VisibilitySpecifier::VIS_EXPORT) {
    ParserError("struct visibility uses public/private, not import/export",
                currentTokenInfo());
  }
  if (declarationModifiers.isStatic) {
    ParserError("static struct declarations are not implemented yet",
                currentTokenInfo());
  }

  auto posInfo = currentTokenInfo().getTokenPositionInfo();
  size_t begin = posInfo.begin;
  size_t line = posInfo.line;

  this->advance();
  expected(TokenKind::IDENT);
  auto structName = currentTokenStr();
  this->advance();

  if (is(TokenKind::LT) || is(TokenKind::FROM)) {
    ParserError(
        "generic/attribute struct syntax is proposal-only; use "
        "`struct Name { field; ... }` for the current MVP",
        currentTokenInfo());
  }

  std::vector<std::string> traits;
  if (is(TokenKind::WITH)) {
    this->advance();
    do {
      skipTopLevelTrivia();
      expected(TokenKind::IDENT);
      traits.push_back(advanceDefinedTypeName());
      skipTopLevelTrivia();
      if (!is(TokenKind::COMMA)) {
        break;
      }
      this->advance();
    } while (!is(TokenKind::_EOF));
  }

  expected(TokenKind::LBRACK);
  this->advance();

  std::vector<StructFieldInfo> fields;
  std::vector<ASTNode> methods;
  while (!is(TokenKind::RBRACK) && !is(TokenKind::_EOF)) {
    skipTopLevelTrivia();
    if (is(TokenKind::RBRACK)) {
      break;
    }

    DeclarationModifiers memberModifiers = parseDeclarationModifiers(false);

    if (is(TokenKind::ALLOCATOR)) {
      ParserError("struct allocator hooks require the allocator/trait phase",
                  currentTokenInfo());
    }

    if (is(TokenKind::OPERATOR)) {
      const auto astSizeBeforeMethod = m_AST.size();
      funcDecl(memberModifiers, false, structName);
      while (m_AST.size() > astSizeBeforeMethod) {
        methods.push_back(std::move(m_AST.back()));
        m_AST.pop_back();
      }
      continue;
    }

    bool outOfSize = false;
    auto nextToken = nextTokenInfo(outOfSize).getTokenKind();
    if (!outOfSize &&
        (is(TokenKind::IDENT) || is(TokenKind::CONSTRUCTOR) ||
         is(TokenKind::DESTRUCTOR) || is(TokenKind::NEW) ||
         is(TokenKind::OPERATOR)) &&
        (nextToken == TokenKind::LPAREN ||
         nextToken == TokenKind::COLONCOLON ||
         nextToken == TokenKind::LBRACK)) {
      const auto astSizeBeforeMethod = m_AST.size();
      funcDecl(memberModifiers, false, structName);
      while (m_AST.size() > astSizeBeforeMethod) {
        methods.push_back(std::move(m_AST.back()));
        m_AST.pop_back();
      }
      continue;
    }

    fields.push_back(parseStructField(memberModifiers));
  }

  expected(TokenKind::RBRACK);
  posInfo = currentTokenInfo().getTokenPositionInfo();
  SemanticLoc semLoc(begin, posInfo.end, line);
  this->advance();

  auto structAst =
      std::make_unique<StructAST>(structName, std::move(fields),
                                  std::move(traits),
                                  declarationModifiers.access, semLoc);
  this->m_AST.emplace_back(std::move(structAst));
  for (auto &method : methods) {
    this->m_AST.emplace_back(std::move(method));
  }

  skipTopLevelTrivia();
  if (is(TokenKind::SEMICOLON)) {
    this->advance();
  }
}

void CStarParser::parseIncludeDirective() {
  auto includeToken = currentTokenInfo();
  this->advance();
  skipTopLevelTrivia();

  if (is(TokenKind::INVOLVED)) {
    this->advance();
    skipTopLevelTrivia();
    expected(TokenKind::LBRACK);
    this->advance();
    while (!is(TokenKind::RBRACK)) {
      skipTopLevelTrivia();
      if (is(TokenKind::LITERAL) || is(TokenKind::LETTER) ||
          is(TokenKind::IDENT)) {
        const auto includeName = currentTokenStr();
        auto sourceInclude = ResolveLogicalIncludeSource(includeName);
        if (!sourceInclude.empty()) {
          m_SourceIncludes.emplace_back(std::move(sourceInclude));
        }
        this->advance();
      } else if (!is(TokenKind::RBRACK)) {
        ParserError("Expected include target", currentTokenInfo());
      }

      skipTopLevelTrivia();
      if (is(TokenKind::COMMA)) {
        this->advance();
      }
    }
    this->advance();
    skipTopLevelTrivia();
    if (is(TokenKind::SEMICOLON)) {
      this->advance();
    }
    return;
  }

  if (is(TokenKind::LBRACK)) {
    this->advance();
    while (!is(TokenKind::RBRACK)) {
      skipTopLevelTrivia();
      if (is(TokenKind::LITERAL) || is(TokenKind::LETTER)) {
        auto includeName = currentTokenStr();
        auto sourceInclude = ResolveLogicalIncludeSource(includeName);
        if (!sourceInclude.empty()) {
          m_SourceIncludes.emplace_back(std::move(sourceInclude));
        }
        this->advance();
      } else if (!is(TokenKind::RBRACK)) {
        ParserError("Expected include target", currentTokenInfo());
      }

      skipTopLevelTrivia();
      if (is(TokenKind::COMMA)) {
        this->advance();
      }
    }
    this->advance();
    skipTopLevelTrivia();
    if (is(TokenKind::SEMICOLON)) {
      this->advance();
    }
    return;
  }

  if (is(TokenKind::LITERAL) || is(TokenKind::LETTER)) {
    auto includeName = currentTokenStr();
    auto sourceInclude = ResolveLogicalIncludeSource(includeName);
    if (!sourceInclude.empty()) {
      m_SourceIncludes.emplace_back(std::move(sourceInclude));
    }
    this->advance();
    skipTopLevelTrivia();
    if (is(TokenKind::AS)) {
      this->advance();
      skipTopLevelTrivia();
      expected(TokenKind::IDENT);
      m_ModuleAliases.push_back(currentTokenStr());
      this->advance();
    }
    skipTopLevelTrivia();
    if (is(TokenKind::SEMICOLON)) {
      this->advance();
    }
    return;
  }

  ParserError("Expected include target after 'include'", includeToken);
}

void CStarParser::translationUnit() {
  while (!this->m_ParsingEndingFlag) {
    // std::cout << this->m_CurrToken.getTokenAsStr() << "\n";
    if (this->m_CurrToken.getTokenKind() == TokenKind::_EOF) {
      //      std::cout << "EOF\n";
      break;
    }

    if (is(TokenKind::UNHANDLED)) {
      auto t = this->currentTokenInfo();
      // TODO: delete later on
      assert(false && "Unhandled");
    }

    if (is(TokenKind::COMMENT) || is(TokenKind::LINEFEED)) {
      this->advance();
      continue;
    }

    if (is(TokenKind::AT)) {
      parseAttributeAnnotation();
      continue;
    }

    if (is(TokenKind::HASH)) {
      parseDirective();
      continue;
    }

    if (isPackageMark(this->m_CurrToken)) {
      parseIncludeDirective();
      continue;
    } else if (IsProposalOnlyTopLevel(currentTokenKind())) {
      ParserHint(
          "This keyword belongs to the C* proposal surface "
          "(struct/trait/protocol/attribute/macro/effects), but its "
          "grammar is not implemented in the compiler yet.",
          currentTokenInfo());
      ParserError("Unexpected proposal keyword '" +
                      std::string(tokenToStr(currentTokenKind())) + "'",
                  currentTokenInfo());
    } else {
      DeclarationModifiers declarationModifiers =
          parseDeclarationModifiers(false);

      skipTopLevelTrivia();
      if (is(TokenKind::ATTRIB)) {
        parseAttributeDecl(declarationModifiers);
        continue;
      }

      if (is(TokenKind::MACRO)) {
        parseMacroDecl(declarationModifiers);
        continue;
      }

      if (is(TokenKind::STRUCT)) {
        parseStructDecl(declarationModifiers);
        continue;
      }

      if (is(TokenKind::TRAIT)) {
        parseTraitDecl(declarationModifiers);
        continue;
      }

      if (is(TokenKind::FLAGS)) {
        parseEnumDecl(declarationModifiers, true);
        continue;
      }

      if (is(TokenKind::ENUM)) {
        parseEnumDecl(declarationModifiers);
        continue;
      }

      if ((declarationModifiers.linkage == VisibilitySpecifier::VIS_IMPORT ||
           declarationModifiers.linkage == VisibilitySpecifier::VIS_EXPORT) &&
          (is(TokenKind::LBRACK) || is(TokenKind::FROM))) {
        parseLinkageBlock(declarationModifiers.linkage);
        continue;
      }

      bool outOfSize = false;
      auto nextToken = nextTokenInfo(outOfSize).getTokenKind();
      if (outOfSize) {
        ParserError("Unexpected token", currentTokenInfo());
      }

      if (is(TokenKind::IDENT) && nextToken == TokenKind::LT) {
        bool genericFunc = false;
        size_t lookahead = 2;
        while (m_TokenIndex + lookahead < m_TokenStream.size()) {
          auto kind = m_TokenStream[m_TokenIndex + lookahead].getTokenKind();
          if (kind == TokenKind::GT) {
            if (m_TokenIndex + lookahead + 1 < m_TokenStream.size() &&
                m_TokenStream[m_TokenIndex + lookahead + 1].getTokenKind() ==
                    TokenKind::LPAREN) {
              genericFunc = true;
            }
            break;
          }
          if (kind == TokenKind::SEMICOLON || kind == TokenKind::LINEFEED ||
              kind == TokenKind::_EOF) {
            break;
          }
          lookahead += 1;
        }
        if (genericFunc) {
          nextToken = TokenKind::LPAREN;
        }
      }

      TypeQualifier typeQualifier = TypeQualifier::Q_NONE;

      if (isTypeQualifier(currentTokenInfo())) {
        switch (currentTokenKind()) {
          case CONST:
            typeQualifier = TypeQualifier::Q_CONST;
            break;
          case CONSTPTR:
            typeQualifier = TypeQualifier::Q_CONSTPTR;
            break;
          case CONSTREF:
            typeQualifier = TypeQualifier::Q_CONSTREF;
            break;
          case READONLY:
            typeQualifier = TypeQualifier::Q_READONLY;
            break;
          default:
            assert(false && "Unreacheable");
        }
        this->advance();
      }

      // int* | float* | uint* ...
      // we check that is an IDENT or not since because isType for operators
      // which only contains primitives. IDENT means it's a symbol (which needed
      // to resolved in next phase - Semantic Analysis- )
      if (this->isType(this->m_CurrToken) || is(TokenKind::IDENT)) {
        if (is(TokenKind::IDENT) && nextToken != TokenKind::LPAREN) {
          varDecl(typeQualifier, declarationModifiers, is(TokenKind::IDENT),
                  false);
        } else if (is(TokenKind::IDENT) && nextToken == TokenKind::LPAREN) {
          funcDecl(declarationModifiers);
        } else {
          // if it's pointer type or something...
          varDecl(typeQualifier, declarationModifiers, is(TokenKind::IDENT),
                  false);
        }
      } else {
        /*if(is(TokenKind::PROTO)) {
          // advance 'proto'
          this->advance();


        }*/
        // protototype, directive, traits, macro
        ParserHint(
            "Not implemented yet (protototype, directive, traits, macro)",
            currentTokenInfo());
        ParserError("Unexpected token '" +
                        std::string(tokenToStr(currentTokenKind())) + "'",
                    currentTokenInfo());
      }
    }
  }
}

bool CStarParser::isType(const TokenInfo& token) {
  switch (token.getTokenKind()) {
    case VOID:
    case I8:
    case I16:
    case I32:
    case I64:
    case INT:
    case U8:
    case U16:
    case U32:
    case U64:
    case U128:
    case UINT:
    case ISIZE:
    case USIZE:
    case F32:
    case F64:
    case FLOAT:
    case UCHAR:
    case CHAR:
    case BOOL:
    case VEC2:
    case VEC3:
      // Every single IDENT is going to be interpreted as Symbol
      // case IDENT:
    case VEC4:
      return true;
    default:
      return false;
  }
}

bool CStarParser::isCastableOperator(const TokenInfo& token) {
  switch (token.getTokenKind()) {
    case UNSAFE_CAST:
    case CAST:
    case AS:
      return true;
    default:
      return false;
  }
}

bool CStarParser::isOperator(const TokenInfo& token) {
  switch (token.getTokenKind()) {
    case LPAREN:
    case QMARK:
      // those are because detecting is binary op
    case COLON:
      // suspicious
   // case LSQPAR:
    case RSQPAR:
      //
    case COLONCOLON:
    case UNSAFE_CAST:
    case CAST:
    case PLUSPLUS:
    case MINUSMINUS:
    case DOT:
    case ARROW:
    case SIZEOF:
    case TYPEOF:
    case MOVE:
    case DEREF:
    case REF:
    case AS:
    case PLUS:
    case MINUS:
    case NOT:
    case TILDE:
    case STAR:
    case MOD:
    case DIV:
    case LSHIFT:
    case RSHIFT:
    case LT:
    case LTEQ:
    case GT:
    case GTEQ:
    case EQUALEQUAL:
    case NOTEQUAL:
    case AND:
    case XOR:
    case OR:
    case LAND:
    case LOR:
    case EQUAL:
    case RET:
    case PLUSEQ:
    case MINUSEQ:
    case STAREQ:
    case DIVEQ:
    case MODEQ:
    case LSHIFTEQ:
    case RSHIFTEQ:
    case ANDEQ:
    case XOREQ:
    case OREQ:
    case COMMA:
      return true;
    default:
      return false;
  }
}

bool CStarParser::isLinkageMark(const TokenInfo& token) {
  switch (token.getTokenKind()) {
    case EXPORT:  // extern
    case IMPORT:  // extern
      return true;
    default:
      return false;
  }
}

bool CStarParser::isDeclarationModifier(const TokenInfo& token) {
  switch (token.getTokenKind()) {
    case PUBLIC:
    case PRIVATE:
    case STATIC:
    case EXPORT:
    case IMPORT:
      return true;
    default:
      return false;
  }
}

DeclarationModifiers CStarParser::parseDeclarationModifiers(bool localScope) {
  DeclarationModifiers modifiers;

  while (isDeclarationModifier(currentTokenInfo())) {
    switch (currentTokenKind()) {
      case PUBLIC:
        if (localScope) {
          ParserError("'public' is only valid on module declarations",
                      currentTokenInfo());
        }
        modifiers.access = ACCESS_PUBLIC;
        break;
      case PRIVATE:
        if (localScope) {
          ParserError("'private' is only valid on module declarations",
                      currentTokenInfo());
        }
        modifiers.access = ACCESS_PRIVATE;
        break;
      case STATIC:
        if (localScope) {
          ParserError("'static' local declarations are not part of the current "
                      "lifetime model; use module-level static state",
                      currentTokenInfo());
        }
        if (modifiers.linkage == VisibilitySpecifier::VIS_EXPORT ||
            modifiers.linkage == VisibilitySpecifier::VIS_IMPORT) {
          ParserError("'static' cannot be combined with import/export linkage",
                      currentTokenInfo());
        }
        modifiers.isStatic = true;
        if (modifiers.linkage == VisibilitySpecifier::VIS_DEFAULT) {
          modifiers.linkage = VisibilitySpecifier::VIS_STATIC;
        }
        break;
      case EXPORT:
        if (localScope) {
          ParserError("'export' is only valid on module declarations",
                      currentTokenInfo());
        }
        if (modifiers.isStatic) {
          ParserError("'export' cannot be combined with static linkage",
                      currentTokenInfo());
        }
        modifiers.linkage = VisibilitySpecifier::VIS_EXPORT;
        break;
      case IMPORT:
        if (localScope) {
          ParserError("'import' is only valid on module declarations",
                      currentTokenInfo());
        }
        if (modifiers.isStatic) {
          ParserError("'import' cannot be combined with static linkage",
                      currentTokenInfo());
        }
        modifiers.linkage = VisibilitySpecifier::VIS_IMPORT;
        break;
      default:
        assert(false && "Unreachable declaration modifier");
    }
    this->advance();
    skipTopLevelTrivia();
  }

  return modifiers;
}

bool CStarParser::isPackageMark(const TokenInfo& token) {
  switch (token.getTokenKind()) {
    case INCLUDE:
      return true;
    default:
      return false;
  }
}

bool CStarParser::isTypeQualifier(const TokenInfo& token) {
  switch (token.getTokenKind()) {
    case CONST:
    case CONSTREF:
    case CONSTPTR:
    case READONLY:
      return true;
    default:
      return false;
  }
}

bool CStarParser::isShortcutOp(const TokenInfo& token) {
  switch (token.getTokenKind()) {
    case PLUSEQ:
    case MINUSEQ:
    case STAREQ:
    case DIVEQ:
    case MODEQ:
    case RSHIFTEQ:
    case LSHIFTEQ:
    case ANDEQ:
    case OREQ:
    case XOREQ:
    case EQUAL:
    case TYPEINF:
      return true;
    default:
      return false;
  }
}

ShortcutOp CStarParser::typeOfShortcutOp(const TokenInfo& token) {
  switch (token.getTokenKind()) {
    case PLUSEQ:
      return S_PLUS;
    case MINUSEQ:
      return S_MIN;
    case STAREQ:
      return S_STA;
    case DIVEQ:
      return S_DIV;
    case MODEQ:
      return S_MOD;
    case RSHIFTEQ:
      return S_SHR;
    case LSHIFTEQ:
      return S_SHL;
    case ANDEQ:
      return S_AND;
    case OREQ:
      return S_OR;
    case XOREQ:
      return S_XOR;
    case TYPEINF:
      return S_MOV;
    case EQUAL:
      return S_NONE;
    default:
      assert(false && "Unreacheable!");
      return S_NONE;
  }
}

Type CStarParser::typeOf(const TokenInfo& token) {
  switch (token.getTokenKind()) {
    case VOID:
      return Type::T_VOID;
    case I8:
      return Type::T_I8;
    case I16:
      return Type::T_I16;
    case I32:
      return Type::T_I32;
    case I64:
      return Type::T_I64;
    case INT:
      return Type::T_INT;
    case U8:
      return Type::T_U8;
    case U16:
      return Type::T_U16;
    case U32:
      return Type::T_U32;
    case U64:
      return Type::T_U64;
    case U128:
      return Type::T_U128;
    case UINT:
      return Type::T_UINT;
    case ISIZE:
      return Type::T_ISIZE;
    case USIZE:
      return Type::T_USIZE;
    case F32:
      return Type::T_F32;
    case F64:
      return Type::T_F64;
    case FLOAT:
      return Type::T_FLOAT;
    case UCHAR:
      return Type::T_UCHAR;
    case CHAR:
      return Type::T_CHAR;
    case BOOL:
      return Type::T_BOOL;
    case VEC2:
      return Type::T_VEC2;
    case VEC3:
      return Type::T_VEC3;
    case VEC4:
      return Type::T_VEC4;
      // every single IDENT is going to be interpreted as Symbol...
      // case IDENT:
      //   return Type::T_DEFINED;
    default:
      assert(false && "Unreacheable");
      return Type::T_VOID;
  }
}

TypeQualifier CStarParser::typeQualifierOf(const TokenInfo& tokenInfo) {
  switch (tokenInfo.getTokenKind()) {
    case CONST:
      return TypeQualifier::Q_CONST;
    case CONSTREF:
      return TypeQualifier::Q_CONSTREF;
    case CONSTPTR:
      return TypeQualifier::Q_CONSTPTR;
    case READONLY:
      return TypeQualifier::Q_READONLY;
    default:
      assert(false && "Unreacheable!");
      return TypeQualifier::Q_NONE;
  }
}

void CStarParser::emitDiagnostic(cstar::diagnostics::Severity severity,
                                 cstar::diagnostics::DiagnosticCode code,
                                 std::string mesg, size_t begin, size_t end,
                                 size_t line, bool exitAfter) {
  cstar::diagnostics::DiagnosticRenderer renderer(std::cout);
  renderer.print({
      code,
      severity,
      std::move(mesg),
      this->m_Lexer.getFilepath().get(),
      this->m_Lexer.getBufferView(),
      {begin, end, line},
  });

  if (exitAfter) {
    exit(1);
  }
}

void CStarParser::ParserHint(std::string mesg, TokenInfo tokenInfo) {
  auto posInfo = tokenInfo.getTokenPositionInfo();
  emitDiagnostic(cstar::diagnostics::Severity::Hint,
                 cstar::diagnostics::DiagnosticCode::ParserHint,
                 std::move(mesg), posInfo.begin, posInfo.end, posInfo.line,
                 false);
}

void CStarParser::ParserWarning(std::string mesg, size_t newBegin,
                                size_t newEnd, size_t newLine) {
  emitDiagnostic(cstar::diagnostics::Severity::Warning,
                 cstar::diagnostics::DiagnosticCode::SemanticWarning,
                 std::move(mesg), newBegin, newEnd, newLine, false);
}

void CStarParser::ParserHint(std::string mesg, TokenInfo tokenInfo,
                             size_t new_begin) {
  auto posInfo = tokenInfo.getTokenPositionInfo();
  emitDiagnostic(cstar::diagnostics::Severity::Hint,
                 cstar::diagnostics::DiagnosticCode::ParserHint,
                 std::move(mesg), new_begin, posInfo.end, posInfo.line, false);
}

void CStarParser::ParserError(const std::string& mesg, TokenInfo tokenInfo) {
  auto posInfo = tokenInfo.getTokenPositionInfo();
  emitDiagnostic(cstar::diagnostics::Severity::Error,
                 cstar::diagnostics::DiagnosticCode::ParserSyntax, mesg,
                 posInfo.begin, posInfo.end, posInfo.line, true);
}

void CStarParser::ParserError(std::string mesg, size_t begin, size_t end,
                              size_t line_) {
  ParserError(std::move(mesg), begin, end, line_,
              cstar::diagnostics::DiagnosticCode::SemanticError);
}

void CStarParser::ParserError(std::string mesg, size_t begin, size_t end,
                              size_t line_,
                              cstar::diagnostics::DiagnosticCode code) {
  emitDiagnostic(cstar::diagnostics::Severity::Error,
                 code, std::move(mesg), begin, end, line_, false);
}

void CStarParser::ParserError(std::string mesg, TokenInfo tokenInfo,
                              size_t new_begin) {
  auto posInfo = tokenInfo.getTokenPositionInfo();
  emitDiagnostic(cstar::diagnostics::Severity::Error,
                 cstar::diagnostics::DiagnosticCode::ParserSyntax,
                 std::move(mesg), new_begin, posInfo.end, posInfo.line, true);
}

std::string_view::iterator CStarParser::viewLine(size_t line, size_t& rlbegin,
                                                 size_t& rlend,
                                                 size_t& offset) {
  auto buffer_view = this->m_Lexer.getBufferView();
  size_t lbegin = 0;
  // first line
  if (line == 0) {
    bool oneLineFlag = true;
    for (size_t i = 0; i < buffer_view.size(); i++) {
      if (buffer_view[i] == '\n') {
        oneLineFlag = false;

        lbegin = 0;
        offset = i;
        break;
      }
    }

    // that means we have only one line in the source file.
    // and it's not ending up with '\n'
    if (oneLineFlag) {
      lbegin = 0;
      offset = buffer_view.size();
    }
  } else {
    auto line_walker = 0;
    bool begin_mark = false;

    for (size_t i = 0; i < buffer_view.size(); ++i) {
      if (buffer_view[i] == '\n') {
        line_walker += 1;
      }

      if (line_walker == line && !begin_mark) {
        lbegin = i + 1;
        begin_mark = true;
      } else if (line_walker - 1 == line) {
        rlbegin -= lbegin;
        rlend -= lbegin;
        offset = i - lbegin;
        break;
      } else {
        continue;
      }
    }
  }

  return buffer_view.begin() + lbegin;
}
