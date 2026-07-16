#include <parser/parser_private.hpp>

using namespace cstar::parser_private;

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
