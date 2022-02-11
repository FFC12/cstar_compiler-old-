#include <parser/parser.hpp>

void CStarParser::funcDecl(VisibilitySpecifier visibilitySpecifier) {
  auto funcName = currentTokenStr();

  // advance the name
  this->advance();

  expected(TokenKind::LPAREN);

  // '('
  this->advance();

  std::vector<ASTNode> params;
  advanceParams(params);

  expected(TokenKind::RPAREN);

  // ')'
  this->advance();

  ASTNode returnType;
  if (is(TokenKind::COLONCOLON)) {
    this->advance();
    if (isType(currentTokenInfo())) {
      returnType = this->advanceType();
    } else if (is(TokenKind::IDENT)) {
      returnType = this->advanceSymbol();
    } else {
      ParserError("Unexpected token", currentTokenInfo());
    }
  }

  // func body
  expected(TokenKind::LBRACK);

  std::vector<ASTNode> localVars;
  this->advanceFuncBody(localVars);
}

void CStarParser::advanceParams(std::vector<ASTNode>& params) {
param_again:
  ASTNode typeNode;
  TypeQualifier typeQualifier = TypeQualifier::Q_NONE;
  bool castAllowed = true;
  ASTNode symbol0, symbol1;

  if (isTypeQualifier(currentTokenInfo())) {
    typeQualifier = typeQualifierOf(currentTokenInfo());
    this->advance();
  }

  size_t beginLoc = currentTokenInfo().getTokenPositionInfo().begin;
  size_t line = currentTokenInfo().getTokenPositionInfo().line;

  if (isType(currentTokenInfo())) {
    auto type = this->advanceType();

    // TODO: for forward declaration, must not be needed to
    //  specify param name.

    // expected param name
    expected(TokenKind::IDENT);

    // get the param name
    symbol0 = this->advanceSymbol();

    size_t endLoc = currentTokenInfo().getTokenPositionInfo().end;
    auto semLoc = SemanticLoc(beginLoc, endLoc, line);
    auto param =
        std::make_unique<ParamAST>(std::move(symbol0), nullptr, std::move(type),
                                   castAllowed, false, typeQualifier, semLoc);
    params.emplace_back(std::move(param));
  } else if (is(TokenKind::IDENT)) {
    bool outOfSize = false;
    auto nextTokenInfo = this->nextTokenInfo(outOfSize);
    if (outOfSize) {
      ParserError("Unexpected token", currentTokenInfo());
    }

    auto nextToken = nextTokenInfo.getTokenKind();
    if (nextToken == TokenKind::STAR || nextToken == TokenKind::XOR) {
      // %100 defined type
      auto type = this->advanceSymbol();
      castAllowed = false;

      expected(TokenKind::IDENT);
      // get the param name
      symbol0 = this->advanceSymbol();

      size_t endLoc = currentTokenInfo().getTokenPositionInfo().end;
      auto semLoc = SemanticLoc(beginLoc, endLoc, line);

      auto param = std::make_unique<ParamAST>(std::move(symbol0), nullptr,
                                              std::move(type), castAllowed,
                                              false, typeQualifier, semLoc);
      params.emplace_back(std::move(param));
    } else {
      // if the next token is primitive type...
      if (isType(nextTokenInfo)) {
        // get the param name
        symbol0 = this->advanceSymbol();

        auto type = this->advanceType();

        castAllowed = false;

        size_t endLoc = currentTokenInfo().getTokenPositionInfo().end;
        auto semLoc = SemanticLoc(beginLoc, endLoc, line);

        auto param = std::make_unique<ParamAST>(std::move(symbol0), nullptr,
                                                std::move(type), castAllowed,
                                                false, typeQualifier, semLoc);
      } else {
        // get the param name
        symbol0 = this->advanceSymbol();
        symbol1 = this->advanceSymbol();

        castAllowed = false;

        size_t endLoc = currentTokenInfo().getTokenPositionInfo().end;
        auto semLoc = SemanticLoc(beginLoc, endLoc, line);

        auto param = std::make_unique<ParamAST>(
            std::move(symbol0), std::move(symbol1), nullptr, castAllowed, true,
            typeQualifier, semLoc);
      }
    }

  } else {
    if (!is(TokenKind::RPAREN))
      ParserError("Expected a primitive or defined type", currentTokenInfo());
  }

  if (is(TokenKind::COMMA)) {
    // skip ','
    this->advance();
    goto param_again;
  }
}

void CStarParser::advanceFuncBody(std::vector<ASTNode>& localVars) {
  this->advance();

  while (!is(TokenKind::RBRACK)) {
    if (isType(currentTokenInfo()) || is(TokenKind::IDENT)) {
      auto localVar =
          varDecl(VisibilitySpecifier::VIS_LOCAL, is(TokenKind::IDENT), true);
      localVars.emplace_back(std::move(localVar));
    } else {
      while (is(TokenKind::LINEFEED) || is(TokenKind::COMMENT)) {
        advance();
      }

      // if - else
      // loop
      // option
      // ret
    }
  }

  this->advance();
}