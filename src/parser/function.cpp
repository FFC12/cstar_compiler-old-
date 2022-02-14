#include <parser/parser.hpp>

void CStarParser::funcDecl(VisibilitySpecifier visibilitySpecifier) {
  auto funcName = currentTokenStr();
  bool isForwardDecl = visibilitySpecifier == VisibilitySpecifier::VIS_IMPORT;
  bool isExported = visibilitySpecifier == VisibilitySpecifier::VIS_EXPORT;

  auto posInfo = currentTokenInfo().getTokenPositionInfo();
  size_t begin = posInfo.begin;
  size_t line = posInfo.line;

  // advance the name
  this->advance();

  expected(TokenKind::LPAREN);

  // '('
  this->advance();

  std::vector<ASTNode> params{};
  advanceParams(params, isForwardDecl);

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
  } else {  // void by default
    posInfo = this->currentTokenInfo().getTokenPositionInfo();
    SemanticLoc semLoc = SemanticLoc(posInfo.begin, posInfo.end, posInfo.line);
    returnType = std::make_unique<TypeAST>(Type::T_VOID, nullptr, false, true,
                                           false, 0, semLoc);
  }

  std::vector<ASTNode> scope{};

  if (!isForwardDecl) {
    expected(TokenKind::LBRACK);

    // func body as scope
    this->advanceScope(scope);
  } else {
    expected(TokenKind::SEMICOLON);
    this->advance();
  }

  posInfo = currentTokenInfo().getTokenPositionInfo();
  size_t end = posInfo.end;
  SemanticLoc semLoc = SemanticLoc(begin, end, line);

  auto func = std::make_unique<FuncAST>(funcName, std::move(returnType),
                                        std::move(params), std::move(scope),
                                        isForwardDecl, isExported, semLoc);

  this->m_AST.emplace_back(std::move(func));
}

void CStarParser::advanceParams(std::vector<ASTNode>& params,
                                bool isForwardDecl) {
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

    if (!isForwardDecl) {
      // expected param name
      expected(TokenKind::IDENT);

      // get the param name
      symbol0 = this->advanceSymbol();
    }

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
      auto type = std::move(this->advanceSymbol());
      castAllowed = false;

      if (!isForwardDecl) {
        expected(TokenKind::IDENT);
        // get the param name
        symbol0 = std::move(this->advanceSymbol());
      }

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
        params.emplace_back(std::move(param));
      } else {
        if (!isForwardDecl) {
          // get the param name
          symbol0 = this->advanceSymbol();
          symbol1 = this->advanceSymbol();

          castAllowed = false;

          size_t endLoc = currentTokenInfo().getTokenPositionInfo().end;
          auto semLoc = SemanticLoc(beginLoc, endLoc, line);

          auto param = std::make_unique<ParamAST>(
              std::move(symbol0), std::move(symbol1), nullptr, castAllowed,
              true, typeQualifier, semLoc);
          params.emplace_back(std::move(param));
        }
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

void CStarParser::advanceScope(std::vector<ASTNode>& scope) {
  this->advance();

  while (!is(TokenKind::RBRACK)) {
    if (isType(currentTokenInfo()) || is(TokenKind::IDENT)) {
      bool outOfSize = false;
      auto nextTokenInfo = this->nextTokenInfo(outOfSize);
      auto nextToken = nextTokenInfo.getTokenKind();
      if (outOfSize) {
        ParserError("Incomplete declaration or expression", currentTokenInfo());
      }

      size_t begin = currentTokenInfo().getTokenPositionInfo().begin;
      size_t line = currentTokenInfo().getTokenPositionInfo().line;

      if (nextToken == TokenKind::IDENT) {
        varDecl(VisibilitySpecifier::VIS_LOCAL, is(TokenKind::IDENT), true,
                &scope);
      } else if (isShortcutOp(nextTokenInfo)) {
        auto symbol = std::move(advanceSymbol());
        auto shortcutOp = typeOfShortcutOp(currentTokenInfo());
        auto shortcutOpStr = currentTokenStr();
        auto expr = std::move(this->expression(false, 0, false, false, true));

        expected(TokenKind::SEMICOLON);
        this->advance();

        size_t end = currentTokenInfo().getTokenPositionInfo().end;
        SemanticLoc semanticLoc = SemanticLoc(begin, end, line);

        auto assignmentExpr = std::make_unique<AssignmentAST>(
            std::move(symbol), std::move(expr), shortcutOp, shortcutOpStr,
            semanticLoc);

        scope.emplace_back(std::move(assignmentExpr));
      } else if (nextToken == LSQPAR) {
        auto symbol = std::move(advanceSymbol());

        expected(TokenKind::LSQPAR);

        // advance '['
        this->advance();

        expected({TokenKind::SCALARI, TokenKind::IDENT});

        std::vector<ASTNode> indexes{};
        auto index = is(TokenKind::SCALARI) ? this->advanceConstantOrLiteral()
                                            : this->advanceSymbol();
        indexes.emplace_back(std::move(index));

        while (is(TokenKind::COLON)) {
          // advance ':'
          this->advance();

          expected({TokenKind::SCALARI, TokenKind::IDENT});

          index = is(TokenKind::SCALARI) ? this->advanceConstantOrLiteral()
                                         : this->advanceSymbol();

          indexes.emplace_back(std::move(index));
        }

        expected(TokenKind::RSQPAR);
        this->advance();

        if (!isShortcutOp(currentTokenInfo())) {
          ParserError("Unexpected token for assignment expression.",
                      currentTokenInfo());
        }

        auto shortcutOp = typeOfShortcutOp(currentTokenInfo());
        auto shortcutOpStr = currentTokenStr();
        auto expr = std::move(this->expression(false, 0, false, false, true));

        expected(TokenKind::SEMICOLON);
        this->advance();

        size_t end = currentTokenInfo().getTokenPositionInfo().end;
        SemanticLoc semanticLoc = SemanticLoc(begin, end, line);

        auto assignmentExpr = std::make_unique<AssignmentAST>(
            std::move(symbol), std::move(expr), std::move(indexes), shortcutOp,
            shortcutOpStr, semanticLoc);
        scope.emplace_back(std::move(assignmentExpr));
      }
    } else {
      while (is(TokenKind::LINEFEED) || is(TokenKind::COMMENT)) {
        advance();
      }

      if (is(TokenKind::RET)) {
        ASTNode retExpr;
        bool noReturn = false;
        bool outOfSize = false;
        auto nextToken = nextTokenInfo(outOfSize).getTokenKind();
        if (outOfSize) {
          ParserError("Unexpected token", currentTokenInfo());
        }

        PositionInfo posInfo = currentTokenInfo().getTokenPositionInfo();
        size_t begin = posInfo.begin;
        size_t line = posInfo.line;

        if (nextToken == TokenKind::SEMICOLON) {
          // empty ret
          retExpr = nullptr;
          noReturn = true;
          this->advance();
        } else {
          retExpr = std::move(this->expression(false, 0, true));
        }

        posInfo = currentTokenInfo().getTokenPositionInfo();
        size_t end = posInfo.end;
        SemanticLoc semLoc = SemanticLoc(begin, end, line);

        auto retAst =
            std::make_unique<RetAST>(std::move(retExpr), noReturn, semLoc);

        expected(TokenKind::SEMICOLON);
        this->advance();
        scope.emplace_back(std::move(retAst));
      } else if (is(TokenKind::IF)) {
        this->advanceIfStmt(scope);
      } else if (is(TokenKind::LOOP)) {
        this->advanceLoopStmt(scope);
      }

      // option
    }
  }

  // advance '}'
  this->advance();
}