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
  TypeQualifier retTypeQualifier = TypeQualifier::Q_NONE;
  if (is(TokenKind::COLONCOLON)) {
    this->advance();

    if (isTypeQualifier(currentTokenInfo())) {
      retTypeQualifier = typeQualifierOf(currentTokenInfo());
      this->advance();
    }

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
    returnType = std::make_unique<TypeAST>(TypeSpecifier::SPEC_VOID, nullptr,
                                           false, true, false, 0, semLoc);
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

  auto func = std::make_unique<FuncAST>(
      funcName, std::move(returnType), std::move(params), std::move(scope),
      retTypeQualifier, isForwardDecl, isExported, semLoc);

  this->m_AST.emplace_back(std::move(func));
}

void CStarParser::advanceParams(std::vector<ASTNode>& params,
                                bool isForwardDecl) {
param_again:
  ASTNode typeNode;
  TypeQualifier typeQualifier = TypeQualifier::Q_NONE;
  ASTNode symbol0, symbol1;

  if (isTypeQualifier(currentTokenInfo())) {
    typeQualifier = typeQualifierOf(currentTokenInfo());
    this->advance();
  }

  size_t beginLoc = currentTokenInfo().getTokenPositionInfo().begin;
  size_t line = currentTokenInfo().getTokenPositionInfo().line;

  if (isType(currentTokenInfo())) {  // cast allowed
    auto type = this->advanceType();

    std::vector<ASTNode> arrayDimensions;
    bool arrayFlag = this->advanceTypeSubscript(arrayDimensions);
    if (is(TokenKind::AND)) {
      this->advance();
      auto typeAst = dynamic_cast<TypeAST*>(type.get());
      typeAst->setIsRef(true);
    }

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
                                   std::move(arrayDimensions), arrayFlag, true,
                                   false, false, true, typeQualifier, semLoc);
    params.emplace_back(std::move(param));
  } else if (is(TokenKind::IDENT)) {
    bool outOfSize = false;
    auto nextTokenInfo = this->nextTokenInfo(outOfSize);
    if (outOfSize) {
      ParserError("Unexpected token", currentTokenInfo());
    }

    auto nextToken = nextTokenInfo.getTokenKind();
    if (nextToken == TokenKind::STAR || nextToken == TokenKind::XOR ||
        nextToken == TokenKind::LSQPAR) {  // cast allowed
      // %100 defined type
      auto type = std::move(this->advanceSymbol());

      std::vector<ASTNode> arrayDimensions;
      bool arrayFlag = this->advanceTypeSubscript(arrayDimensions);
      if (is(TokenKind::AND)) {
        this->advance();
        auto typeAst = dynamic_cast<TypeAST*>(type.get());
        typeAst->setIsRef(true);
      }

      if (!isForwardDecl) {
        expected(TokenKind::IDENT);
        // get the param name
        symbol0 = std::move(this->advanceSymbol());
      }

      size_t endLoc = currentTokenInfo().getTokenPositionInfo().end;
      auto semLoc = SemanticLoc(beginLoc, endLoc, line);

      auto param = std::make_unique<ParamAST>(
          std::move(symbol0), nullptr, std::move(type),
          std::move(arrayDimensions), arrayFlag, true, false, false, false,
          typeQualifier, semLoc);
      params.emplace_back(std::move(param));
    } else {
      // if the next token is primitive type...
      if (isType(nextTokenInfo)) {  // cast not allowed
        // get the param name
        if (!isForwardDecl) {
          expected(TokenKind::IDENT);
          symbol0 = this->advanceSymbol();
        }

        auto type = this->advanceType();

        std::vector<ASTNode> arrayDimensions;
        bool arrayFlag = this->advanceTypeSubscript(arrayDimensions);
        if (is(TokenKind::AND)) {
          this->advance();
          auto typeAst = dynamic_cast<TypeAST*>(type.get());
          typeAst->setIsRef(true);
        }

        size_t endLoc = currentTokenInfo().getTokenPositionInfo().end;
        auto semLoc = SemanticLoc(beginLoc, endLoc, line);

        auto param = std::make_unique<ParamAST>(
            std::move(symbol0), nullptr, std::move(type),
            std::move(arrayDimensions), arrayFlag, false, false, false, true,
            typeQualifier, semLoc);
        params.emplace_back(std::move(param));
      } else {
        auto prevNextTokenInfo = nextTokenInfo;
        outOfSize = false;
        nextTokenInfo = this->nextTokenInfo(outOfSize, 2);
        if (outOfSize) {
          ParserError("Unexpected token", currentTokenInfo());
        }

        nextToken = nextTokenInfo.getTokenKind();
        if (nextToken == TokenKind::STAR || nextToken == TokenKind::XOR ||
            nextToken == TokenKind::LSQPAR) {  // cast allowed
          // expect param name
          if (!isForwardDecl) {
            symbol0 = std::move(this->advanceSymbol());
          }

          expected(TokenKind::IDENT);
          // get the defined type
          auto type = std::move(this->advanceSymbol());
          std::vector<ASTNode> arrayDimensions;
          bool arrayFlag = this->advanceTypeSubscript(arrayDimensions);
          if (is(TokenKind::AND)) {
            this->advance();
            auto typeAst = dynamic_cast<TypeAST*>(type.get());
            typeAst->setIsRef(true);
          }

          size_t endLoc = currentTokenInfo().getTokenPositionInfo().end;
          auto semLoc = SemanticLoc(beginLoc, endLoc, line);

          auto param = std::make_unique<ParamAST>(
              std::move(symbol0), nullptr, std::move(type),
              std::move(arrayDimensions), arrayFlag, false, false, true, false,
              typeQualifier, semLoc);
          params.emplace_back(std::move(param));
        } else {
          if (!isForwardDecl) {  // not clear that is cast allowed or not
            // get the param name
            symbol0 = this->advanceSymbol();
            symbol1 = this->advanceSymbol();

            size_t endLoc = currentTokenInfo().getTokenPositionInfo().end;
            auto semLoc = SemanticLoc(beginLoc, endLoc, line);

            auto param = std::make_unique<ParamAST>(
                std::move(symbol0), std::move(symbol1), nullptr,
                std::vector<ASTNode>(), false, false, true, true, false,
                typeQualifier, semLoc);
            params.emplace_back(std::move(param));
          }
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
    TypeQualifier typeQualifier = TypeQualifier::Q_NONE;
    bool hasConstness = false;

    if (isTypeQualifier(currentTokenInfo())) {
      hasConstness = true;
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

    if (isType(currentTokenInfo()) || is(TokenKind::IDENT) ||
        is(TokenKind::STAR) || is(TokenKind::DEREF)) {
      bool outOfSize = false;
      auto nextTokenInfo = this->nextTokenInfo(outOfSize);
      auto nextToken = nextTokenInfo.getTokenKind();
      if (outOfSize) {
        ParserError("Incomplete declaration or expression", currentTokenInfo());
      }

      size_t begin = currentTokenInfo().getTokenPositionInfo().begin;
      size_t line = currentTokenInfo().getTokenPositionInfo().line;

      bool deref = false;
      size_t dereferencedLevel = 0;
      if (is(TokenKind::DEREF) || is(TokenKind::STAR)) {
        dereferencedLevel = 1;

        bool derefKeywordUsed = false;
        if (is(TokenKind::DEREF)) derefKeywordUsed = true;

        while (is(TokenKind::DEREF) || is(TokenKind::STAR)) {
          this->advance();

          if (is(TokenKind::DEREF) && !derefKeywordUsed) {
            derefKeywordUsed = true;
          } else if (is(TokenKind::DEREF) && derefKeywordUsed) {
            ParserError("'deref' keyword cannot be used more than one",
                        currentTokenInfo());
          }

          nextTokenInfo = this->nextTokenInfo(outOfSize);
          nextToken = nextTokenInfo.getTokenKind();
          if (outOfSize) {
            ParserError("Incomplete declaration or expression",
                        currentTokenInfo());
          }

          if (is(TokenKind::DEREF) || is(TokenKind::STAR))
            dereferencedLevel += 1;

          deref = true;
        }
      }

      if (isType(currentTokenInfo()) || nextToken == TokenKind::IDENT ||
          (isTypeQualifier(prevTokenInfo()) && hasConstness)) {
        varDecl(typeQualifier, VisibilitySpecifier::VIS_LOCAL,
                is(TokenKind::IDENT), true, &scope);
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
            std::move(symbol), std::move(expr), deref, dereferencedLevel,
            shortcutOp, shortcutOpStr, semanticLoc);

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
            std::move(symbol), std::move(expr), deref, dereferencedLevel,
            std::move(indexes), shortcutOp, shortcutOpStr, semanticLoc);
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
      } else if (is(TokenKind::IDENT) || is(TokenKind::MINUSMINUS) ||
                 is(TokenKind::PLUSPLUS)) {
        bool outOfSize = false;
        auto nextToken = nextTokenInfo(outOfSize).getTokenKind();
        if (outOfSize) {
          ParserError("Unexpected token", currentTokenInfo());
        }

        PositionInfo posInfo = currentTokenInfo().getTokenPositionInfo();
        size_t begin = posInfo.begin;
        size_t line = posInfo.line;

        bool postfix = false, prefix = false;
        bool increment = false, decrement = false;
        std::string name;
        if (is(TokenKind::MINUSMINUS) || is(TokenKind::PLUSPLUS)) {
          increment = is(TokenKind::PLUSPLUS);
          decrement = is(TokenKind::MINUSMINUS);
          prefix = true;

          if (nextToken == TokenKind::IDENT) {
            this->advance();
            name = currentTokenStr();
            this->advance();
          }
        } else if (is(TokenKind::IDENT)) {
          name = currentTokenStr();

          if (nextToken == TokenKind::MINUSMINUS ||
              nextToken == TokenKind::PLUSPLUS) {
            postfix = true;
            this->advance();
            increment = is(TokenKind::PLUSPLUS);
            decrement = is(TokenKind::MINUSMINUS);
            this->advance();
          }
        }
        posInfo = currentTokenInfo().getTokenPositionInfo();
        size_t end = posInfo.end;
        SemanticLoc semLoc = SemanticLoc(begin, end, line);

        auto fixExpr = std::make_unique<FixAST>(name, prefix, postfix,
                                                increment, decrement, semLoc);
        scope.emplace_back(std::move(fixExpr));
      } else {
        ParserError("Unexpected token", currentTokenInfo());
      }

      // option - case
    }
  }

  // advance '}'
  this->advance();
}