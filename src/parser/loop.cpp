#include <parser/parser.hpp>

void CStarParser::advanceLoopStmt(std::vector<ASTNode> &scope) {
  size_t begin = currentTokenInfo().getTokenPositionInfo().begin;
  size_t line = currentTokenInfo().getTokenPositionInfo().line;

  // loop
  this->advance();

  expected(TokenKind::LPAREN);

  ASTNode min, max, indexSymbol, dataSymbol;
  ASTNode iterSymbol, cond;
  bool rangeLoop = false;
  bool hasNumericRange = false;
  bool indexable = false;
  bool outOfSize = false;

  auto nextToken = nextTokenInfo(outOfSize).getTokenKind();
  if (outOfSize) {
    ParserError("Incomplete loop statement", currentTokenInfo());
  }
  auto nextToken2 = nextTokenInfo(outOfSize, 2).getTokenKind();
  if (outOfSize) {
    ParserError("Incomplete loop statement", currentTokenInfo());
  }
  auto nextToken3 = nextTokenInfo(outOfSize, 3).getTokenKind();
  if (outOfSize) {
    ParserError("Incomplete loop statement", currentTokenInfo());
  }

  if (nextToken == TokenKind::IDENT && nextToken2 == TokenKind::IN) {
    // advance '('
    this->advance();

    expected(TokenKind::IDENT);
    dataSymbol = std::move(this->advanceSymbol());

    expected(TokenKind::IN);

  sequence:
    // advance 'in'
    this->advance();

    expected({TokenKind::IDENT, TokenKind::LSQPAR});

    if (is(TokenKind::IDENT)) {
      iterSymbol = std::move(this->advanceConstantOrLiteral());
      expected(TokenKind::RPAREN);
      this->advance();
    } else if (is(TokenKind::LSQPAR) && !indexable) {
      // advance '['
      this->advance();

      expected({IDENT, SCALARI});

      auto lower = std::move(this->advanceConstantOrLiteral());

      expected(TokenKind::COMMA);

      // advance ','
      this->advance();

      auto higher = std::move(this->advanceConstantOrLiteral());

      expected(TokenKind::RSQPAR);

      // advance ']'
      this->advance();

      expected(TokenKind::RPAREN);

      this->advance();

      min = std::move(lower);
      max = std::move(higher);

      hasNumericRange = true;
    } else {
      ParserError("Loop statement condition is not met", currentTokenInfo());
    }

    rangeLoop = true;
    goto not_conditional;
  } else {
    auto nextToken4 = nextTokenInfo(outOfSize, 4).getTokenKind();
    if (outOfSize) {
      ParserError("Incomplete loop statement", currentTokenInfo());
    }

    if (nextToken4 == TokenKind::IN && nextToken2 == TokenKind::COMMA &&
        nextToken3 == TokenKind::IDENT && nextToken == TokenKind::IDENT) {
      // advance '('
      this->advance();

      expected(TokenKind::IDENT);
      indexSymbol = std::move(this->advanceSymbol());

      expected(TokenKind::COMMA);

      // advance ','
      this->advance();

      expected(TokenKind::IDENT);
      dataSymbol = std::move(this->advanceSymbol());

      indexable = true;
      goto sequence;
    }
  }

  cond = std::move(this->expression(true, 1));

not_conditional:
  size_t end = currentTokenInfo().getTokenPositionInfo().end;
  SemanticLoc semanticLoc = SemanticLoc(begin, end, line);

  expected(TokenKind::LBRACK);

  std::vector<ASTNode> loopBody{};

  this->advanceScope(loopBody);

  auto loopAst = std::make_unique<LoopStmtAST>(
      std::move(cond), std::move(indexSymbol), std::move(dataSymbol),
      std::move(iterSymbol), std::move(min), std::move(max),
      std::move(loopBody), rangeLoop, hasNumericRange, indexable, semanticLoc);

  scope.emplace_back(std::move(loopAst));
}