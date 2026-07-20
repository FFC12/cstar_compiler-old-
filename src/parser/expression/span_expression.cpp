#include <parser/parser_private.hpp>

using namespace cstar::parser_private;

ASTNode CStarParser::advanceSpanExpression() {
  auto begin = currentTokenInfo().getTokenPositionInfo().begin;
  auto line = currentTokenInfo().getTokenPositionInfo().line;

  // consume contextual `span`
  this->advance();

  while (is(TokenKind::COMMENT) || is(TokenKind::LINEFEED)) {
    this->advance();
  }

  if (!is(TokenKind::IDENT) && !is(TokenKind::STATE) && !is(TokenKind::SELF)) {
    ParserError("`span` expects a fixed array value", currentTokenInfo());
  }

  ASTNode base = this->advanceSymbol();
  ASTNode start;
  ASTNode end;
  bool hasRange = false;

  if (is(TokenKind::LSQPAR)) {
    hasRange = true;
    start = this->expression(true, 4);

    if (!is(TokenKind::RANGE)) {
      ParserError("`span` range must use `..` as in `span data[4..32]`",
                  currentTokenInfo());
    }
    end = this->expression(true, 3);
  }

  auto endLoc = prevTokenInfo().getTokenPositionInfo().end;
  auto semLoc = SemanticLoc(begin, endLoc, line);
  return std::make_unique<SpanAST>(std::move(base), std::move(start),
                                   std::move(end), hasRange, false, semLoc);
}
