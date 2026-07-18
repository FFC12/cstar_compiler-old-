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
    this->advance();
    if (is(TokenKind::SCALARI) || is(TokenKind::SCALARD) ||
        is(TokenKind::TRUE) || is(TokenKind::FALSE) || is(TokenKind::LITERAL)) {
      start = this->advanceConstantOrLiteral();
    } else if (is(TokenKind::IDENT) || is(TokenKind::STATE) ||
               is(TokenKind::SELF)) {
      start = this->advanceSymbol();
    } else {
      ParserError("`span` range begin must be a scalar or symbol",
                  currentTokenInfo());
    }

    if (!is(TokenKind::RANGE)) {
      ParserError("`span` range must use `..` as in `span data[4..32]`",
                  currentTokenInfo());
    }
    this->advance();
    if (is(TokenKind::SCALARI) || is(TokenKind::SCALARD) ||
        is(TokenKind::TRUE) || is(TokenKind::FALSE) || is(TokenKind::LITERAL)) {
      end = this->advanceConstantOrLiteral();
    } else if (is(TokenKind::IDENT) || is(TokenKind::STATE) ||
               is(TokenKind::SELF)) {
      end = this->advanceSymbol();
    } else {
      ParserError("`span` range end must be a scalar or symbol",
                  currentTokenInfo());
    }

    if (!is(TokenKind::RSQPAR)) {
      ParserError("`span` range must be closed with ']'", currentTokenInfo());
    }
    this->advance();
  }

  auto endLoc = prevTokenInfo().getTokenPositionInfo().end;
  auto semLoc = SemanticLoc(begin, endLoc, line);
  return std::make_unique<SpanAST>(std::move(base), std::move(start),
                                   std::move(end), hasRange, false, semLoc);
}
