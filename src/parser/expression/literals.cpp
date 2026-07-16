#include <parser/parser_private.hpp>

using namespace cstar::parser_private;

ASTNode CStarParser::advanceConstantOrLiteral() {
  // this is obviously a scalar or literal or others(matrix and vec?)
  if (is(TokenKind::SCALARD) || is(TokenKind::SCALARI) ||
      is(TokenKind::LETTER) || is(TokenKind::LITERAL) || is(TokenKind::TRUE) ||
      is(TokenKind::FALSE)) {
    bool isIntegral = !is(TokenKind::LITERAL) && !is(TokenKind::LETTER);
    bool isFloat = is(TokenKind::SCALARD);
    bool isBoolean = is(TokenKind::TRUE) || is(TokenKind::FALSE);
    bool isLetter = is(TokenKind::LETTER);
    bool isLiteral = is(TokenKind::LITERAL);

    auto value = this->currentTokenStr();

    auto tokenPos = currentTokenInfo().getTokenPositionInfo();
    auto begin = tokenPos.begin;
    this->advance();
    auto semLoc = SemanticLoc(begin, tokenPos.end, tokenPos.line);

    return std::make_unique<ScalarOrLiteralAST>(
        value, isIntegral, isFloat, isBoolean, isLetter, isLiteral, semLoc);
  }

  return std::move(this->advanceType());
}

// Every IDENT will be evaulated here.
// Our Symbols are might be DEFINED.
// So they might have POINTER LEVEL(s)
