#include <parser/parser_private.hpp>

using namespace cstar::parser_private;

ASTNode CStarParser::advanceFieldAccessChain(size_t begin, size_t line) {
  ASTNode access = std::move(advanceSymbol());

  while (is(TokenKind::DOT) || is(TokenKind::COLONCOLON)) {
    auto accessInfo = currentTokenInfo().getTokenPositionInfo();
    auto binOpKind =
        is(TokenKind::DOT) ? BinOpKind::B_DOT : BinOpKind::B_CCOL;
    std::string accessOp = currentTokenStr();
    this->advance();

    if (!is(TokenKind::IDENT) && !is(TokenKind::DESTRUCTOR)) {
      expected(TokenKind::IDENT);
    }
    auto tokenPos = currentTokenInfo().getTokenPositionInfo();
    auto memberLoc = SemanticLoc(tokenPos.begin, tokenPos.end, tokenPos.line);
    auto fieldSymbol =
        std::make_unique<SymbolAST>(currentTokenStr(), memberLoc);
    this->advance();

    SemanticLoc fieldLoc(begin, accessInfo.end, line);
    access = std::make_unique<BinaryOpAST>(
        std::move(access), std::move(fieldSymbol), nullptr, binOpKind,
        accessOp, fieldLoc);
  }

  return access;
}
