#include <parser/parser_private.hpp>

using namespace cstar::parser_private;

ASTNode CStarParser::advanceType() {
  if (this->isType(this->currentTokenInfo())) {
    TokenInfo prevTokenInfo = this->currentTokenInfo();
    size_t indirectionLevel = 0;
    bool isUniquePtr = false;

    auto tokenPos = currentTokenInfo().getTokenPositionInfo();
    auto semLoc = SemanticLoc(tokenPos.begin, tokenPos.end, tokenPos.line);

    this->advance();

    bool isRef = false;
    if (is(TokenKind::AND)) {
      isRef = true;
      this->advance();
      semLoc.end += indirectionLevel;
    } else {
      //* | ^
      while (is(TokenKind::STAR) || is(TokenKind::XOR)) {
        isUniquePtr = this->currentTokenKind() == TokenKind::XOR;

        indirectionLevel = advancePointerType(isUniquePtr);
        semLoc.end += indirectionLevel;
        // std::cout << "Type Indirection level: " << indirectionLevel << "\n";
      }
    }

    ASTNode typeAst = std::make_unique<TypeAST>(
        typeSpecifierOf(prevTokenInfo), nullptr, isUniquePtr, true, isRef,
        indirectionLevel, semLoc);

    // expected >
    // expected({TokenKind::GT, TokenKind::RPAREN});

    return std::move(typeAst);
  }

  return std::move(this->advanceSymbol());
}
