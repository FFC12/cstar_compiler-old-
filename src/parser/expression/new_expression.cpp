#include <parser/parser_private.hpp>

using namespace cstar::parser_private;

ASTNode CStarParser::advanceNewExpression(bool isShared) {
  auto startInfo = currentTokenInfo();
  auto startPos = startInfo.getTokenPositionInfo();

  if (isShared) {
    this->advance();
    expected(TokenKind::NEW);
  }

  expected(TokenKind::NEW);
  this->advance();
  bool isFallible = false;
  if (is(TokenKind::QMARK)) {
    isFallible = true;
    this->advance();
  }

  ASTNode allocator = nullptr;
  if (is(TokenKind::LPAREN)) {
    bool outOfSize = false;
    auto tokenAfterOpen = nextTokenInfo(outOfSize).getTokenKind();
    if (outOfSize) {
      ParserError("Incomplete new allocator expression", currentTokenInfo());
    }
    if (tokenAfterOpen == TokenKind::RPAREN) {
      ParserError("new allocator expression cannot be empty",
                  currentTokenInfo());
    }
    allocator = expression(true, 1);
  }

  TypeSpecifier typeSpec = TypeSpecifier::SPEC_DEFINED;
  std::string typeName;
  if (isType(currentTokenInfo())) {
    typeSpec = typeSpecifierOf(currentTokenInfo());
    if (typeSpec == TypeSpecifier::SPEC_VOID) {
      ParserError("`new` cannot allocate void", currentTokenInfo());
    }
    typeName = currentTokenStr();
    this->advance();
  } else {
    expected(TokenKind::IDENT);
    typeName = advanceDefinedTypeName();
  }

  expected(TokenKind::LPAREN);
  ASTNode args = nullptr;
  bool outOfSize = false;
  auto tokenAfterOpen = nextTokenInfo(outOfSize).getTokenKind();
  if (outOfSize) {
    ParserError("Incomplete new constructor call", currentTokenInfo());
  }
  if (tokenAfterOpen == TokenKind::RPAREN) {
    this->advance();
    this->advance();
  } else {
    args = advanceArgumentList();
  }

  auto endPos = prevTokenInfo().getTokenPositionInfo();
  SemanticLoc semLoc(startPos.begin, endPos.end, startPos.line);
  return std::make_unique<NewAST>(typeSpec, typeName, std::move(allocator),
                                  std::move(args), isShared, isFallible,
                                  semLoc);
}
