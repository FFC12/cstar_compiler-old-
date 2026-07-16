#include <parser/parser_private.hpp>

using namespace cstar::parser_private;

ASTNode CStarParser::advanceDefinedType() {
  expected(TokenKind::IDENT);

  auto tokenPos = currentTokenInfo().getTokenPositionInfo();
  auto semLoc = SemanticLoc(tokenPos.begin, tokenPos.end, tokenPos.line);
  auto symbol = std::make_unique<SymbolAST>(advanceDefinedTypeName(), semLoc);

  bool isUniquePtr = false;
  size_t indirectionLevel = 0;
  bool isRef = false;

  while (is(TokenKind::STAR) || is(TokenKind::XOR)) {
    const bool currentPointerIsUnique =
        this->currentTokenKind() == TokenKind::XOR;
    indirectionLevel = advancePointerType(currentPointerIsUnique);
    if (currentPointerIsUnique) {
      isUniquePtr = true;
    }
  }

  if (is(TokenKind::AND)) {
    indirectionLevel = 1;
    isRef = true;
    this->advance();
  }

  return std::make_unique<TypeAST>(TypeSpecifier::SPEC_DEFINED,
                                   std::move(symbol), isUniquePtr, true, isRef,
                                   indirectionLevel, semLoc);
}

std::string CStarParser::advanceDefinedTypeName() {
  expected(TokenKind::IDENT);
  auto firstName = currentTokenStr();
  this->advance();

  if (!is(TokenKind::DOT)) {
    return firstName;
  }

  auto dotInfo = currentTokenInfo();
  if (std::find(m_ModuleAliases.begin(), m_ModuleAliases.end(), firstName) ==
      m_ModuleAliases.end()) {
    ParserError("Qualified type names must start with an include alias",
                dotInfo);
  }

  this->advance();
  expected(TokenKind::IDENT);
  auto typeName = currentTokenStr();
  this->advance();

  return typeName;
}
