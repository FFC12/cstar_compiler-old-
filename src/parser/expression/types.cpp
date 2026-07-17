#include <parser/parser_private.hpp>

using namespace cstar::parser_private;

ASTNode CStarParser::advanceType() {
  if (this->isType(this->currentTokenInfo())) {
    TokenInfo prevTokenInfo = this->currentTokenInfo();
    size_t indirectionLevel = 0;
    bool isUniquePtr = false;
    bool isNullable = false;

    auto tokenPos = currentTokenInfo().getTokenPositionInfo();
    auto semLoc = SemanticLoc(tokenPos.begin, tokenPos.end, tokenPos.line);

    this->advance();

    bool isRef = false;
    if (is(TokenKind::AND)) {
      isRef = true;
      this->advance();
      semLoc.end += indirectionLevel;
      if (is(TokenKind::QMARK)) {
        ParserError("References cannot be nullable; use an explicit pointer type",
                    currentTokenInfo());
      }
    } else {
      //* | ^
      while (is(TokenKind::STAR) || is(TokenKind::XOR)) {
        isUniquePtr = this->currentTokenKind() == TokenKind::XOR;

        indirectionLevel = advancePointerType(isUniquePtr);
        isNullable = m_LastPointerTypeNullable;
        semLoc.end += indirectionLevel;
        // std::cout << "Type Indirection level: " << indirectionLevel << "\n";
      }
    }

    ASTNode typeAst = std::make_unique<TypeAST>(
        typeSpecifierOf(prevTokenInfo), nullptr, isUniquePtr, true, isRef,
        indirectionLevel, semLoc, isNullable);

    // expected >
    // expected({TokenKind::GT, TokenKind::RPAREN});

    return std::move(typeAst);
  }

  return std::move(this->advanceSymbol());
}

ASTNode CStarParser::advanceDynamicTraitType() {
  expected(TokenKind::DYNAMIC);
  auto startPos = currentTokenInfo().getTokenPositionInfo();
  this->advance();

  expected(TokenKind::IDENT);
  auto traitName = currentTokenStr();
  auto semLoc = SemanticLoc(startPos.begin,
                            currentTokenInfo().getTokenPositionInfo().end,
                            startPos.line);
  auto symbol = std::make_unique<SymbolAST>(traitName, semLoc);
  this->advance();

  size_t indirectionLevel = 0;
  bool isUnique = false;
  bool isRef = false;
  bool isNullable = false;

  if (is(TokenKind::AND)) {
    indirectionLevel = 1;
    isRef = true;
    this->advance();
    if (is(TokenKind::QMARK)) {
      ParserError("References cannot be nullable; use `dynamic Trait*?` or "
                  "`dynamic Trait^?`",
                  currentTokenInfo());
    }
  } else if (is(TokenKind::STAR) || is(TokenKind::XOR)) {
    const bool currentPointerIsUnique =
        this->currentTokenKind() == TokenKind::XOR;
    indirectionLevel = advancePointerType(currentPointerIsUnique);
    isUnique = currentPointerIsUnique;
    isNullable = m_LastPointerTypeNullable;
  } else {
    ParserError("Dynamic trait object requires an explicit ownership marker "
                "(`dynamic Trait&`, `dynamic Trait*`, or `dynamic Trait^`)",
                currentTokenInfo());
  }

  semLoc.end = prevTokenInfo().getTokenPositionInfo().end;
  return std::make_unique<TypeAST>(
      TypeSpecifier::SPEC_DEFINED, std::move(symbol), isUnique, false, isRef,
      indirectionLevel, semLoc, isNullable, true);
}

ASTNode CStarParser::advanceDynamicTraitEraseExpression() {
  expected(TokenKind::DYNAMIC);
  const auto startPos = currentTokenInfo().getTokenPositionInfo();
  this->advance();

  const bool isRefErase = is(TokenKind::REF);
  const bool isMoveErase = is(TokenKind::MOVE);
  if (!isRefErase && !isMoveErase) {
    ParserError("Dynamic trait erasure expects `ref` or `move` after "
                "`dynamic`",
                currentTokenInfo());
  }
  this->advance();

  ASTNode source;
  if (is(TokenKind::IDENT) || is(TokenKind::SELF)) {
    const auto sourcePos = currentTokenInfo().getTokenPositionInfo();
    auto sourceLoc =
        SemanticLoc(sourcePos.begin, sourcePos.end, sourcePos.line);
    source = std::make_unique<SymbolAST>(currentTokenStr(), sourceLoc);
    this->advance();
  } else {
    ParserError("Dynamic trait erasure currently expects a named value before "
                "`as Trait`",
                currentTokenInfo());
  }

  expected(TokenKind::AS);
  this->advance();

  expected(TokenKind::IDENT);
  const auto traitPos = currentTokenInfo().getTokenPositionInfo();
  auto traitLoc = SemanticLoc(traitPos.begin, traitPos.end, traitPos.line);
  auto traitSymbol = std::make_unique<SymbolAST>(currentTokenStr(), traitLoc);
  const auto traitEnd = traitPos.end;
  this->advance();

  auto typeLoc = SemanticLoc(startPos.begin, traitEnd, startPos.line);
  auto targetType = std::make_unique<TypeAST>(
      TypeSpecifier::SPEC_DEFINED, std::move(traitSymbol), isMoveErase, false,
      isRefErase, 1, typeLoc, false, true);

  auto semLoc = SemanticLoc(startPos.begin, traitEnd, startPos.line);
  return std::make_unique<CastNode>(
      std::move(source), std::move(targetType),
      isMoveErase ? CastOpKind::C_DYNAMIC_MOVE_AS
                  : CastOpKind::C_DYNAMIC_REF_AS,
      true, semLoc);
}
