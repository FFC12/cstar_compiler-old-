#include <parser/parser.hpp>

void CStarParser::varDecl(bool isDefinedType) {
  TokenKind type = TokenKind::VOID;

  if (isDefinedType) {
    // will be passed to the VarDeclAST;
    auto symbol = this->advanceSymbol();
    type = TokenKind::IDENT;
  } else {
    // FLOAT | INT | ...
    type = this->currentTokenKind();

    // advance type
    this->advance();
  }

not_needed_type:
  ASTNode rhs = nullptr, arrayRhs;

  //* | ^
  while (is(TokenKind::STAR) || is(TokenKind::XOR)) {
    size_t indirection_level =
        advancePointerType(this->currentTokenKind() == TokenKind::XOR);
    // std::cout << "Indirection level: " << indirection_level << "\n";
  }

  // expect ident (name of the variable)
  expected(TokenKind::IDENT);

  // get name of the variable
  std::string name = currentTokenStr();

  // advance name
  this->advance();

  std::vector<ASTNode> arrayDimensions;
  bool arrayFlag = false;

  if (is(TokenKind::LSQPAR)) {
    // [
    this->advance();

    auto cond = is(SCALARI) || is(IDENT);
    if (!cond) {
      expected({SCALARI, IDENT});
    }

    while (cond) {
      if (is(SCALARI)) {
        arrayDimensions.emplace_back(
            std::move(this->advanceConstantOrLiteral()));
      } else {
        arrayDimensions.emplace_back(std::move(this->advanceSymbol()));
      }

      if (!is(TokenKind::RSQPAR)) {
        expected(TokenKind::COLON);
        this->advance();
      } else {
        expected(TokenKind::RSQPAR);
        this->advance();
        break;
      }
    }

    arrayFlag = true;
  }

  // This is an AssignmentExpr and then will take potentially a BinopExpr and
  // then will take any other expressions like ConstExpr or *Expr .
  if (is(TokenKind::EQUAL)) {
    // advance the value or expression..
    rhs = std::move(this->initializer());

    // rhs->debugNode();
    // std::cout << std::endl;
  }

  if (is(TokenKind::COMMA)) {
    // advance comma
    this->advance();
    auto tokenPos = currentTokenInfo().getTokenPositionInfo();
    auto semLoc = SemanticLoc(tokenPos.begin, tokenPos.end, tokenPos.line);

    // ASTNode ast = std::unique_ptr<VarAST>(new VarAST(name, std::move(rhs),
    // TypeSpecifier::SPEC_I8, VisibilitySpecifier::VIS_EXPORT));
    auto ast =
        std::make_unique<VarAST>(name, std::move(rhs), TypeSpecifier::SPEC_I8,
                                 VisibilitySpecifier::VIS_EXPORT, semLoc);

    // Will be pushed into the AST that VarAST
    this->m_AST.push_back(std::move(ast));

    // advance to next symbol
    goto not_needed_type;
  } else {
    // we expect the SEMICOLON..
    expected(TokenKind::SEMICOLON);

    auto tokenPos = currentTokenInfo().getTokenPositionInfo();
    auto semLoc = SemanticLoc(tokenPos.begin, tokenPos.end, tokenPos.line);

    // ASTNode ast = std::unique_ptr<VarAST>(new VarAST(name, std::move(rhs),
    // TypeSpecifier::SPEC_I8, VisibilitySpecifier::VIS_EXPORT));
    auto ast =
        std::make_unique<VarAST>(name, std::move(rhs), TypeSpecifier::SPEC_I8,
                                 VisibilitySpecifier::VIS_EXPORT, semLoc);

    // Will be pushed into the AST that VarAST
    this->m_AST.push_back(std::move(ast));
  }

  this->advance();
}

ASTNode CStarParser::initializer() {
  /*  // '{'
  bool outOfSize = false;
  auto nextToken = nextTokenInfo(outOfSize).getTokenKind();
  if (outOfSize) {
    ParserError(
        "Unexpected token " + std::string(tokenToStr(currentTokenKind())),
        currentTokenInfo());
  }

  if (nextToken == TokenKind::LBRACK) {
    // advance '{'
    this->advance();
    return std::move(this->initializerList());
  }*/

  return std::move(this->expression(false));
}

ASTNode CStarParser::initializerList() {
  this->advance();
  auto el = std::move(this->expression(false));
  ASTNode el2 = nullptr;
  while (is(TokenKind::COMMA)) {
    el2 = std::move(this->expression(true));
  }
  expected(TokenKind::RBRACK);
}

size_t CStarParser::advancePointerType(bool isUniquePtr) {
  size_t level = 1;
  TokenKind pointerType = TokenKind::STAR;

  if (isUniquePtr) {
    expected(TokenKind::XOR);
    pointerType = TokenKind::XOR;
  } else {
    expected(TokenKind::STAR);
    pointerType = TokenKind::STAR;
  }

  this->advance();

  while (is(pointerType)) {
    this->advance();

    if (pointerType == TokenKind::STAR &&
        this->currentTokenKind() == TokenKind::XOR) {
      ParserError(
          "Next pointer type [^] is not matching with type of before "
          "level's [*]. This is not supported yet!",
          currentTokenInfo());
    } else if (pointerType == TokenKind::XOR &&
               this->currentTokenKind() == TokenKind::STAR) {
      ParserError(
          "Next pointer type [*] is not matching with type of before "
          "level's [^]. This is not supported yet!",
          currentTokenInfo());
    }

    level += 1;
  }

  return level;
}
