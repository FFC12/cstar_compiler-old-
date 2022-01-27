#include <parser/parser.hpp>

void CStarParser::varDecl() {
  // FLOAT | INT | ...
  TokenKind type = this->currentTokenKind();

  // advance type
  this->advance();

not_needed_type:
  ASTNode rhs = nullptr;

  //* | ^
  while (is(TokenKind::STAR) || is(TokenKind::XOR)) {
    size_t indirection_level = advancePointerType(
        this->currentTokenKind() == TokenKind::XOR ? true : false);
    std::cout << "Indirection level: " << indirection_level << "\n";
  }

  // expect ident (name of the variable)
  expected(TokenKind::IDENT);

  // get name of the variable
  std::string name = currentTokenStr();

  // advance name
  this->advance();

  // This is an AssignmentExpr and then will take potentially a BinopExpr and
  // then will take any other expressions like ConstExpr or *Expr .
  if (is(TokenKind::EQUAL)) {
    // advance the value or expression..
    rhs = this->initializer();
    auto *expr = static_cast<ScalarAST *>(rhs.get());
    std::cout << expr->getValue() << std::endl;
  }

  if (is(TokenKind::COMMA)) {
    // advance comma
    this->advance();

    // ASTNode ast = std::unique_ptr<VarAST>(new VarAST(name, std::move(rhs),
    // TypeSpecifier::SPEC_I8, VisibilitySpecifier::VIS_EXPORT));
    auto ast =
        std::make_unique<VarAST>(name, std::move(rhs), TypeSpecifier::SPEC_I8,
                                 VisibilitySpecifier::VIS_EXPORT);

    // Will be pushed into the AST that VarAST
    this->m_AST.push_back(std::move(ast));

    // advance to next symbol
    goto not_needed_type;
  } else {
    // we expect the SEMICOLON..
    expected(TokenKind::SEMICOLON);

    // ASTNode ast = std::unique_ptr<VarAST>(new VarAST(name, std::move(rhs),
    // TypeSpecifier::SPEC_I8, VisibilitySpecifier::VIS_EXPORT));
    auto ast =
        std::make_unique<VarAST>(name, std::move(rhs), TypeSpecifier::SPEC_I8,
                                 VisibilitySpecifier::VIS_EXPORT);

    // Will be pushed into the AST that VarAST
    this->m_AST.push_back(std::move(ast));
  }

  this->advance();
}

ASTNode CStarParser::initializer() { return this->expression(); }

ASTNode CStarParser::initializerList() {}

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
      ErrorMessage("Next pointer type [^] is not matching with type before "
                   "level [*]. This is not supported yet!");
    } else if (pointerType == TokenKind::XOR &&
               this->currentTokenKind() == TokenKind::STAR) {
      ErrorMessage("Next pointer type [*] is not matching with type before "
                   "level [^]. This is not supported yet!");
    }

    level += 1;
  }

  return level;
}
