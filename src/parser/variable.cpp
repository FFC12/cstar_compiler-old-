#include <parser/parser.hpp>

void CStarParser::varDecl(TypeQualifier typeQualifier,
                          VisibilitySpecifier visibilitySpecifier,
                          bool isDefinedType, bool isLocal,
                          std::vector<ASTNode>* scope) {
  TypeSpecifier type = TypeSpecifier::SPEC_I8;

  auto begin = currentTokenInfo().getTokenPositionInfo().begin;
  if (isDefinedType) {
    // will be passed to the VarDeclAST;
    auto symbol = this->advanceSymbol();

    type = TypeSpecifier::SPEC_DEFINED;
  } else {
    // FLOAT | INT | ...
    type = typeSpecifierOf(currentTokenInfo());

    // advance type
    this->advance();
  }


  not_needed_type:
  ASTNode rhs = nullptr, arrayRhs;
  size_t indirectionLevel = 0;
  bool isUnique = false;
  bool isRef = false;

  //* | ^
  // TODO: while -> if
  while (is(TokenKind::STAR) || is(TokenKind::XOR)) {
    indirectionLevel =
        advancePointerType(this->currentTokenKind() == TokenKind::XOR);

    if (this->currentTokenKind() == TokenKind::XOR) isUnique = true;
    // std::cout << "Indirection level: " << indirection_level << "\n";
  }

  if (is(TokenKind::AND)) {
    indirectionLevel = 1;
    this->advance();
    isRef = true;
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
    auto semLoc = SemanticLoc(begin, tokenPos.end, tokenPos.line);

    // ASTNode ast = std::unique_ptr<VarAST>(new VarAST(name, std::move(rhs),
    // TypeSpecifier::SPEC_I8, VisibilitySpecifier::VIS_EXPORT));
    auto ast = std::make_unique<VarAST>(
        name, std::move(rhs), type, typeQualifier, visibilitySpecifier,
        indirectionLevel, isRef, isUnique, isLocal, arrayFlag,
        std::move(arrayDimensions), semLoc);

    ast->setDeclKind(getDeclKind(visibilitySpecifier));

    if (isLocal) {
      scope->emplace_back(std::move(ast));
    } else {
      this->m_AST.emplace_back(std::move(ast));
    }

    // advance to next symbol
    goto not_needed_type;
  } else {
    // we expect the SEMICOLON..
    expected(TokenKind::SEMICOLON);

    auto tokenPos = currentTokenInfo().getTokenPositionInfo();
    auto semLoc = SemanticLoc(begin, tokenPos.end, tokenPos.line);

    // ASTNode ast = std::unique_ptr<VarAST>(new VarAST(name, std::move(rhs),
    // TypeSpecifier::SPEC_I8, VisibilitySpecifier::VIS_EXPORT));
    auto ast = std::make_unique<VarAST>(
        name, std::move(rhs), type, typeQualifier, visibilitySpecifier,
        indirectionLevel, isRef, isUnique, isLocal, arrayFlag,
        std::move(arrayDimensions), semLoc);

    ast->setDeclKind(getDeclKind(visibilitySpecifier));

    this->advance();
    if (isLocal) {
      scope->emplace_back(std::move(ast));
    } else {
      this->m_AST.emplace_back(std::move(ast));
    }
  }
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
          "Next pointer type [^] is not same as type of before "
          "level's [*]. This is not supported yet!",
          currentTokenInfo());
    } else if (pointerType == TokenKind::XOR &&
               this->currentTokenKind() == TokenKind::STAR) {
      ParserError(
          "Next pointer type [*] is not same as type of before "
          "level's [^]. This is not supported yet!",
          currentTokenInfo());
    }

    level += 1;
  }

  if (pointerType == TokenKind::STAR &&
      this->currentTokenKind() == TokenKind::XOR) {
    ParserError(
        "Next pointer type [^] is not same as type of before "
        "level's [*]. This is not supported!",
        currentTokenInfo());
  } else if (pointerType == TokenKind::XOR &&
             this->currentTokenKind() == TokenKind::STAR) {
    ParserError(
        "Next pointer type [*] is not same as type of before "
        "level's [^]. This is not supported!",
        currentTokenInfo());
  }

  return level;
}

DeclKind CStarParser::getDeclKind(VisibilitySpecifier visibilitySpecifier) {
  switch (visibilitySpecifier) {
    case VIS_EXPORT:
      return DeclKind::ExportVarDecl;
    case VIS_IMPORT:
      return DeclKind::ImportVarDecl;
    case VIS_STATIC:
      return DeclKind::GlobVarDecl;
    case VIS_DEFAULT:
    default:
      return DeclKind::VarDecl;
  }
}

TypeSpecifier CStarParser::typeSpecifierOf(const TokenInfo& token) {
  switch (token.getTokenKind()) {
    case I8:
      return TypeSpecifier::SPEC_I8;
    case I16:
      return TypeSpecifier::SPEC_I16;
    case I32:
      return TypeSpecifier::SPEC_I32;
    case I64:
      return TypeSpecifier::SPEC_I64;
    case INT:
      return TypeSpecifier::SPEC_INT;
    case U8:
      return TypeSpecifier::SPEC_U8;
    case U16:
      return TypeSpecifier::SPEC_U16;
    case U32:
      return TypeSpecifier::SPEC_U32;
    case U64:
      return TypeSpecifier::SPEC_U64;
    case U128:
      return TypeSpecifier::SPEC_U128;
    case UINT:
      return TypeSpecifier::SPEC_UINT;
    case ISIZE:
      return TypeSpecifier::SPEC_ISIZE;
    case USIZE:
      return TypeSpecifier::SPEC_USIZE;
    case F32:
      return TypeSpecifier::SPEC_F32;
    case F64:
      return TypeSpecifier::SPEC_F64;
    case FLOAT:
      return TypeSpecifier::SPEC_FLOAT;
    case UCHAR:
      return TypeSpecifier::SPEC_UCHAR;
    case CHAR:
      return TypeSpecifier::SPEC_CHAR;
    case BOOL:
      return TypeSpecifier::SPEC_BOOL;
    case VEC2:
      return TypeSpecifier::SPEC_VEC2;
    case VEC3:
      return TypeSpecifier::SPEC_VEC3;
    case VEC4:
      return TypeSpecifier::SPEC_VEC4;
      // every single IDENT is going to be interpreted as Symbol...
    case IDENT:
      return TypeSpecifier::SPEC_DEFINED;
    default:
      assert(false && "Unreacheable");
  }
}
