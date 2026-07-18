#include <parser/parser.hpp>

void CStarParser::varDecl(TypeQualifier typeQualifier,
                          DeclarationModifiers declarationModifiers,
                          bool isDefinedType, bool isLocal,
                          std::vector<ASTNode>* scope) {
  TypeSpecifier type = TypeSpecifier::SPEC_I8;
  ASTNode definedTypeSymbol;
  bool isDynamicTraitObject = false;
  ASTNode rhs = nullptr, arrayRhs;
  size_t indirectionLevel = 0;
  bool isUnique = false;
  bool isRef = false;
  bool isNullable = false;
  bool isMoveInit = false;
  std::vector<std::string> stateQualifiers;

  auto begin = currentTokenInfo().getTokenPositionInfo().begin;
  if (!is(TokenKind::DYNAMIC)) {
    stateQualifiers = collectStateQualifiersBeforeType();
    isDefinedType = isDefinedType || !stateQualifiers.empty();
  }

  if (is(TokenKind::DYNAMIC)) {
    auto dynamicType = advanceDynamicTraitType();
    auto *typeAst = static_cast<TypeAST *>(dynamicType.get());
    type = TypeSpecifier::SPEC_DEFINED;
    isDynamicTraitObject = true;
    indirectionLevel = typeAst->indirectLevel();
    isUnique = typeAst->isUniquePtr();
    isRef = typeAst->isRef();
    isNullable = typeAst->isNullable();
    if (typeAst->symbol() != nullptr &&
        typeAst->symbol()->getExprKind() == ExprKind::SymbolExpr) {
      auto *typeSymbol = static_cast<SymbolAST *>(typeAst->symbol().get());
      auto semLoc = typeSymbol->getSemLoc();
      definedTypeSymbol =
          std::make_unique<SymbolAST>(typeSymbol->name(), semLoc);
    }
  } else if (isDefinedType) {
    // will be passed to the VarDeclAST;
    auto tokenPos = currentTokenInfo().getTokenPositionInfo();
    auto semLoc = SemanticLoc(tokenPos.begin, tokenPos.end, tokenPos.line);
    definedTypeSymbol =
        std::make_unique<SymbolAST>(advanceDefinedTypeName(), semLoc);

    type = TypeSpecifier::SPEC_DEFINED;
  } else {
    // FLOAT | INT | ...
    type = typeSpecifierOf(currentTokenInfo());

    // advance type
    this->advance();
  }


  not_needed_type:

  //* | ^
  // TODO: while -> if
  while (is(TokenKind::STAR) || is(TokenKind::XOR)) {
    const bool currentPointerIsUnique =
        this->currentTokenKind() == TokenKind::XOR;
    indirectionLevel = advancePointerType(currentPointerIsUnique);
    isNullable = m_LastPointerTypeNullable;

    if (currentPointerIsUnique) isUnique = true;
    // std::cout << "Indirection level: " << indirection_level << "\n";
  }

  if (is(TokenKind::AND)) {
    this->advance();
    isRef = true;
    if (is(TokenKind::QMARK)) {
      ParserError("References cannot be nullable; use an explicit pointer type",
                  currentTokenInfo());
    }
  }

  // expect ident (name of the variable). `state` is a contextual keyword and
  // remains a valid declarator name outside protocol-state syntax.
  if (!is(TokenKind::IDENT) && !is(TokenKind::STATE)) {
    expected(TokenKind::IDENT);
  }

  // get name of the variable
  std::string name = currentTokenStr();

  // advance name
  this->advance();

  std::vector<ASTNode> arrayDimensions;
  bool arrayFlag = this->advanceTypeSubscript(arrayDimensions);

  // This is an AssignmentExpr and then will take potentially a BinopExpr and
  // then will take any other expressions like ConstExpr or *Expr .
  if (is(TokenKind::EQUAL) || is(TokenKind::TYPEINF)) {
    isMoveInit = is(TokenKind::TYPEINF);
    // advance the value or expression..
    rhs = std::move(this->initializer(arrayFlag));

    // rhs->debugNode();
    // std::cout << std::endl;
  }

  if (is(TokenKind::COMMA)) {
    auto tokenPos = currentTokenInfo().getTokenPositionInfo();
    auto semLoc = SemanticLoc(begin, tokenPos.end - 1, tokenPos.line);

    // advance comma
    this->advance();

    // ASTNode ast = std::unique_ptr<VarAST>(new VarAST(name, std::move(rhs),
    // TypeSpecifier::SPEC_I8, VisibilitySpecifier::VIS_EXPORT));
    auto ast = std::make_unique<VarAST>(
        name, std::move(definedTypeSymbol), std::move(rhs), type, typeQualifier,
        declarationModifiers.linkage, declarationModifiers.access,
        declarationModifiers.isStatic,
        indirectionLevel, isRef, isUnique, isLocal, arrayFlag,
        std::move(arrayDimensions), semLoc, isMoveInit, isNullable,
        isDynamicTraitObject, stateQualifiers);

    ast->setDeclKind(getDeclKind(declarationModifiers.linkage));

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
        name, std::move(definedTypeSymbol),std::move(rhs), type, typeQualifier,
        declarationModifiers.linkage, declarationModifiers.access,
        declarationModifiers.isStatic,
        indirectionLevel, isRef, isUnique, isLocal, arrayFlag,
        std::move(arrayDimensions), semLoc, isMoveInit, isNullable,
        isDynamicTraitObject, stateQualifiers);

    ast->setDeclKind(getDeclKind(declarationModifiers.linkage));

    this->advance();
    if (isLocal) {
      scope->emplace_back(std::move(ast));
    } else {
      this->m_AST.emplace_back(std::move(ast));
    }
  }
}

bool CStarParser::advanceTypeSubscript(std::vector<ASTNode>& arrayDimensions) {
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

  return arrayFlag;
}

static bool IsInitializerTrivia(TokenKind kind) {
  return kind == TokenKind::COMMENT || kind == TokenKind::LINEFEED;
}

ASTNode CStarParser::advanceArrayInitializerElement() {
  while (IsInitializerTrivia(currentTokenKind())) {
    this->advance();
  }

  if (is(TokenKind::LPAREN)) {
    return advanceArrayInitializerList();
  }

  if (is(TokenKind::SCALARD) || is(TokenKind::SCALARI) ||
      is(TokenKind::LETTER) || is(TokenKind::LITERAL) || is(TokenKind::TRUE) ||
      is(TokenKind::FALSE) || is(TokenKind::NIL)) {
    return advanceConstantOrLiteral();
  }

  if (is(TokenKind::IDENT)) {
    return advanceSymbol();
  }

  ParserError("Unexpected token '" + currentTokenInfo().getTokenAsStr() +
                  "' in array initializer",
              currentTokenInfo());
  return nullptr;
}

ASTNode CStarParser::advanceArrayInitializerList() {
  auto openToken = currentTokenInfo();
  expected(TokenKind::LPAREN);
  this->advance();

  ASTNode result = nullptr;
  while (true) {
    while (IsInitializerTrivia(currentTokenKind())) {
      this->advance();
    }

    if (is(TokenKind::RPAREN)) {
      break;
    }

    if (is(TokenKind::_EOF) || is(TokenKind::SEMICOLON)) {
      ParserError("Array initializer is missing ')'", openToken);
    }

    auto element = advanceArrayInitializerElement();
    if (element == nullptr) {
      ParserError("Array initializer element expected", currentTokenInfo());
    }

    if (result == nullptr) {
      result = std::move(element);
    } else {
      auto lhsBegin = result->getSemLoc().begin;
      auto tokenPos = currentTokenInfo().getTokenPositionInfo();
      auto semLoc = SemanticLoc(lhsBegin, tokenPos.end, tokenPos.line);
      std::string op = ",";
      result = std::make_unique<BinaryOpAST>(
          std::move(result), std::move(element), nullptr, BinOpKind::B_COMM,
          op, semLoc);
    }

    while (IsInitializerTrivia(currentTokenKind())) {
      this->advance();
    }

    if (is(TokenKind::COMMA)) {
      this->advance();
      continue;
    }

    if (is(TokenKind::RPAREN)) {
      break;
    }

    ParserError("Unexpected token '" + currentTokenInfo().getTokenAsStr() +
                    "' in array initializer; expected ',' or ')'",
                currentTokenInfo());
  }

  expected(TokenKind::RPAREN);
  this->advance();

  if (result == nullptr) {
    ParserError("Array initializer cannot be empty", openToken);
  }

  return result;
}

ASTNode CStarParser::initializer(bool isArrayInitializer) {
  if (isArrayInitializer) {
    this->advance();
    while (IsInitializerTrivia(currentTokenKind())) {
      this->advance();
    }

    if (is(TokenKind::LPAREN)) {
      return advanceArrayInitializerList();
    }

    ParserError("Array initializer must use parenthesized values",
                currentTokenInfo());
  }

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
  return el;
}

size_t CStarParser::advancePointerType(bool isUniquePtr) {
  size_t level = 1;
  m_LastPointerTypeNullable = false;
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

  if (is(TokenKind::QMARK)) {
    m_LastPointerTypeNullable = true;
    this->advance();
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
    case VOID:
      return TypeSpecifier::SPEC_VOID;
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
      return TypeSpecifier::SPEC_NIL;
  }
}
