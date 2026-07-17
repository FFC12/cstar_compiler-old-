#include <parser/parser_private.hpp>

using namespace cstar::parser_private;

void CStarParser::funcDecl(DeclarationModifiers declarationModifiers,
                           bool forceForwardDecl,
                           const std::string &methodOwner) {
  std::string sourceFuncName = currentTokenStr();
  bool isForwardDecl =
      forceForwardDecl ||
      declarationModifiers.linkage == VisibilitySpecifier::VIS_IMPORT;
  bool isExported =
      declarationModifiers.linkage == VisibilitySpecifier::VIS_EXPORT;

  auto posInfo = currentTokenInfo().getTokenPositionInfo();
  size_t begin = posInfo.begin;
  size_t line = posInfo.line;

  if (is(TokenKind::OPERATOR)) {
    if (methodOwner.empty()) {
      ParserError("operator overloads are only valid inside structs",
                  currentTokenInfo());
    }
    this->advance();
    if (is(TokenKind::NEW) || is(TokenKind::MOVE) || is(TokenKind::DROP) ||
        (is(TokenKind::IDENT) &&
         (currentTokenStr() == "delete" || currentTokenStr() == "copy" ||
          currentTokenStr() == "shared_new" ||
          currentTokenStr() == "shared_delete"))) {
      ParserError(
          "lifecycle/allocation operators are compiler-reserved; use "
          "constructor, destructor, drop, and `new Type(args)`",
          currentTokenInfo());
    }
    if (is(TokenKind::IDENT) && currentTokenStr() == "index") {
      sourceFuncName = "operatorindex";
      this->advance();
    } else if (IsValueOperatorToken(currentTokenKind())) {
      sourceFuncName = "operator" + std::string(currentTokenStr());
      this->advance();
    } else {
      ParserError("Expected value operator after 'operator'",
                  currentTokenInfo());
    }
  } else {
    // advance the name
    this->advance();
  }

  auto funcName = methodOwner.empty()
                      ? sourceFuncName
                      : methodOwner + "." + sourceFuncName;

  if (is(TokenKind::LT)) {
    this->advance();
    if (isType(currentTokenInfo())) {
      auto ignoredType = this->advanceType();
    } else if (is(TokenKind::IDENT)) {
      auto ignoredSymbol = this->advanceSymbol();
    } else {
      ParserError("Expected type attribute after '<'", currentTokenInfo());
    }
    expected(TokenKind::GT);
    this->advance();
  }

  std::vector<ASTNode> params{};
  bool isVariadic = false;
  const bool hasParamList = is(TokenKind::LPAREN);
  if (hasParamList) {
    // '('
    this->advance();

    advanceParams(params, isForwardDecl, isVariadic);

    while (is(TokenKind::LINEFEED) || is(TokenKind::COMMENT)) {
      this->advance();
    }

    expected(TokenKind::RPAREN);

    // ')'
    this->advance();
  } else if (methodOwner.empty()) {
    expected(TokenKind::LPAREN);
  }

  if (!methodOwner.empty() &&
      declarationModifiers.isStatic &&
      (sourceFuncName == "constructor" || sourceFuncName == "destructor")) {
    ParserError("constructor/destructor methods cannot be static",
                currentTokenInfo());
  }

  if (!methodOwner.empty() && sourceFuncName == "new") {
    ParserError(
        "`new` is an allocation operator, not a struct method; use "
        "`new Type(args)` or `new(allocator) Type(args)`",
        currentTokenInfo());
  }

  if (!methodOwner.empty() && !declarationModifiers.isStatic) {
    auto selfLoc = SemanticLoc(begin, begin + methodOwner.size(), line);
    auto selfSymbol = std::make_unique<SymbolAST>("self", selfLoc);
    auto selfTypeSymbol = std::make_unique<SymbolAST>(methodOwner, selfLoc);
    auto selfType = std::make_unique<TypeAST>(
        TypeSpecifier::SPEC_DEFINED, std::move(selfTypeSymbol), false, true,
        true, 1, selfLoc);
    auto selfParam = std::make_unique<ParamAST>(
        std::move(selfSymbol), nullptr, std::move(selfType),
        std::vector<ASTNode>(), false, false, false, false, false,
        TypeQualifier::Q_NONE, selfLoc);
    params.insert(params.begin(), std::move(selfParam));
  }

  if (is(TokenKind::ASYNC)) {
    ParserHint(
        "`async` marks a task/thread boundary in the C* proposal. Its "
        "ownership rules are reserved for the Send/Sync-capability phase and "
        "are not lowered by this compiler yet.",
        currentTokenInfo());
    ParserError(
        "`async` function effects are part of the C* proposal, but async/task "
        "ownership lowering is not implemented yet.",
        currentTokenInfo());
    this->advance();
  }

  ASTNode returnType;
  TypeQualifier retTypeQualifier = TypeQualifier::Q_NONE;
  bool hasExplicitReturnType = false;
  if (is(TokenKind::COLONCOLON)) {
    hasExplicitReturnType = true;
    this->advance();

    if (isTypeQualifier(currentTokenInfo())) {
      retTypeQualifier = typeQualifierOf(currentTokenInfo());
      this->advance();
    }

    if (is(TokenKind::DYNAMIC)) {
      returnType = this->advanceDynamicTraitType();
    } else if (isType(currentTokenInfo())) {
      returnType = this->advanceType();
    } else if (is(TokenKind::IDENT)) {
      returnType = this->advanceDefinedType();
    } else {
      ParserError("Unexpected token", currentTokenInfo());
    }
  } else {  // void by default
    posInfo = this->currentTokenInfo().getTokenPositionInfo();
    SemanticLoc semLoc = SemanticLoc(posInfo.begin, posInfo.end, posInfo.line);
    returnType = std::make_unique<TypeAST>(TypeSpecifier::SPEC_VOID, nullptr,
                                           false, true, false, 0, semLoc);
  }

  skipTopLevelTrivia();
  if (is(TokenKind::FROM)) {
    auto library = parseLinkSource();
    registerNativeLinkLibrary(library);
    isForwardDecl = true;
  }

  if (isExported && is(TokenKind::SEMICOLON)) {
    isForwardDecl = true;
  }

  std::vector<ASTNode> scope{};

  if (!isForwardDecl) {
    expected(TokenKind::LBRACK);

    // func body as scope
    this->advanceScope(scope);
  } else {
    expected(TokenKind::SEMICOLON);
    this->advance();
  }

  posInfo = currentTokenInfo().getTokenPositionInfo();
  size_t end = posInfo.end;
  SemanticLoc semLoc = SemanticLoc(begin, end, line);

  auto func = std::make_unique<FuncAST>(
      funcName, std::move(returnType), std::move(params), std::move(scope),
      retTypeQualifier, isForwardDecl, isExported,
      declarationModifiers.isStatic, isVariadic, hasExplicitReturnType,
      declarationModifiers.access, semLoc);

  this->m_AST.emplace_back(std::move(func));
}
