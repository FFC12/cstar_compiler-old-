#include <parser/parser.hpp>

static bool IsValueOperatorToken(TokenKind kind) {
  switch (kind) {
    case PLUS:
    case MINUS:
    case STAR:
    case DIV:
    case MOD:
    case EQUALEQUAL:
    case NOTEQUAL:
    case LT:
    case LTEQ:
    case GT:
    case GTEQ:
      return true;
    default:
      return false;
  }
}

ASTNode CStarParser::advanceDefinedType() {
  expected(TokenKind::IDENT);

  auto tokenPos = currentTokenInfo().getTokenPositionInfo();
  auto semLoc = SemanticLoc(tokenPos.begin, tokenPos.end, tokenPos.line);
  auto symbol =
      std::make_unique<SymbolAST>(currentTokenStr(), semLoc);
  this->advance();

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
  if (is(TokenKind::COLONCOLON)) {
    this->advance();

    if (isTypeQualifier(currentTokenInfo())) {
      retTypeQualifier = typeQualifierOf(currentTokenInfo());
      this->advance();
    }

    if (isType(currentTokenInfo())) {
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
      declarationModifiers.isStatic, isVariadic, declarationModifiers.access,
      semLoc);

  this->m_AST.emplace_back(std::move(func));
}

void CStarParser::advanceParams(std::vector<ASTNode>& params,
                                bool isForwardDecl, bool& isVariadic) {
param_again:
  if (is(TokenKind::TRIPLET)) {
    isVariadic = true;
    this->advance();
    if (!is(TokenKind::RPAREN)) {
      ParserError("Variadic marker '...' must be the last parameter",
                  currentTokenInfo());
    }
    return;
  }

  ASTNode typeNode;
  TypeQualifier typeQualifier = TypeQualifier::Q_NONE;
  bool isNoMove = false;
  ASTNode symbol0, symbol1;

  if (is(TokenKind::NOMOVE)) {
    isNoMove = true;
    this->advance();
  }

  if (isTypeQualifier(currentTokenInfo())) {
    typeQualifier = typeQualifierOf(currentTokenInfo());
    this->advance();
  }

  if (is(TokenKind::NOMOVE)) {
    isNoMove = true;
    this->advance();
  }

  size_t beginLoc = currentTokenInfo().getTokenPositionInfo().begin;
  size_t line = currentTokenInfo().getTokenPositionInfo().line;

  if (isType(currentTokenInfo())) {  // cast allowed
    auto type = this->advanceType();

    std::vector<ASTNode> arrayDimensions;
    bool arrayFlag = this->advanceTypeSubscript(arrayDimensions);
    if (is(TokenKind::AND)) {
      this->advance();
      auto typeAst = dynamic_cast<TypeAST*>(type.get());
      typeAst->setIsRef(true);
    }

    if (!isForwardDecl) {
      // expected param name
      expected(TokenKind::IDENT);

      // get the param name
      symbol0 = this->advanceSymbol();
    } else if (is(TokenKind::IDENT)) {
      symbol0 = this->advanceSymbol();
    }

    size_t endLoc = currentTokenInfo().getTokenPositionInfo().end;
    auto semLoc = SemanticLoc(beginLoc, endLoc, line);
    if (isForwardDecl && symbol0 == nullptr) {
      symbol0 = std::make_unique<SymbolAST>(
          "__cstar_param" + std::to_string(params.size()), semLoc);
    }
    auto param =
        std::make_unique<ParamAST>(std::move(symbol0), nullptr, std::move(type),
                                   std::move(arrayDimensions), arrayFlag, true,
                                   false, false, true, typeQualifier, semLoc,
                                   isNoMove);
    params.emplace_back(std::move(param));
  } else if (is(TokenKind::IDENT)) {
    bool outOfSize = false;
    auto nextTokenInfo = this->nextTokenInfo(outOfSize);
    if (outOfSize) {
      ParserError("Unexpected token", currentTokenInfo());
    }

    auto nextToken = nextTokenInfo.getTokenKind();
    if (nextToken == TokenKind::STAR || nextToken == TokenKind::XOR ||
        nextToken == TokenKind::LSQPAR) {  // cast allowed
      // %100 defined type
      auto type = this->advanceDefinedType();

      std::vector<ASTNode> arrayDimensions;
      bool arrayFlag = this->advanceTypeSubscript(arrayDimensions);
      if (is(TokenKind::AND)) {
        this->advance();
        auto typeAst = dynamic_cast<TypeAST*>(type.get());
        typeAst->setIsRef(true);
      }

      if (!isForwardDecl) {
        expected(TokenKind::IDENT);
        // get the param name
        symbol0 = std::move(this->advanceSymbol());
      } else if (is(TokenKind::IDENT)) {
        symbol0 = std::move(this->advanceSymbol());
      }

      size_t endLoc = currentTokenInfo().getTokenPositionInfo().end;
      auto semLoc = SemanticLoc(beginLoc, endLoc, line);
      if (isForwardDecl && symbol0 == nullptr) {
        symbol0 = std::make_unique<SymbolAST>(
            "__cstar_param" + std::to_string(params.size()), semLoc);
      }

      auto param = std::make_unique<ParamAST>(
          std::move(symbol0), nullptr, std::move(type),
          std::move(arrayDimensions), arrayFlag, true, false, false, false,
          typeQualifier, semLoc, isNoMove);
      params.emplace_back(std::move(param));
    } else {
      // if the next token is primitive type...
      if (isType(nextTokenInfo)) {  // cast not allowed
        // get the param name
        if (!isForwardDecl) {
          expected(TokenKind::IDENT);
          symbol0 = this->advanceSymbol();
        } else if (is(TokenKind::IDENT)) {
          symbol0 = this->advanceSymbol();
        }

        auto type = this->advanceType();

        std::vector<ASTNode> arrayDimensions;
        bool arrayFlag = this->advanceTypeSubscript(arrayDimensions);
        if (is(TokenKind::AND)) {
          this->advance();
          auto typeAst = dynamic_cast<TypeAST*>(type.get());
          typeAst->setIsRef(true);
        }

        size_t endLoc = currentTokenInfo().getTokenPositionInfo().end;
        auto semLoc = SemanticLoc(beginLoc, endLoc, line);
        if (isForwardDecl && symbol0 == nullptr) {
          symbol0 = std::make_unique<SymbolAST>(
              "__cstar_param" + std::to_string(params.size()), semLoc);
        }

        auto param = std::make_unique<ParamAST>(
            std::move(symbol0), nullptr, std::move(type),
            std::move(arrayDimensions), arrayFlag, false, false, false, true,
            typeQualifier, semLoc, isNoMove);
        params.emplace_back(std::move(param));
      } else {
        auto prevNextTokenInfo = nextTokenInfo;
        outOfSize = false;
        nextTokenInfo = this->nextTokenInfo(outOfSize, 2);
        if (outOfSize) {
          ParserError("Unexpected token", currentTokenInfo());
        }

        nextToken = nextTokenInfo.getTokenKind();
        if (nextToken == TokenKind::STAR || nextToken == TokenKind::XOR ||
            nextToken == TokenKind::LSQPAR) {  // cast allowed
          // expect param name
          if (!isForwardDecl) {
            symbol0 = std::move(this->advanceSymbol());
          }

          expected(TokenKind::IDENT);
          // get the defined type
          auto type = std::move(this->advanceSymbol());
          std::vector<ASTNode> arrayDimensions;
          bool arrayFlag = this->advanceTypeSubscript(arrayDimensions);
          if (is(TokenKind::AND)) {
            this->advance();
            auto typeAst = dynamic_cast<TypeAST*>(type.get());
            typeAst->setIsRef(true);
          }

          size_t endLoc = currentTokenInfo().getTokenPositionInfo().end;
          auto semLoc = SemanticLoc(beginLoc, endLoc, line);

          auto param = std::make_unique<ParamAST>(
              std::move(symbol0), nullptr, std::move(type),
              std::move(arrayDimensions), arrayFlag, false, false, true, false,
              typeQualifier, semLoc, isNoMove);
          params.emplace_back(std::move(param));
        } else {
          if (!isForwardDecl) {
            auto type = this->advanceDefinedType();

            expected(TokenKind::IDENT);
            symbol0 = this->advanceSymbol();

            size_t endLoc = currentTokenInfo().getTokenPositionInfo().end;
            auto semLoc = SemanticLoc(beginLoc, endLoc, line);

            auto param = std::make_unique<ParamAST>(
                std::move(symbol0), nullptr, std::move(type),
                std::vector<ASTNode>(), false, false, false, false, false,
                typeQualifier, semLoc, isNoMove);
            params.emplace_back(std::move(param));
          }
        }
      }
    }

  } else {
    if (!is(TokenKind::RPAREN))
      ParserError("Expected a primitive or defined type", currentTokenInfo());
  }

  if (is(TokenKind::COMMA)) {
    // skip ','
    this->advance();
    if (isVariadic) {
      ParserError("Unexpected parameter after variadic marker '...'",
                  currentTokenInfo());
    }
    goto param_again;
  }
}

void CStarParser::advanceScope(std::vector<ASTNode>& scope) {
  this->advance();

  while (!is(TokenKind::RBRACK)) {
    TypeQualifier typeQualifier = TypeQualifier::Q_NONE;
    bool hasConstness = false;

    if (isDeclarationModifier(currentTokenInfo())) {
      parseDeclarationModifiers(true);
    }

    if (isTypeQualifier(currentTokenInfo())) {
      hasConstness = true;
      switch (currentTokenKind()) {
        case CONST:
          typeQualifier = TypeQualifier::Q_CONST;
          break;
        case CONSTPTR:
          typeQualifier = TypeQualifier::Q_CONSTPTR;
          break;
        case CONSTREF:
          typeQualifier = TypeQualifier::Q_CONSTREF;
          break;
        case READONLY:
          typeQualifier = TypeQualifier::Q_READONLY;
          break;
        default:
          assert(false && "Unreacheable");
      }
      this->advance();
    }

    if (isType(currentTokenInfo()) || is(TokenKind::IDENT) ||
        is(TokenKind::SELF) ||
        is(TokenKind::STAR) || is(TokenKind::DEREF) ||
        is(TokenKind::PLUSPLUS) || is(TokenKind::MINUSMINUS)) {
      bool outOfSize = false;
      auto nextTokenInfo = this->nextTokenInfo(outOfSize);
      auto nextToken = nextTokenInfo.getTokenKind();
      if (outOfSize) {
        ParserError("Incomplete declaration or expression", currentTokenInfo());
      }

      size_t begin = currentTokenInfo().getTokenPositionInfo().begin;
      size_t line = currentTokenInfo().getTokenPositionInfo().line;

      bool deref = false;
      size_t dereferencedLevel = 0;
      if (is(TokenKind::DEREF) || is(TokenKind::STAR)) {
        dereferencedLevel = 1;

        bool derefKeywordUsed = false;
        if (is(TokenKind::DEREF)) derefKeywordUsed = true;

        while (is(TokenKind::DEREF) || is(TokenKind::STAR)) {
          this->advance();

          if (is(TokenKind::DEREF) && !derefKeywordUsed) {
            derefKeywordUsed = true;
          } else if (is(TokenKind::DEREF) && derefKeywordUsed) {
            ParserError("'deref' keyword cannot be used more than one",
                        currentTokenInfo());
          }

          nextTokenInfo = this->nextTokenInfo(outOfSize);
          nextToken = nextTokenInfo.getTokenKind();
          if (outOfSize) {
            ParserError("Incomplete declaration or expression",
                        currentTokenInfo());
          }

          if (is(TokenKind::DEREF) || is(TokenKind::STAR))
            dereferencedLevel += 1;

          deref = true;
        }
      }

      if (isType(currentTokenInfo()) ||
          (!(is(TokenKind::PLUSPLUS) || is(TokenKind::MINUSMINUS)) &&
           (nextToken == TokenKind::IDENT || nextToken == TokenKind::STAR ||
            nextToken == TokenKind::XOR || nextToken == TokenKind::AND)) ||
          (isTypeQualifier(prevTokenInfo()) && hasConstness)) {
        DeclarationModifiers localModifiers;
        localModifiers.linkage = VisibilitySpecifier::VIS_LOCAL;
        varDecl(typeQualifier, localModifiers,
                is(TokenKind::IDENT), true, &scope);
      } else if (nextToken == TokenKind::POLICY_ASSIGN) {
        ParserError(
            "'.=' policy/member-safe assignment is part of the C* proposal "
            "and requires the policy runtime semantics first",
            nextTokenInfo);
      } else if ((is(TokenKind::IDENT) || is(TokenKind::SELF)) &&
                 (nextToken == TokenKind::DOT ||
                  nextToken == TokenKind::COLONCOLON)) {
        auto fieldAccess = advanceFieldAccessChain(begin, line);

        if (is(TokenKind::LPAREN)) {
          ASTNode args = nullptr;
          bool callOutOfSize = false;
          auto tokenAfterOpen =
              this->nextTokenInfo(callOutOfSize).getTokenKind();
          if (callOutOfSize) {
            ParserError("Incomplete function call expression",
                        currentTokenInfo());
          }

          if (tokenAfterOpen == TokenKind::RPAREN) {
            this->advance();
            this->advance();
          } else {
            args = std::move(this->expression(true, 1));
          }

          size_t end = currentTokenInfo().getTokenPositionInfo().end;
          SemanticLoc semanticLoc = SemanticLoc(begin, end, line);

          auto expr = std::make_unique<FuncCallAST>(
              std::move(fieldAccess), nullptr, std::move(args), semanticLoc);

          expected(TokenKind::SEMICOLON);
          this->advance();

          scope.emplace_back(std::move(expr));
          continue;
        }

        if (is(TokenKind::POLICY_ASSIGN)) {
          ParserError(
              "'.=' policy/member-safe assignment is part of the C* proposal "
              "and requires the policy runtime semantics first",
              currentTokenInfo());
        }

        if (!isShortcutOp(currentTokenInfo())) {
          ParserError("Unexpected token for assignment expression.",
                      currentTokenInfo());
        }

        auto shortcutOp = typeOfShortcutOp(currentTokenInfo());
        auto shortcutOpStr = currentTokenStr();
        auto expr = std::move(this->expression(false, 0, false, false, true));

        expected(TokenKind::SEMICOLON);
        this->advance();

        size_t end = currentTokenInfo().getTokenPositionInfo().end;
        SemanticLoc semanticLoc = SemanticLoc(begin, end, line);

        auto assignmentExpr = std::make_unique<AssignmentAST>(
            std::move(fieldAccess), std::move(expr), false, 0, shortcutOp,
            shortcutOpStr, semanticLoc);

        scope.emplace_back(std::move(assignmentExpr));
      } else if (isShortcutOp(nextTokenInfo)) {
        auto symbol = std::move(advanceSymbol());
        auto shortcutOp = typeOfShortcutOp(currentTokenInfo());
        auto shortcutOpStr = currentTokenStr();
        auto expr = std::move(this->expression(false, 0, false, false, true));

        expected(TokenKind::SEMICOLON);
        this->advance();

        size_t end = currentTokenInfo().getTokenPositionInfo().end;
        SemanticLoc semanticLoc = SemanticLoc(begin, end, line);

        auto assignmentExpr = std::make_unique<AssignmentAST>(
            std::move(symbol), std::move(expr), deref, dereferencedLevel,
            shortcutOp, shortcutOpStr, semanticLoc);

        scope.emplace_back(std::move(assignmentExpr));
      } else if (nextToken == LSQPAR) {
        auto symbol = std::move(advanceSymbol());

        expected(TokenKind::LSQPAR);

        // advance '['
        this->advance();

        expected({TokenKind::SCALARI, TokenKind::IDENT});

        std::vector<ASTNode> indexes{};
        auto index = is(TokenKind::SCALARI) ? this->advanceConstantOrLiteral()
                                            : this->advanceSymbol();
        indexes.emplace_back(std::move(index));

        while (is(TokenKind::COLON)) {
          // advance ':'
          this->advance();

          expected({TokenKind::SCALARI, TokenKind::IDENT});

          index = is(TokenKind::SCALARI) ? this->advanceConstantOrLiteral()
                                         : this->advanceSymbol();

          indexes.emplace_back(std::move(index));
        }

        expected(TokenKind::RSQPAR);
        this->advance();

        if (is(TokenKind::POLICY_ASSIGN)) {
          ParserError(
              "'.=' policy/member-safe assignment is part of the C* proposal "
              "and requires the policy runtime semantics first",
              currentTokenInfo());
        }

        if (!isShortcutOp(currentTokenInfo())) {
          ParserError("Unexpected token for assignment expression.",
                      currentTokenInfo());
        }

        auto shortcutOp = typeOfShortcutOp(currentTokenInfo());
        auto shortcutOpStr = currentTokenStr();
        auto expr = std::move(this->expression(false, 0, false, false, true));

        expected(TokenKind::SEMICOLON);
        this->advance();

        size_t end = currentTokenInfo().getTokenPositionInfo().end;
        SemanticLoc semanticLoc = SemanticLoc(begin, end, line);

        auto assignmentExpr = std::make_unique<AssignmentAST>(
            std::move(symbol), std::move(expr), deref, dereferencedLevel,
            std::move(indexes), shortcutOp, shortcutOpStr, semanticLoc);
        scope.emplace_back(std::move(assignmentExpr));
      } else if (is(TokenKind::IDENT) && nextToken == TokenKind::LPAREN) {
        auto funcSymbol = std::move(this->advanceSymbol());

        expected(TokenKind::LPAREN);
        ASTNode args = nullptr;
        bool callOutOfSize = false;
        auto tokenAfterOpen = this->nextTokenInfo(callOutOfSize).getTokenKind();
        if (callOutOfSize) {
          ParserError("Incomplete function call expression",
                      currentTokenInfo());
        }

        if (tokenAfterOpen == TokenKind::RPAREN) {
          this->advance();
          this->advance();
        } else {
          args = std::move(this->expression(true, 1));
        }

        size_t end = currentTokenInfo().getTokenPositionInfo().end;
        SemanticLoc semanticLoc = SemanticLoc(begin, end, line);

        auto expr = std::make_unique<FuncCallAST>(
            std::move(funcSymbol), nullptr, std::move(args), semanticLoc);

        expected(TokenKind::SEMICOLON);
        this->advance();

        scope.emplace_back(std::move(expr));
      } else if (is(TokenKind::IDENT) || is(TokenKind::MINUSMINUS) ||
                 is(TokenKind::PLUSPLUS)) {
        PositionInfo posInfo = currentTokenInfo().getTokenPositionInfo();
        begin = posInfo.begin;
        line = posInfo.line;

        bool postfix = false, prefix = false;
        bool increment = false, decrement = false;
        std::string name;
        if (is(TokenKind::MINUSMINUS) || is(TokenKind::PLUSPLUS)) {
          increment = is(TokenKind::PLUSPLUS);
          decrement = is(TokenKind::MINUSMINUS);
          prefix = true;

          if (nextToken == TokenKind::IDENT) {
            this->advance();
            name = currentTokenStr();
            this->advance();
          }
        } else if (is(TokenKind::IDENT)) {
          name = currentTokenStr();

          if (nextToken == TokenKind::MINUSMINUS ||
              nextToken == TokenKind::PLUSPLUS) {
            postfix = true;
            this->advance();
            increment = is(TokenKind::PLUSPLUS);
            decrement = is(TokenKind::MINUSMINUS);
            this->advance();
          }
        }
        expected(SEMICOLON);
        this->advance();

        posInfo = currentTokenInfo().getTokenPositionInfo();
        size_t end = posInfo.end;
        SemanticLoc semLoc = SemanticLoc(begin, end, line);

        auto symbol = std::make_unique<SymbolAST>(name, semLoc);

        auto fixExpr = std::make_unique<FixAST>(
            std::move(symbol), prefix, postfix, increment, decrement, semLoc);
        scope.emplace_back(std::move(fixExpr));
      }
    } else {
      while (is(TokenKind::LINEFEED) || is(TokenKind::COMMENT)) {
        advance();
      }

      if (is(TokenKind::DROP)) {
        PositionInfo posInfo = currentTokenInfo().getTokenPositionInfo();
        size_t begin = posInfo.begin;
        size_t line = posInfo.line;

        this->advance();
        expected(TokenKind::IDENT);
        auto symbolName = currentTokenStr();
        this->advance();

        posInfo = currentTokenInfo().getTokenPositionInfo();
        SemanticLoc semLoc = SemanticLoc(begin, posInfo.end, line);

        expected(TokenKind::SEMICOLON);
        this->advance();
        scope.emplace_back(std::make_unique<DropStmtAST>(symbolName, semLoc));
      } else if (is(TokenKind::RET)) {
        ASTNode retExpr;
        bool noReturn = false;
        bool outOfSize = false;
        auto nextToken = nextTokenInfo(outOfSize).getTokenKind();
        if (outOfSize) {
          ParserError("Unexpected token", currentTokenInfo());
        }

        PositionInfo posInfo = currentTokenInfo().getTokenPositionInfo();
        size_t begin = posInfo.begin;
        size_t line = posInfo.line;

        if (nextToken == TokenKind::SEMICOLON) {
          // empty ret
          retExpr = nullptr;
          noReturn = true;
          this->advance();
        } else {
          retExpr = std::move(this->expression(false, 0, true));
        }

        posInfo = currentTokenInfo().getTokenPositionInfo();
        size_t end = posInfo.end;
        SemanticLoc semLoc = SemanticLoc(begin, end, line);

        auto retAst =
            std::make_unique<RetAST>(std::move(retExpr), noReturn, semLoc);

        expected(TokenKind::SEMICOLON);
        this->advance();
        scope.emplace_back(std::move(retAst));
      } else if (is(TokenKind::IF)) {
        this->advanceIfStmt(scope);
      } else if (is(TokenKind::LOOP)) {
        this->advanceLoopStmt(scope);
      } else if (is(TokenKind::OPTION)) {
        ParserHint(
            "`option` is the C* value-match statement proposal. Planned "
            "syntax is `option (value) { pattern: { ... }, _: { ... } }`; "
            "it is not lowered by this compiler yet.",
            currentTokenInfo());
        ParserError(
            "`option`/match statements are part of the C* proposal, but "
            "parser AST and codegen are not implemented yet.",
            currentTokenInfo());
      } else if (is(TokenKind::BREAK) || is(TokenKind::CONTINUE)) {
        const bool isBreak = is(TokenKind::BREAK);
        PositionInfo posInfo = currentTokenInfo().getTokenPositionInfo();
        SemanticLoc semLoc =
            SemanticLoc(posInfo.begin, posInfo.end, posInfo.line);

        this->advance();
        expected(TokenKind::SEMICOLON);
        this->advance();

        if (isBreak) {
          scope.emplace_back(std::make_unique<BreakStmtAST>(semLoc));
        } else {
          scope.emplace_back(std::make_unique<ContinueStmtAST>(semLoc));
        }
      } else {
        // ParserError("Unexpected token", currentTokenInfo());
      }

      // option - case
    }
  }

  // advance '}'
  this->advance();
}
