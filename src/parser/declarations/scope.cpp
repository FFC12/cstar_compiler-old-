#include <parser/parser_private.hpp>

using namespace cstar::parser_private;

void CStarParser::advanceScope(std::vector<ASTNode>& scope) {
  this->advance();

  while (!is(TokenKind::RBRACK)) {
    const size_t statementStartIndex = m_TokenIndex;
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
        is(TokenKind::DYNAMIC) ||
        is(TokenKind::SELF) ||
        is(TokenKind::STAR) || is(TokenKind::DEREF) ||
        is(TokenKind::PLUSPLUS) || is(TokenKind::MINUSMINUS)) {
      bool outOfSize = false;
      auto nextTokenInfo = this->nextTokenInfo(outOfSize);
      auto nextToken = nextTokenInfo.getTokenKind();
      if (outOfSize) {
        ParserError("Incomplete declaration or expression", currentTokenInfo());
      }

      bool qualifiedTypeDeclaration = false;
      if (is(TokenKind::IDENT) && nextToken == TokenKind::DOT) {
        bool lookaheadOutOfSize = false;
        const auto typeToken =
            this->nextTokenInfo(lookaheadOutOfSize, 2).getTokenKind();
        const auto afterTypeToken =
            this->nextTokenInfo(lookaheadOutOfSize, 3).getTokenKind();
        if (!lookaheadOutOfSize && typeToken == TokenKind::IDENT) {
          qualifiedTypeDeclaration =
              afterTypeToken == TokenKind::IDENT ||
              afterTypeToken == TokenKind::STAR ||
              afterTypeToken == TokenKind::XOR ||
              afterTypeToken == TokenKind::AND ||
              afterTypeToken == TokenKind::LSQPAR;
        }
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
          is(TokenKind::DYNAMIC) ||
          qualifiedTypeDeclaration ||
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
          ASTNode args = std::move(this->advanceArgumentList());

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

        std::vector<ASTNode> indexes{};
        auto collectSubscriptIndexes = [&](auto &self, ASTNode expr) -> void {
          if (expr == nullptr) {
            return;
          }

          if (expr->getExprKind() == ExprKind::BinOp) {
            auto *binary = static_cast<BinaryOpAST *>(expr.get());
            if (binary->binOpKind() == BinOpKind::B_MARRS) {
              self(self, binary->takeLeft());
              self(self, binary->takeRight());
              return;
            }
          }

          indexes.emplace_back(std::move(expr));
        };

        auto indexExpr = this->expression(true, 3);
        collectSubscriptIndexes(collectSubscriptIndexes, std::move(indexExpr));

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
        ASTNode args = std::move(this->advanceArgumentList());

        size_t end = currentTokenInfo().getTokenPositionInfo().end;
        SemanticLoc semanticLoc = SemanticLoc(begin, end, line);

        auto expr = std::make_unique<FuncCallAST>(
            std::move(funcSymbol), nullptr, std::move(args), semanticLoc);

        expected(TokenKind::SEMICOLON);
        this->advance();

        scope.emplace_back(std::move(expr));
      } else if ((is(TokenKind::IDENT) &&
                  (nextToken == TokenKind::MINUSMINUS ||
                   nextToken == TokenKind::PLUSPLUS)) ||
                 is(TokenKind::MINUSMINUS) || is(TokenKind::PLUSPLUS)) {
        PositionInfo posInfo = currentTokenInfo().getTokenPositionInfo();
        begin = posInfo.begin;
        line = posInfo.line;
        size_t end = posInfo.end;

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
            end = currentTokenInfo().getTokenPositionInfo().end;
            this->advance();
          } else {
            ParserError("Prefix increment/decrement expects a symbol",
                        currentTokenInfo());
          }
        } else if (is(TokenKind::IDENT)) {
          name = currentTokenStr();
          end = currentTokenInfo().getTokenPositionInfo().end;

          if (nextToken == TokenKind::MINUSMINUS ||
              nextToken == TokenKind::PLUSPLUS) {
            postfix = true;
            this->advance();
            increment = is(TokenKind::PLUSPLUS);
            decrement = is(TokenKind::MINUSMINUS);
            end = currentTokenInfo().getTokenPositionInfo().end;
            this->advance();
          }
        }
        expected(SEMICOLON);
        this->advance();

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
      } else if (is(TokenKind::THROW)) {
        PositionInfo posInfo = currentTokenInfo().getTokenPositionInfo();
        size_t begin = posInfo.begin;
        size_t line = posInfo.line;

        bool throwOutOfSize = false;
        if (nextTokenInfo(throwOutOfSize).getTokenKind() ==
            TokenKind::SEMICOLON) {
          ParserError("`throw` expects an error expression", currentTokenInfo());
        }

        auto errorExpr = std::move(this->expression(false, 0, true));
        posInfo = currentTokenInfo().getTokenPositionInfo();
        SemanticLoc semLoc = SemanticLoc(begin, posInfo.end, line);

        expected(TokenKind::SEMICOLON);
        this->advance();
        scope.emplace_back(
            std::make_unique<ThrowStmtAST>(std::move(errorExpr), semLoc));
      } else if (is(TokenKind::DEFER)) {
        PositionInfo posInfo = currentTokenInfo().getTokenPositionInfo();
        size_t begin = posInfo.begin;
        size_t line = posInfo.line;
        this->advance();

        expected(TokenKind::LBRACK);
        std::vector<ASTNode> deferScope;
        this->advanceScope(deferScope);

        posInfo = prevTokenInfo().getTokenPositionInfo();
        SemanticLoc semLoc = SemanticLoc(begin, posInfo.end, line);
        scope.emplace_back(
            std::make_unique<DeferStmtAST>(std::move(deferScope), semLoc));
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
        this->advanceOptionStmt(scope);
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

    if (m_TokenIndex == statementStartIndex && !is(TokenKind::RBRACK) &&
        !is(TokenKind::_EOF)) {
      ParserError("Unexpected token in scope", currentTokenInfo());
    }
  }

  // advance '}'
  this->advance();
}
