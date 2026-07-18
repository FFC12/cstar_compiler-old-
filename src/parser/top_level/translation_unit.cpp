#include <parser/parser_private.hpp>

using namespace cstar::parser_private;

void CStarParser::parseIncludeDirective() {
  auto includeToken = currentTokenInfo();
  this->advance();
  skipTopLevelTrivia();

  if (is(TokenKind::INVOLVED)) {
    this->advance();
    skipTopLevelTrivia();
    expectBlockStart();
    this->advance();
    while (!is(TokenKind::RBRACK)) {
      skipTopLevelTrivia();
      if (is(TokenKind::LITERAL) || is(TokenKind::LETTER) ||
          is(TokenKind::IDENT)) {
        const auto includeName = currentTokenStr();
        auto sourceInclude = ResolveLogicalIncludeSource(includeName);
        if (!sourceInclude.empty()) {
          m_SourceIncludes.emplace_back(std::move(sourceInclude));
        }
        this->advance();
      } else if (!is(TokenKind::RBRACK)) {
        ParserError("Expected include target", currentTokenInfo());
      }

      skipTopLevelTrivia();
      if (is(TokenKind::COMMA)) {
        this->advance();
      }
    }
    this->advance();
    skipTopLevelTrivia();
    if (is(TokenKind::SEMICOLON)) {
      this->advance();
    }
    return;
  }

  if (is(TokenKind::LBRACK)) {
    this->advance();
    while (!is(TokenKind::RBRACK)) {
      skipTopLevelTrivia();
      if (is(TokenKind::LITERAL) || is(TokenKind::LETTER)) {
        auto includeName = currentTokenStr();
        auto sourceInclude = ResolveLogicalIncludeSource(includeName);
        if (!sourceInclude.empty()) {
          m_SourceIncludes.emplace_back(std::move(sourceInclude));
        }
        this->advance();
      } else if (!is(TokenKind::RBRACK)) {
        ParserError("Expected include target", currentTokenInfo());
      }

      skipTopLevelTrivia();
      if (is(TokenKind::COMMA)) {
        this->advance();
      }
    }
    this->advance();
    skipTopLevelTrivia();
    if (is(TokenKind::SEMICOLON)) {
      this->advance();
    }
    return;
  }

  if (is(TokenKind::LITERAL) || is(TokenKind::LETTER)) {
    auto includeName = currentTokenStr();
    auto sourceInclude = ResolveLogicalIncludeSource(includeName);
    if (!sourceInclude.empty()) {
      m_SourceIncludes.emplace_back(std::move(sourceInclude));
    }
    this->advance();
    skipTopLevelTrivia();
    if (is(TokenKind::AS)) {
      this->advance();
      skipTopLevelTrivia();
      expected(TokenKind::IDENT);
      m_ModuleAliases.push_back(currentTokenStr());
      this->advance();
    }
    skipTopLevelTrivia();
    if (is(TokenKind::SEMICOLON)) {
      this->advance();
    }
    return;
  }

  ParserError("Expected include target after 'include'", includeToken);
}

void CStarParser::translationUnit() {
  while (!this->m_ParsingEndingFlag) {
    // std::cout << this->m_CurrToken.getTokenAsStr() << "\n";
    if (this->m_CurrToken.getTokenKind() == TokenKind::_EOF) {
      //      std::cout << "EOF\n";
      break;
    }

    if (is(TokenKind::UNHANDLED)) {
      auto t = this->currentTokenInfo();
      // TODO: delete later on
      assert(false && "Unhandled");
    }

    if (is(TokenKind::COMMENT) || is(TokenKind::LINEFEED)) {
      this->advance();
      continue;
    }

    if (is(TokenKind::AT)) {
      parseAttributeAnnotation();
      continue;
    }

    if (is(TokenKind::HASH)) {
      bool outOfSize = false;
      if (nextTokenInfo(outOfSize).getTokenKind() == TokenKind::LSQPAR) {
        parseLanguageItemAnnotation(true);
        continue;
      }
      parseDirective();
      continue;
    }

    if (isPackageMark(this->m_CurrToken)) {
      parseIncludeDirective();
      continue;
    } else if (IsProposalOnlyTopLevel(currentTokenKind())) {
      ParserHint(
          "This keyword belongs to the C* proposal surface "
          "(struct/trait/protocol/attribute/macro/effects), but its "
          "grammar is not implemented in the compiler yet.",
          currentTokenInfo());
      ParserError("Unexpected proposal keyword '" +
                      std::string(tokenToStr(currentTokenKind())) + "'",
                  currentTokenInfo());
    } else {
      DeclarationModifiers declarationModifiers =
          parseDeclarationModifiers(false);

      skipTopLevelTrivia();
      if (!m_PendingLanguageItem.empty() && !is(TokenKind::TRAIT)) {
        ParserError("language item annotation `lang(" +
                        m_PendingLanguageItem +
                        ")` can only be applied to a trait declaration",
                    currentTokenInfo());
      }

      if (is(TokenKind::ATTRIB)) {
        parseAttributeDecl(declarationModifiers);
        continue;
      }

      if (is(TokenKind::MACRO)) {
        parseMacroDecl(declarationModifiers);
        continue;
      }

      if (is(TokenKind::STRUCT)) {
        parseStructDecl(declarationModifiers);
        continue;
      }

      if (is(TokenKind::TRAIT)) {
        parseTraitDecl(declarationModifiers);
        continue;
      }

      if (is(TokenKind::PROTOCOL)) {
        parseProtocolDecl(declarationModifiers);
        continue;
      }

      if (is(TokenKind::DYNAMIC)) {
        this->advance();
        expected(TokenKind::PROTOCOL);
        parseProtocolDecl(declarationModifiers, true);
        continue;
      }

      if (is(TokenKind::FLAGS)) {
        parseEnumDecl(declarationModifiers, true);
        continue;
      }

      if (is(TokenKind::ENUM)) {
        parseEnumDecl(declarationModifiers);
        continue;
      }

      if ((declarationModifiers.linkage == VisibilitySpecifier::VIS_IMPORT ||
           declarationModifiers.linkage == VisibilitySpecifier::VIS_EXPORT) &&
          (is(TokenKind::LBRACK) || is(TokenKind::FROM))) {
        parseLinkageBlock(declarationModifiers.linkage);
        continue;
      }

      bool outOfSize = false;
      auto nextToken = nextTokenInfo(outOfSize).getTokenKind();
      if (outOfSize) {
        ParserError("Unexpected token", currentTokenInfo());
      }

      if (is(TokenKind::IDENT) && nextToken == TokenKind::LT) {
        bool genericFunc = false;
        size_t lookahead = 2;
        while (m_TokenIndex + lookahead < m_TokenStream.size()) {
          auto kind = m_TokenStream[m_TokenIndex + lookahead].getTokenKind();
          if (kind == TokenKind::GT) {
            if (m_TokenIndex + lookahead + 1 < m_TokenStream.size() &&
                m_TokenStream[m_TokenIndex + lookahead + 1].getTokenKind() ==
                    TokenKind::LPAREN) {
              genericFunc = true;
            }
            break;
          }
          if (kind == TokenKind::SEMICOLON || kind == TokenKind::LINEFEED ||
              kind == TokenKind::_EOF) {
            break;
          }
          lookahead += 1;
        }
        if (genericFunc) {
          nextToken = TokenKind::LPAREN;
        }
      }

      TypeQualifier typeQualifier = TypeQualifier::Q_NONE;

      if (isTypeQualifier(currentTokenInfo())) {
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

      // int* | float* | uint* ...
      // we check that is an IDENT or not since because isType for operators
      // which only contains primitives. IDENT means it's a symbol (which needed
      // to resolved in next phase - Semantic Analysis- )
      if (this->isType(this->m_CurrToken) || is(TokenKind::IDENT) ||
          is(TokenKind::DYNAMIC)) {
        if (is(TokenKind::IDENT) && nextToken != TokenKind::LPAREN) {
          varDecl(typeQualifier, declarationModifiers, is(TokenKind::IDENT),
                  false);
        } else if (is(TokenKind::IDENT) && nextToken == TokenKind::LPAREN) {
          funcDecl(declarationModifiers);
        } else {
          // if it's pointer type or something...
          varDecl(typeQualifier, declarationModifiers, is(TokenKind::IDENT),
                  false);
        }
      } else {
        /*if(is(TokenKind::PROTO)) {
          // advance 'proto'
          this->advance();


        }*/
        // protototype, directive, traits, macro
        ParserHint(
            "Not implemented yet (protototype, directive, traits, macro)",
            currentTokenInfo());
        ParserError("Unexpected token '" +
                        std::string(tokenToStr(currentTokenKind())) + "'",
                    currentTokenInfo());
      }
    }
  }
}
