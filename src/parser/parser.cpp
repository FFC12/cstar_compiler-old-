#include <parser/parser.hpp>

#include <algorithm>
#include <filesystem>
#include <string>

static bool IsProposalOnlyTopLevel(TokenKind kind) {
  switch (kind) {
    case DYNAMIC:
    case PROTOCOL:
    case STATE:
    case WITH:
    case TRAIT:
    case MACRO:
    case CONSTRUCTOR:
    case DESTRUCTOR:
    case ALLOCATOR:
    case EXCEPT:
    case THROW:
    case DEFER:
    case ASYNC:
    case AWAIT:
    case SELF:
    case IS:
    case ATTRIB:
    case PROTO:
    case ENUM:
      return true;
    default:
      return false;
  }
}

void CStarParser::parse() {
  m_TokenStream = this->m_Lexer.perform();
  if (m_StatsEnabled) {
    this->m_Lexer.lexerStats();
  }
  this->m_StartTime = time(nullptr);

  if (!this->m_TokenStream.empty()) {
    // auto eof = std::move(m_TokenStream[-1]);
    this->m_CurrToken = m_TokenStream[m_TokenIndex];  //[0]
    this->translationUnit();
  } else {
    assert(false && "Parser token stream is not enough to parse!\n");
  }

  /*for(auto &node: this->m_AST) {
    node->debugNode();
  }*/

  this->parserStats();
}

void CStarParser::skipTopLevelTrivia() {
  while (is(TokenKind::COMMENT) || is(TokenKind::LINEFEED)) {
    this->advance();
  }
}

void CStarParser::registerNativeLinkLibrary(const std::string& library) {
  if (library.empty()) {
    return;
  }

  if (std::find(m_NativeLinkLibraries.begin(), m_NativeLinkLibraries.end(),
                library) == m_NativeLinkLibraries.end()) {
    m_NativeLinkLibraries.push_back(library);
  }
}

std::string CStarParser::parseLinkSource() {
  expected(TokenKind::FROM);
  this->advance();
  skipTopLevelTrivia();

  expected(TokenKind::LITERAL);
  auto source = currentTokenStr();
  this->advance();
  return source;
}

void CStarParser::parseLinkageBlock(
    VisibilitySpecifier visibilitySpecifier) {
  std::string library;
  if (is(TokenKind::FROM)) {
    library = parseLinkSource();
    registerNativeLinkLibrary(library);
    skipTopLevelTrivia();
  }

  expected(TokenKind::LBRACK);
  this->advance();

  while (!is(TokenKind::RBRACK)) {
    skipTopLevelTrivia();
    if (is(TokenKind::RBRACK)) {
      break;
    }
    expected(TokenKind::IDENT);
    DeclarationModifiers modifiers;
    modifiers.linkage = visibilitySpecifier;
    funcDecl(modifiers, true);
    if (!library.empty()) {
      registerNativeLinkLibrary(library);
    }
    skipTopLevelTrivia();
  }

  expected(TokenKind::RBRACK);
  this->advance();
  skipTopLevelTrivia();
  if (is(TokenKind::SEMICOLON)) {
    this->advance();
  }
}

StructFieldInfo CStarParser::parseStructField() {
  DeclarationModifiers modifiers = parseDeclarationModifiers(false);
  if (modifiers.isStatic) {
    ParserError("static struct members are not implemented yet",
                prevTokenInfo());
  }

  StructFieldInfo field;
  field.isPublic = modifiers.access == ACCESS_PUBLIC;

  if (!isType(currentTokenInfo()) && !is(TokenKind::IDENT)) {
    ParserError("Expected field type in struct declaration",
                currentTokenInfo());
  }

  if (is(TokenKind::IDENT)) {
    field.type = TypeSpecifier::SPEC_DEFINED;
    field.definedTypeName = currentTokenStr();
  } else {
    field.type = typeSpecifierOf(currentTokenInfo());
  }
  this->advance();

  while (is(TokenKind::STAR) || is(TokenKind::XOR)) {
    const bool currentPointerIsUnique =
        this->currentTokenKind() == TokenKind::XOR;
    field.indirectionLevel = advancePointerType(currentPointerIsUnique);
    if (currentPointerIsUnique) {
      field.isUnique = true;
    }
  }

  if (is(TokenKind::AND)) {
    field.indirectionLevel = 1;
    field.isRef = true;
    this->advance();
  }

  expected(TokenKind::IDENT);
  field.name = currentTokenStr();
  this->advance();

  if (is(TokenKind::EQUAL) || is(TokenKind::TYPEINF)) {
    ParserError("struct field initializers are not implemented yet",
                currentTokenInfo());
  }

  expected(TokenKind::SEMICOLON);
  this->advance();
  return field;
}

void CStarParser::parseStructDecl(DeclarationModifiers declarationModifiers) {
  if (declarationModifiers.linkage == VisibilitySpecifier::VIS_IMPORT ||
      declarationModifiers.linkage == VisibilitySpecifier::VIS_EXPORT) {
    ParserError("struct visibility uses public/private, not import/export",
                currentTokenInfo());
  }
  if (declarationModifiers.isStatic) {
    ParserError("static struct declarations are not implemented yet",
                currentTokenInfo());
  }

  auto posInfo = currentTokenInfo().getTokenPositionInfo();
  size_t begin = posInfo.begin;
  size_t line = posInfo.line;

  this->advance();
  expected(TokenKind::IDENT);
  auto structName = currentTokenStr();
  this->advance();

  if (is(TokenKind::LT) || is(TokenKind::FROM) || is(TokenKind::WITH)) {
    ParserError(
        "generic/attribute/trait-bound struct syntax is proposal-only; use "
        "`struct Name { field; ... }` for the current MVP",
        currentTokenInfo());
  }

  expected(TokenKind::LBRACK);
  this->advance();

  std::vector<StructFieldInfo> fields;
  std::vector<ASTNode> methods;
  while (!is(TokenKind::RBRACK) && !is(TokenKind::_EOF)) {
    skipTopLevelTrivia();
    if (is(TokenKind::RBRACK)) {
      break;
    }

    if (is(TokenKind::CONSTRUCTOR) || is(TokenKind::DESTRUCTOR) ||
        is(TokenKind::ALLOCATOR)) {
      ParserError("struct lifetime hooks are not implemented yet",
                  currentTokenInfo());
    }

    bool outOfSize = false;
    auto nextToken = nextTokenInfo(outOfSize).getTokenKind();
    if (!outOfSize && is(TokenKind::IDENT) &&
        (nextToken == TokenKind::LPAREN ||
         nextToken == TokenKind::COLONCOLON ||
         nextToken == TokenKind::LBRACK)) {
      DeclarationModifiers methodModifiers;
      methodModifiers.access = declarationModifiers.access;
      const auto astSizeBeforeMethod = m_AST.size();
      funcDecl(methodModifiers, false, structName);
      while (m_AST.size() > astSizeBeforeMethod) {
        methods.push_back(std::move(m_AST.back()));
        m_AST.pop_back();
      }
      continue;
    }

    fields.push_back(parseStructField());
  }

  expected(TokenKind::RBRACK);
  posInfo = currentTokenInfo().getTokenPositionInfo();
  SemanticLoc semLoc(begin, posInfo.end, line);
  this->advance();

  auto structAst =
      std::make_unique<StructAST>(structName, std::move(fields),
                                  declarationModifiers.access, semLoc);
  this->m_AST.emplace_back(std::move(structAst));
  for (auto &method : methods) {
    this->m_AST.emplace_back(std::move(method));
  }

  skipTopLevelTrivia();
  if (is(TokenKind::SEMICOLON)) {
    this->advance();
  }
}

void CStarParser::parseIncludeDirective() {
  auto includeToken = currentTokenInfo();
  this->advance();
  skipTopLevelTrivia();

  if (is(TokenKind::INVOLVED)) {
    this->advance();
    skipTopLevelTrivia();
    expected(TokenKind::LBRACK);
    this->advance();
    while (!is(TokenKind::RBRACK)) {
      skipTopLevelTrivia();
      if (is(TokenKind::LITERAL) || is(TokenKind::LETTER) ||
          is(TokenKind::IDENT)) {
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
        if (includeName.size() >= 6 &&
            includeName.substr(includeName.size() - 6) == ".cstar") {
          m_SourceIncludes.emplace_back(includeName);
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
    if (includeName.size() >= 6 &&
        includeName.substr(includeName.size() - 6) == ".cstar") {
      m_SourceIncludes.emplace_back(includeName);
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
      if (is(TokenKind::STRUCT)) {
        parseStructDecl(declarationModifiers);
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
      if (this->isType(this->m_CurrToken) || is(TokenKind::IDENT)) {
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

bool CStarParser::isType(const TokenInfo& token) {
  switch (token.getTokenKind()) {
    case VOID:
    case I8:
    case I16:
    case I32:
    case I64:
    case INT:
    case U8:
    case U16:
    case U32:
    case U64:
    case U128:
    case UINT:
    case ISIZE:
    case USIZE:
    case F32:
    case F64:
    case FLOAT:
    case UCHAR:
    case CHAR:
    case BOOL:
    case VEC2:
    case VEC3:
      // Every single IDENT is going to be interpreted as Symbol
      // case IDENT:
    case VEC4:
      return true;
    default:
      return false;
  }
}

bool CStarParser::isCastableOperator(const TokenInfo& token) {
  switch (token.getTokenKind()) {
    case UNSAFE_CAST:
    case CAST:
    case AS:
      return true;
    default:
      return false;
  }
}

bool CStarParser::isOperator(const TokenInfo& token) {
  switch (token.getTokenKind()) {
    case LPAREN:
    case QMARK:
      // those are because detecting is binary op
    case COLON:
      // suspicious
   // case LSQPAR:
    case RSQPAR:
      //
    case COLONCOLON:
    case UNSAFE_CAST:
    case CAST:
    case PLUSPLUS:
    case MINUSMINUS:
    case DOT:
    case ARROW:
    case SIZEOF:
    case TYPEOF:
    case MOVE:
    case DEREF:
    case REF:
    case AS:
    case PLUS:
    case MINUS:
    case NOT:
    case TILDE:
    case STAR:
    case MOD:
    case DIV:
    case LSHIFT:
    case RSHIFT:
    case LT:
    case LTEQ:
    case GT:
    case GTEQ:
    case EQUALEQUAL:
    case NOTEQUAL:
    case AND:
    case XOR:
    case OR:
    case LAND:
    case LOR:
    case EQUAL:
    case RET:
    case PLUSEQ:
    case MINUSEQ:
    case STAREQ:
    case DIVEQ:
    case MODEQ:
    case LSHIFTEQ:
    case RSHIFTEQ:
    case ANDEQ:
    case XOREQ:
    case OREQ:
    case COMMA:
      return true;
    default:
      return false;
  }
}

bool CStarParser::isLinkageMark(const TokenInfo& token) {
  switch (token.getTokenKind()) {
    case EXPORT:  // extern
    case IMPORT:  // extern
      return true;
    default:
      return false;
  }
}

bool CStarParser::isDeclarationModifier(const TokenInfo& token) {
  switch (token.getTokenKind()) {
    case PUBLIC:
    case PRIVATE:
    case STATIC:
    case EXPORT:
    case IMPORT:
      return true;
    default:
      return false;
  }
}

DeclarationModifiers CStarParser::parseDeclarationModifiers(bool localScope) {
  DeclarationModifiers modifiers;

  while (isDeclarationModifier(currentTokenInfo())) {
    switch (currentTokenKind()) {
      case PUBLIC:
        if (localScope) {
          ParserError("'public' is only valid on module declarations",
                      currentTokenInfo());
        }
        modifiers.access = ACCESS_PUBLIC;
        break;
      case PRIVATE:
        if (localScope) {
          ParserError("'private' is only valid on module declarations",
                      currentTokenInfo());
        }
        modifiers.access = ACCESS_PRIVATE;
        break;
      case STATIC:
        if (localScope) {
          ParserError("'static' local declarations are not implemented yet",
                      currentTokenInfo());
        }
        if (modifiers.linkage == VisibilitySpecifier::VIS_EXPORT ||
            modifiers.linkage == VisibilitySpecifier::VIS_IMPORT) {
          ParserError("'static' cannot be combined with import/export linkage",
                      currentTokenInfo());
        }
        modifiers.isStatic = true;
        if (modifiers.linkage == VisibilitySpecifier::VIS_DEFAULT) {
          modifiers.linkage = VisibilitySpecifier::VIS_STATIC;
        }
        break;
      case EXPORT:
        if (localScope) {
          ParserError("'export' is only valid on module declarations",
                      currentTokenInfo());
        }
        if (modifiers.isStatic) {
          ParserError("'export' cannot be combined with static linkage",
                      currentTokenInfo());
        }
        modifiers.linkage = VisibilitySpecifier::VIS_EXPORT;
        break;
      case IMPORT:
        if (localScope) {
          ParserError("'import' is only valid on module declarations",
                      currentTokenInfo());
        }
        if (modifiers.isStatic) {
          ParserError("'import' cannot be combined with static linkage",
                      currentTokenInfo());
        }
        modifiers.linkage = VisibilitySpecifier::VIS_IMPORT;
        break;
      default:
        assert(false && "Unreachable declaration modifier");
    }
    this->advance();
    skipTopLevelTrivia();
  }

  return modifiers;
}

bool CStarParser::isPackageMark(const TokenInfo& token) {
  switch (token.getTokenKind()) {
    case INCLUDE:
      return true;
    default:
      return false;
  }
}

bool CStarParser::isTypeQualifier(const TokenInfo& token) {
  switch (token.getTokenKind()) {
    case CONST:
    case CONSTREF:
    case CONSTPTR:
    case READONLY:
      return true;
    default:
      return false;
  }
}

bool CStarParser::isShortcutOp(const TokenInfo& token) {
  switch (token.getTokenKind()) {
    case PLUSEQ:
    case MINUSEQ:
    case STAREQ:
    case DIVEQ:
    case MODEQ:
    case RSHIFTEQ:
    case LSHIFTEQ:
    case ANDEQ:
    case OREQ:
    case XOREQ:
    case EQUAL:
    case TYPEINF:
      return true;
    default:
      return false;
  }
}

ShortcutOp CStarParser::typeOfShortcutOp(const TokenInfo& token) {
  switch (token.getTokenKind()) {
    case PLUSEQ:
      return S_PLUS;
    case MINUSEQ:
      return S_MIN;
    case STAREQ:
      return S_STA;
    case DIVEQ:
      return S_DIV;
    case MODEQ:
      return S_MOD;
    case RSHIFTEQ:
      return S_SHR;
    case LSHIFTEQ:
      return S_SHL;
    case ANDEQ:
      return S_AND;
    case OREQ:
      return S_OR;
    case XOREQ:
      return S_XOR;
    case TYPEINF:
      return S_MOV;
    case EQUAL:
      return S_NONE;
    default:
      assert(false && "Unreacheable!");
      return S_NONE;
  }
}

Type CStarParser::typeOf(const TokenInfo& token) {
  switch (token.getTokenKind()) {
    case VOID:
      return Type::T_VOID;
    case I8:
      return Type::T_I8;
    case I16:
      return Type::T_I16;
    case I32:
      return Type::T_I32;
    case I64:
      return Type::T_I64;
    case INT:
      return Type::T_INT;
    case U8:
      return Type::T_U8;
    case U16:
      return Type::T_U16;
    case U32:
      return Type::T_U32;
    case U64:
      return Type::T_U64;
    case U128:
      return Type::T_U128;
    case UINT:
      return Type::T_UINT;
    case ISIZE:
      return Type::T_ISIZE;
    case USIZE:
      return Type::T_USIZE;
    case F32:
      return Type::T_F32;
    case F64:
      return Type::T_F64;
    case FLOAT:
      return Type::T_FLOAT;
    case UCHAR:
      return Type::T_UCHAR;
    case CHAR:
      return Type::T_CHAR;
    case BOOL:
      return Type::T_BOOL;
    case VEC2:
      return Type::T_VEC2;
    case VEC3:
      return Type::T_VEC3;
    case VEC4:
      return Type::T_VEC4;
      // every single IDENT is going to be interpreted as Symbol...
      // case IDENT:
      //   return Type::T_DEFINED;
    default:
      assert(false && "Unreacheable");
      return Type::T_VOID;
  }
}

TypeQualifier CStarParser::typeQualifierOf(const TokenInfo& tokenInfo) {
  switch (tokenInfo.getTokenKind()) {
    case CONST:
      return TypeQualifier::Q_CONST;
    case CONSTREF:
      return TypeQualifier::Q_CONSTREF;
    case CONSTPTR:
      return TypeQualifier::Q_CONSTPTR;
    case READONLY:
      return TypeQualifier::Q_READONLY;
    default:
      assert(false && "Unreacheable!");
      return TypeQualifier::Q_NONE;
  }
}

void CStarParser::emitDiagnostic(cstar::diagnostics::Severity severity,
                                 cstar::diagnostics::DiagnosticCode code,
                                 std::string mesg, size_t begin, size_t end,
                                 size_t line, bool exitAfter) {
  cstar::diagnostics::DiagnosticRenderer renderer(std::cout);
  renderer.print({
      code,
      severity,
      std::move(mesg),
      this->m_Lexer.getFilepath().get(),
      this->m_Lexer.getBufferView(),
      {begin, end, line},
  });

  if (exitAfter) {
    exit(1);
  }
}

void CStarParser::ParserHint(std::string mesg, TokenInfo tokenInfo) {
  auto posInfo = tokenInfo.getTokenPositionInfo();
  emitDiagnostic(cstar::diagnostics::Severity::Hint,
                 cstar::diagnostics::DiagnosticCode::ParserHint,
                 std::move(mesg), posInfo.begin, posInfo.end, posInfo.line,
                 false);
}

void CStarParser::ParserWarning(std::string mesg, size_t newBegin,
                                size_t newEnd, size_t newLine) {
  emitDiagnostic(cstar::diagnostics::Severity::Warning,
                 cstar::diagnostics::DiagnosticCode::SemanticWarning,
                 std::move(mesg), newBegin, newEnd, newLine, false);
}

void CStarParser::ParserHint(std::string mesg, TokenInfo tokenInfo,
                             size_t new_begin) {
  auto posInfo = tokenInfo.getTokenPositionInfo();
  emitDiagnostic(cstar::diagnostics::Severity::Hint,
                 cstar::diagnostics::DiagnosticCode::ParserHint,
                 std::move(mesg), new_begin, posInfo.end, posInfo.line, false);
}

void CStarParser::ParserError(const std::string& mesg, TokenInfo tokenInfo) {
  auto posInfo = tokenInfo.getTokenPositionInfo();
  emitDiagnostic(cstar::diagnostics::Severity::Error,
                 cstar::diagnostics::DiagnosticCode::ParserSyntax, mesg,
                 posInfo.begin, posInfo.end, posInfo.line, true);
}

void CStarParser::ParserError(std::string mesg, size_t begin, size_t end,
                              size_t line_) {
  ParserError(std::move(mesg), begin, end, line_,
              cstar::diagnostics::DiagnosticCode::SemanticError);
}

void CStarParser::ParserError(std::string mesg, size_t begin, size_t end,
                              size_t line_,
                              cstar::diagnostics::DiagnosticCode code) {
  emitDiagnostic(cstar::diagnostics::Severity::Error,
                 code, std::move(mesg), begin, end, line_, false);
}

void CStarParser::ParserError(std::string mesg, TokenInfo tokenInfo,
                              size_t new_begin) {
  auto posInfo = tokenInfo.getTokenPositionInfo();
  emitDiagnostic(cstar::diagnostics::Severity::Error,
                 cstar::diagnostics::DiagnosticCode::ParserSyntax,
                 std::move(mesg), new_begin, posInfo.end, posInfo.line, true);
}

std::string_view::iterator CStarParser::viewLine(size_t line, size_t& rlbegin,
                                                 size_t& rlend,
                                                 size_t& offset) {
  auto buffer_view = this->m_Lexer.getBufferView();
  size_t lbegin = 0;
  // first line
  if (line == 0) {
    bool oneLineFlag = true;
    for (size_t i = 0; i < buffer_view.size(); i++) {
      if (buffer_view[i] == '\n') {
        oneLineFlag = false;

        lbegin = 0;
        offset = i;
        break;
      }
    }

    // that means we have only one line in the source file.
    // and it's not ending up with '\n'
    if (oneLineFlag) {
      lbegin = 0;
      offset = buffer_view.size();
    }
  } else {
    auto line_walker = 0;
    bool begin_mark = false;

    for (size_t i = 0; i < buffer_view.size(); ++i) {
      if (buffer_view[i] == '\n') {
        line_walker += 1;
      }

      if (line_walker == line && !begin_mark) {
        lbegin = i + 1;
        begin_mark = true;
      } else if (line_walker - 1 == line) {
        rlbegin -= lbegin;
        rlend -= lbegin;
        offset = i - lbegin;
        break;
      } else {
        continue;
      }
    }
  }

  return buffer_view.begin() + lbegin;
}
