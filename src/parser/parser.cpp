#include <parser/parser.hpp>

void CStarParser::parse() {
  m_TokenStream = this->m_Lexer.perform();
  this->m_Lexer.lexerStats();
  this->m_StartTime = time(nullptr);

  if (!this->m_TokenStream.empty()) {
    // auto eof = std::move(m_TokenStream[-1]);
    this->m_CurrToken = m_TokenStream[m_TokenIndex];  //[0]
    this->translationUnit();
  } else {
    assert(false && "Parser token stream is not enough to parse!\n");
  }

  this->parserStats();
}

void CStarParser::translationUnit() {
  while (!this->m_ParsingEndingFlag) {
    // std::cout << this->m_CurrToken.getTokenAsStr() << "\n";
    // TODO(1): EOF cannot be processed
    if (this->m_CurrToken.getTokenKind() == TokenKind::_EOF) {
      std::cout << "EOF\n";
      break;
    }

    // for (int n = 0; n < 3; n++) {
    //   size_t offset;
    //   auto it = this->viewLine(n, offset);
    //   for (int i = 0; i < offset; i++) {
    //     std::cout << it[i];
    //   }
    //   std::cout << std::endl;
    // }

    if (is(TokenKind::UNHANDLED)) {
      auto t = this->currentTokenInfo();
    }

    if (is(TokenKind::COMMENT) || is(TokenKind::LINEFEED)) {
      this->advance();
      continue;
    }

    if (isPackageMark(this->m_CurrToken)) {
      // package | package involved
    } else {
      // export | import
      if (isLinkageMark(this->m_CurrToken)) {
        this->advance();
        if (this->isType(this->m_CurrToken)) {
        }
      } else {
        // int* | float* | uint* ...
        // we check that is an IDENT or not since because isType for operators
        // only contains primitives. IDENT means it's a symbol (which needed to
        // resolved in next phase - Semantic Analysis- )
        if (this->isType(this->m_CurrToken) || is(TokenKind::IDENT)) {
          varDecl(is(TokenKind::IDENT));
        } else {
          ParserHint("Not implemented yet", currentTokenInfo());
          ParserError("Unexpected token '" +
                          std::string(tokenToStr(currentTokenKind())) + "'",
                      currentTokenInfo());
        }
      }
    }
  }
}

bool CStarParser::isType(const TokenInfo& token) {
  switch (token.getTokenKind()) {
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
    case EXPORT:
    case IMPORT:
      return true;
    default:
      return false;
  }
}

bool CStarParser::isPackageMark(const TokenInfo& token) {
  switch (token.getTokenKind()) {
    case PACKAGE:
      return true;
    default:
      return false;
  }
}

Type CStarParser::typeOf(const TokenInfo& token) {
  switch (token.getTokenKind()) {
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
  }
}

void CStarParser::ParserHint(std::string mesg, TokenInfo tokenInfo) {
  auto posInfo = tokenInfo.getTokenPositionInfo();

  // copy one time for each translation unit
  static std::string messageHeader(this->m_Lexer.getFilepath().get());
  const size_t CHAR_LIMIT = 256;
  const size_t MARGIN_LEFT = 5;

  // token pos
  size_t tok_begin = posInfo.begin;
  size_t tok_end = posInfo.end;
  size_t line = posInfo.line;

  // line
  size_t offset = 0;

  // tok_begin will be relative begin according to the line.
  auto buffer_it = this->viewLine(line, tok_begin, tok_end, offset);

  // message header
  messageHeader += ":" + std::to_string(line + 1) + ":" +
                   std::to_string(tok_begin + 1) + BWHT "\x20 hint: " RESET +
                   mesg + "\n";

  std::cout << BLU + messageHeader + RESET;

  // line beginning
  std::cout << std::endl << "\x20" << line + 1 << "\x20|\x20";

  // linw
  for (int i = 0; i < offset; i++) {
    if (i < CHAR_LIMIT) {
      std::cout << buffer_it[i];
    } else {
      std::cout << "...";
      break;
    }
  }

  std::cout << std::endl;

  auto lineNumberLen = std::to_string(line).size();

  // for margin
  for (int i = 0; i < MARGIN_LEFT + lineNumberLen - 1; i++)
    if (i == 3 + lineNumberLen - 1)
      putchar('|');
    else
      putchar('\x20');

  // indicator
  std::cout << BLU;
  for (int i = 0; i < offset; i++) {
    if (i >= tok_begin && i < tok_end) {
      putchar('~');
    } else {
      putchar('\x20');
    }
  }
  std::cout << RESET;
  std::cout << std::endl << std::endl;
}

void CStarParser::ParserHint(std::string mesg, TokenInfo tokenInfo,
                             size_t new_begin) {
  auto posInfo = tokenInfo.getTokenPositionInfo();

  // copy one time for each translation unit
  static std::string messageHeader(this->m_Lexer.getFilepath().get());
  const size_t CHAR_LIMIT = 256;
  const size_t MARGIN_LEFT = 5;

  // token pos
  size_t tok_begin = new_begin;
  size_t tok_end = posInfo.end;
  size_t line = posInfo.line;

  // line
  size_t offset = 0;

  // tok_begin will be relative begin according to the line.
  auto buffer_it = this->viewLine(line, tok_begin, tok_end, offset);

  // message header
  messageHeader += ":" + std::to_string(line + 1) + ":" +
                   std::to_string(tok_begin + 1) + BWHT "\x20 hint: " RESET +
                   mesg + "\n";

  std::cout << BLU + messageHeader + RESET;

  // line beginning
  std::cout << std::endl << "\x20" << line + 1 << "\x20|\x20";

  // linw
  for (int i = 0; i < offset; i++) {
    if (i < CHAR_LIMIT) {
      std::cout << buffer_it[i];
    } else {
      std::cout << "...";
      break;
    }
  }

  std::cout << std::endl;

  auto lineNumberLen = std::to_string(line).size();

  // for margin
  for (int i = 0; i < MARGIN_LEFT + lineNumberLen - 1; i++)
    if (i == 3 + lineNumberLen - 1)
      putchar('|');
    else
      putchar('\x20');

  // indicator
  std::cout << BLU;
  for (int i = 0; i < offset; i++) {
    if (i >= tok_begin && i < tok_end) {
      putchar('~');
    } else {
      putchar('\x20');
    }
  }
  std::cout << RESET;
  std::cout << std::endl << std::endl;
}

// Parsing errors doesn't let you accumulate error and warning messages.
// This means when a parser error occured it will be exited (1) unlike
// semantic analyzer does
void CStarParser::ParserError(std::string mesg, TokenInfo tokenInfo) {
  auto posInfo = tokenInfo.getTokenPositionInfo();

  // copy one time for each translation unit
  static std::string messageHeader(this->m_Lexer.getFilepath().get());
  const size_t CHAR_LIMIT = 256;
  const size_t MARGIN_LEFT = 5;

  // token pos
  size_t tok_begin = posInfo.begin;
  size_t tok_end = posInfo.end;
  size_t line = posInfo.line;

  // line
  size_t offset = 0;

  // tok_begin will be relative begin according to the line.
  auto buffer_it = this->viewLine(line, tok_begin, tok_end, offset);

  // message header
  messageHeader += ":" + std::to_string(line + 1) + ":" +
                   std::to_string(tok_begin + 1) + RED "\x20 error: " RESET +
                   mesg + "\n";

  std::cout << BLU + messageHeader + RESET;

  // line beginning
  std::cout << std::endl << "\x20" << line + 1 << "\x20|\x20";

  // linw
  for (int i = 0; i < offset; i++) {
    if (i < CHAR_LIMIT) {
      std::cout << buffer_it[i];
    } else {
      std::cout << "...";
      break;
    }
  }

  std::cout << std::endl;

  auto lineNumberLen = std::to_string(line).size();

  // for margin
  for (int i = 0; i < MARGIN_LEFT + lineNumberLen - 1; i++)
    if (i == 3 + lineNumberLen - 1)
      putchar('|');
    else
      putchar('\x20');

  // indicator
  std::cout << BLU;
  for (int i = 0; i < offset; i++) {
    if (i >= tok_begin && i < tok_end) {
      putchar('~');
    } else {
      putchar('\x20');
    }
  }
  std::cout << RESET;
  std::cout << std::endl << std::endl;

  exit(1);
}

// This is for indicating to the long expressions/buffers.
void CStarParser::ParserError(std::string mesg, TokenInfo tokenInfo,
                              size_t new_begin) {
  auto posInfo = tokenInfo.getTokenPositionInfo();

  // copy one time for each translation unit
  static std::string messageHeader(this->m_Lexer.getFilepath().get());
  const size_t CHAR_LIMIT = 256;
  const size_t MARGIN_LEFT = 5;

  // token pos
  size_t tok_begin = new_begin;
  size_t tok_end = posInfo.end;
  size_t line = posInfo.line;

  // line
  size_t offset = 0;

  // tok_begin will be relative begin according to the line.
  auto buffer_it = this->viewLine(line, tok_begin, tok_end, offset);

  // message header
  messageHeader += ":" + std::to_string(line + 1) + ":" +
                   std::to_string(tok_begin + 1) + RED "\x20 error: " RESET +
                   mesg + "\n";

  std::cout << BLU + messageHeader + RESET;

  // line beginning
  std::cout << std::endl << "\x20" << line + 1 << "\x20|\x20";

  // linw
  for (int i = 0; i < offset; i++) {
    if (i < CHAR_LIMIT) {
      std::cout << buffer_it[i];
    } else {
      std::cout << "...";
      break;
    }
  }

  std::cout << std::endl;

  auto lineNumberLen = std::to_string(line).size();

  // for margin
  for (int i = 0; i < MARGIN_LEFT + lineNumberLen - 1; i++)
    if (i == 3 + lineNumberLen - 1)
      putchar('|');
    else
      putchar('\x20');

  // indicator
  std::cout << BLU;
  for (int i = 0; i < offset; i++) {
    if (i >= tok_begin && i < tok_end) {
      putchar('~');
    } else {
      putchar('\x20');
    }
  }
  std::cout << RESET;
  std::cout << std::endl << std::endl;

  exit(1);
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
