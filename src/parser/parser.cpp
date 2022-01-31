#include <parser/parser.hpp>

void CStarParser::parse() {
  m_TokenStream = this->m_Lexer.perform();
  if (this->m_TokenStream.size() >= 1) {
    this->m_CurrToken = m_TokenStream[m_TokenIndex];  //[0]
    this->translationUnit();
  } else {
    assert(false && "Parser token stream is not enough to parse!\n");
  }
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

    if (is(TokenKind::COMMENT)) {
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
        if (this->isType(this->m_CurrToken)) {
          this->ParserError("hello", this->currentTokenInfo());
          varDecl();
        } else {
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

PositionInfo CStarParser::getPosInfo(TokenInfo tokenInfo) {
  return tokenInfo.getTokenPositionInfo();
}

// Parsing errors doesn't let you accumulate error and warning messages.
// This means when a parser error occured it will be exited (1) unlike
// semantic analyzer what it does
void CStarParser::ParserError(std::string mesg, TokenInfo tokenInfo) {
  auto posInfo = this->getPosInfo(tokenInfo);

  // copy one time for each translation unit
  static std::string messageHeader(this->m_Lexer.getFilepath().get());

  // token pos
  size_t tok_begin = posInfo.begin;
  size_t tok_end = posInfo.end;
  size_t line = posInfo.line;

  // line
  size_t offset = 0;

  auto buffer_it = this->viewLine(line, offset);

  messageHeader += ":" + std::to_string(tok_begin) + ":" +
                   std::to_string(line) + "\x20 error: " + mesg + "\n";
  ErrorMessage(messageHeader.c_str());

  for (int i = 0; i < offset; i++) {
    std::cout << buffer_it[i];
  }

  std::cout << std::endl;

  for (int i = 0; i < offset; i++) {
    if (i >= tok_begin && i < tok_end) {
      putchar('~');
    }
  }

  std::cout << std::endl;

  //	std::string fmt_mesg =
  //	ErrorMessage(fmt_mesg);

  exit(0);
}

std::string_view::iterator CStarParser::viewLine(size_t line, size_t& offset) {
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
        offset = i - lbegin;
        break;
      } else {
        continue;
      }
    }
  }

  return buffer_view.begin() + lbegin;
}
