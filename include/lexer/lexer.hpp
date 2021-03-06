#ifndef LEXER_HPP
#define LEXER_HPP
#include <base.hpp>
#include <cassert>
#include <ctime>
#include <iostream>
#include <map>
#include <memory>
#include <string_view>
#include <utility>
#include <vector>

// This section is for compiler frontend
enum TokenKind {
  IMPORT,
  NATIVE,
  SCALARI,  // integer scalar like 10, 11
  SCALARD,  // double scalar like 10.03f, 20.0243f
  MATRIX,
  VECTOR,
  IDENT,
  LITERAL,
  DOT,
  DOLLAR,  // $
  RANGE,
  TRIPLET,
  SEMICOLON,
  COMMA,
  UNDERSCORE,  // _
  TYPEINF,
  PLUS,
  PLUSEQ,
  PLUSPLUS,
  MINUS,
  MINUSMINUS,
  MINUSEQ,
  QMARK,   //?
  ARROW,   //->
  DARROW,  // =>
  DIV,
  DIVEQ,
  STAR,    // *
  STAREQ,  // *=
  MOD,
  MODEQ,
  LSHIFT,
  LSHIFTEQ,
  RSHIFT,
  RSHIFTEQ,
  EQUAL,
  EQUALEQUAL,
  NOT,
  NOTEQUAL,
  TILDE,
  TILDEEQ,
  LAND,
  AND,
  ANDEQ,
  LOR,
  OR,
  OREQ,
  XOR,
  XOREQ,
  LTEQ,
  LT,
  GTEQ,
  GT,
  COMMENT,
  LETTER,
  LBRACK,
  RBRACK,
  LPAREN,
  RPAREN,
  LSQPAR,
  RSQPAR,
  HASH,
  COLONCOLON,
  COLON,
  PAREQ,  // partial eq ===
  CONST,
  CONSTPTR,
  CONSTREF,
  READONLY,
  RET,
  IN,
  AS,
  IF,
  ELIF,
  ELSE,
  REF,
  DEREF,
  INCLUDE,
  INVOLVED,
  OPTION,
  LOOP,
  DEFAULT,
  EXTERN,
  FROM,
  EXPORT,
  STATIC,
  CAST,
  UNSAFE_CAST,
  SIZEOF,
  TYPEOF,
  MOVE,
  MOVEQ,
  I8,
  I16,
  I32,
  I64,
  //  I128,
  INT,
  U8,
  U16,
  U32,
  U64,
  U128,
  UINT,
  ISIZE,
  USIZE,
  F32,
  F64,
  FLOAT,
  UCHAR,
  CHAR,
  BOOL,
  VEC2,
  VEC3,
  VEC4,
  VEC,
  VOID,
  ANY,
  ATTRIB,
  PROTO,
  ENUM,
  FOR,
  BREAK,
  CONTINUE,
  SQUOTE,
  DQUOTE,
  NIL,
  TRUE,
  FALSE,
  UNKNOWN,
  UNHANDLED,
  LINEFEED,
  WHITESAPACE,
  _EOF,
};

struct TypeInfo {
  TokenKind kind;
  // For the matrix and vector
  // mat2x2 => row = 2, column = 2
  // vec2 => row = 1, column = 2
  int row;
  int column;
};

struct PositionInfo {
  size_t begin;
  size_t end;
  size_t line;

  PositionInfo() = default;

  // It's not good idea that copying each token.
  // Because there may be a million token.That's why we have to moved.
  PositionInfo(size_t begin, size_t end, size_t line)
      : begin(begin), end(end), line(line) {}

  void setBegin(size_t b) { this->begin = b; }
};

struct FileInfo {
  std::shared_ptr<char> filename;
  std::shared_ptr<char> filepath;

  FileInfo() = default;
  FileInfo(std::shared_ptr<char> filename, std::shared_ptr<char> filepath)
      : filename(std::move(filename)), filepath(std::move(filepath)) {}
};

enum LexerFlags { IDLE, JUST_STARTED, RUNNING, DONE };

struct TokenInfo {
 private:
  bool m_HasKeyword;
  std::string m_TokenStr;
  TokenKind m_TokenKind;
  PositionInfo m_PositionInfo;

 public:
  TokenInfo() = default;
  TokenInfo(const TokenKind &pKind, const PositionInfo &pPositionInfo,
            const std::string &pTokenStr = "", bool pHasKeyword = false)
      : m_PositionInfo(pPositionInfo) {
    this->m_HasKeyword = pHasKeyword;
    this->m_TokenKind = pKind;
    this->m_TokenStr = pTokenStr;
  }

  bool operator==(TokenKind token) const {
    return this->getTokenKind() == token;
  }

  [[nodiscard]] std::string getTokenAsStr() const { return this->m_TokenStr; }

  [[nodiscard]] TokenKind getTokenKind() const { return this->m_TokenKind; }

  [[nodiscard]] bool getHasKeyword() const { return this->m_HasKeyword; }

  [[nodiscard]] PositionInfo getTokenPositionInfo() {
    return this->m_PositionInfo;
  }
};

class CStarLexer {
  time_t m_StartTime;
  LexerFlags m_LexerFlags;
  std::string m_Buffer;
  std::string_view m_BufferView;
  size_t m_Index;
  size_t m_Line, m_Col;
  char m_CurrChar;
  size_t m_LastBegin;
  bool m_IsKeyword;
  std::string m_LastKeyword;
  FileInfo m_FileInfo;
  std::map<size_t, size_t> m_CharCountOfLine;

  void preprocess() {
    size_t charCounter = 0;
    for (int i = 0, j = 0; i < m_Buffer.size(); i++) {
      if (m_Buffer[i] == '\t') {
        m_Buffer[i] = ' ';
        m_Buffer.insert(i, " ");
      }
    }
  }

  void restoreLastChar() { this->m_Index--; }

  char nextCharLinefeed() {
    if (m_Index < m_BufferView.size() + 1) {
      if (m_Index == 0) m_Index = 1;
      auto c = m_BufferView[m_Index];
      m_CurrChar = c;

      m_Index++;
      return c;
    } else if (m_Index == m_BufferView.size() + 1) {
      this->m_LexerFlags = LexerFlags::DONE;

      // Well, this may be moved to the function 'perform'. But for now, I don't
      // care.
      //      if(this->m_LexerFlags == LexerFlags::DONE){
      //	    std::exit(0);
      //      }
      return ' ';  // for making the compiler silent
    } else {
      std::cerr << "Out of index in the buffer...Probably corrupted data\n\n";
      std::abort();
      return ' ';  // for making the compiler silent
    }
  }

  char nextChar() {
    if (m_Index < m_BufferView.size() + 1) {
      auto c = m_BufferView[m_Index];
      m_CurrChar = c;

      /*if (c == '\n' || c == '\r') {
        m_Line += 1;
      }*/

      m_Index++;
      return c;
    } else if (m_Index == m_BufferView.size() + 1) {
      this->m_LexerFlags = LexerFlags::DONE;
      return ' ';  // for making the compiler silent
    } else {
      std::cerr << "Out of index in the buffer...Probably corrupted data\n\n";
      std::abort();
      return ' ';  // for making the compiler silent
    }
  }

 public:
  // Potentially big buffers will be passed here so let it moved by move
  // semantics...
  CStarLexer(const std::string &&pBuffer, const std::shared_ptr<char> &realpath)
      : m_IsKeyword(false),
        m_Index(0),
        m_LastBegin(0),
        m_LexerFlags(LexerFlags::IDLE),
        m_FileInfo(nullptr, realpath) {
    m_StartTime = time(nullptr);
    m_Buffer = pBuffer;


    // Add new line at the end of file
    // since our ParserError function can
    // be worked correctly
    if (!m_Buffer.empty()) {
      if (m_Buffer[m_Buffer.size()] != '\n') m_Buffer.push_back('\n');
    }

    preprocess();
    m_BufferView = m_Buffer;
    m_Line = m_Col = 0;
    m_CurrChar = m_BufferView[0];
    /*preprocess();
    for(auto &el: this->m_CharCountOfLine) {
      std::cout << el.first << " - " << el.second << std::endl;
      }*/
  }

  [[nodiscard]] std::shared_ptr<char> getFilepath() const {
    return this->m_FileInfo.filepath;
  }

  [[nodiscard]] std::string_view getBufferView() const {
    return this->m_BufferView;  // std::string(this->m_BufferView.begin(),
                                // this->m_BufferView.end());
  }

  bool lookAhead(char expected) {
    bool isExpectedChar = false;

    // if (m_BufferView[m_Index + 1] == expected)
    if (m_BufferView[m_Index] == expected) {
      isExpectedChar = true;
      nextChar();
    } else {
      isExpectedChar = false;
    }

    return isExpectedChar;
  }

  TokenKind advanceConstant() {
    char _c = this->m_CurrChar;
    bool _is_float = false;
    std::string _keyword;
    TokenKind _scalarType = TokenKind::SCALARI;

    while (isdigit(_c) || _c == '.') {
      _keyword += _c;
      _c = nextChar();

      if (_c == '.') {
        // if there is not
        if (lookAhead('0') || lookAhead('1') || lookAhead('2') ||
            lookAhead('3') || lookAhead('4') || lookAhead('5') ||
            lookAhead('6') || lookAhead('7') || lookAhead('8') ||
            lookAhead('9')) {
        } else {
          m_Buffer.insert(m_Index, "0");
        }

        if (_is_float) {
          _is_float = false;
          break;
        }

        _is_float = true;
        _scalarType = TokenKind::SCALARD;
      }
    }
    restoreLastChar();
    this->m_LastKeyword = _keyword;
    this->m_IsKeyword = true;
    return _scalarType;
  }

  TokenKind advanceLiteral() {
    char _c = nextChar();  // skip '"'
    std::string _keyword = "";
    while (_c != '"') {
      if (m_Index > m_BufferView.size()) {
        std::cerr << "Missing terminating \" character\n";
        std::abort();
      }

      _keyword += _c;
      _c = nextChar();
    }

    this->m_LastKeyword = _keyword;
    this->m_IsKeyword = true;
    return LITERAL;
  }

  TokenKind advanceComments() {
    if (m_CurrChar == '/') {
      char _c = nextChar();  // skip '/'

      while (_c != '\n' && _c != '\0') {
        if (m_Index > m_BufferView.size()) {
          break;
        } else {
          _c = nextChar();
        }
      }

      // if (_c != '\0')
      //   nextChar();

      restoreLastChar();

      return COMMENT;
    } else if (m_CurrChar == '*') {
      char _c = nextChar();  // skip '*'
      if (_c == '\n' || _c == '\r') m_Line += 1;

      while (true) {
        if (m_Index > m_BufferView.size()) {
          std::cerr << "Missing terminating '*/' multi-line comment\n";
          std::abort();
        }

        _c = nextChar();
        if (_c == '*') {
          if (lookAhead('/')) break;
        } else if (_c == '\n' || _c == '\r')
          m_Line += 1;
      }
      _c = nextChar();  // skip '*'
      if (_c == '\n' || _c == '\r') m_Line += 1;

      // m_Line -= 1;
      return COMMENT;
    } else {
      assert("This is not possible");
      return UNKNOWN;  // for making the compiler silent
    }
  }

  TokenKind advanceLetter() {
    char _c = nextChar();  // skip '"'
    std::string _keyword = "";
    while (_c != '\'') {
      if (m_Index > m_BufferView.size()) {
        std::cerr << "Missing terminating \' character\n";
        std::abort();
      }
      _keyword += _c;
      _c = nextChar();
    }

    this->m_LastKeyword = _keyword;
    this->m_IsKeyword = true;
    return LETTER;
  }

  TokenKind advanceIdentifier() {
    char _c = this->m_CurrChar;
    std::string _keyword = "";
    while (isalnum(_c) || _c == '_') {
      _keyword += _c;
      _c = nextChar();
    }
    restoreLastChar();  // restore last character..

    this->m_LastKeyword = _keyword;
    this->m_IsKeyword = true;
    return IDENT;
  }

  bool isSymbol() { return ispunct(m_CurrChar) != 0; }

  // TODO: You can just hashmapped rather than let it make a crazy vast
  // branches...
  TokenKind classifyIdents(const std::string &ident) {
    if (ident == "ret")
      return RET;
    else if (ident == "in")
      return IN;
    else if (ident == "as")
      return AS;
    else if (ident == "if")
      return IF;
    else if (ident == "elif")
      return ELIF;
    else if (ident == "else")
      return ELSE;
    else if (ident == "ref")
      return REF;
    else if (ident == "deref")
      return DEREF;
    else if (ident == "include")
      return INCLUDE;
    else if (ident == "involved")
      return INVOLVED;
    else if (ident == "option")
      return OPTION;
    else if (ident == "loop")
      return LOOP;
    else if (ident == "default")
      return DEFAULT;
    else if (ident == "from")
      return FROM;
    else if (ident == "import")
      return IMPORT;
    else if (ident == "export")
      return EXPORT;
    else if (ident == "static")
      return STATIC;
    else if (ident == "cast")
      return CAST;
    else if (ident == "unsafe_cast")
      return UNSAFE_CAST;
    else if (ident == "sizeof")
      return SIZEOF;
    else if (ident == "typeof")
      return TYPEOF;
    else if (ident == "move")
      return MOVE;
    else if (ident == "const")
      return CONST;
    else if (ident == "constptr")
      return CONSTPTR;
    else if (ident == "constref")
      return CONSTREF;
    else if (ident == "readonly")
      return READONLY;
    else if (ident == "int8")
      return I8;
    else if (ident == "int16")
      return I16;
    else if (ident == "int32")
      return I32;
    else if (ident == "int64")
      return I64;
    else if (ident == "int")
      return INT;
    else if (ident == "uint8")
      return U8;
    else if (ident == "uint16")
      return U16;
    else if (ident == "uint32")
      return U32;
    else if (ident == "uint64")
      return U64;
    else if (ident == "uint128")
      return U128;
    else if (ident == "uint")
      return UINT;
    else if (ident == "isize")
      return ISIZE;
    else if (ident == "usize")
      return USIZE;
    else if (ident == "float")
      return FLOAT;
    else if (ident == "float32")
      return F32;
    else if (ident == "float64")
      return F64;
    else if (ident == "uchar")
      return UCHAR;
    else if (ident == "char")
      return CHAR;
    else if (ident == "bool")
      return BOOL;
    else if (ident == "vec2")
      return VEC2;
    else if (ident == "vec3")
      return VEC3;
    else if (ident == "vec4")
      return VEC4;
    else if (ident == "void")
      return VOID;
    else if (ident == "any")
      return ANY;
    else if (ident == "attribute")
      return ATTRIB;
    else if (ident == "prototype")
      return PROTO;
    else if (ident == "enum")
      return ENUM;
    else if (ident == "break")
      return BREAK;
    else if (ident == "continue")
      return CONTINUE;
    else if (ident == "nil")
      return NIL;
    else if (ident == "true")
      return TRUE;
    else if (ident == "false")
      return FALSE;
    else {
      return IDENT;
    }
  }

  // returns token stream as token info
  std::vector<TokenInfo> perform() {
    if (this->m_LexerFlags == LexerFlags::IDLE) {
      this->m_LexerFlags = LexerFlags::RUNNING;
    }
    std::vector<TokenInfo> tokenInfoList;

    while (this->m_LexerFlags == LexerFlags::RUNNING) {
      PositionInfo posInfo{};

      auto token = nextToken();
      posInfo.begin = this->m_LastBegin;
      posInfo.end = this->m_Index;
      posInfo.line = this->m_Line;

      if (token == LINEFEED) {
        this->m_Line += 1;
      }

      // skip COMMENT
      if (token == COMMENT) {
        continue;
      }

      // TODO: This will be changed after all ops implemented to &&
      if (token != TokenKind::UNKNOWN && token != TokenKind::UNHANDLED) {
        bool hasKeyword = false;
        std::string tokenStr;

        if (this->m_IsKeyword) {
          hasKeyword = true;
          tokenStr = this->m_LastKeyword;
          if (token == TokenKind::IDENT) {
            token = classifyIdents(tokenStr);
          }
        } else {
          tokenStr = tokenAsStr(token);
        }

        tokenInfoList.emplace_back(token, posInfo, tokenStr, hasKeyword);
      }
    }
    return tokenInfoList;
  }

  TokenKind nextToken() {
    // reset the last keyword flags
    this->m_IsKeyword = false;
    this->m_LastBegin = this->m_Index;

    char _c = nextChar();

  jump_retokenize:
    switch (_c) {
      // Integer or Float Constants
      case '0':
      case '1':
      case '2':
      case '3':
      case '4':
      case '5':
      case '6':
      case '7':
      case '8':
      case '9':
        return advanceConstant();
        // Identifier
      case 'A':
      case 'B':
      case 'C':
      case 'D':
      case 'E':
      case 'F':
      case 'G':
      case 'H':
      case 'I':
      case 'J':
      case 'K':
      case 'L':
      case 'M':
      case 'N':
      case 'O':
      case 'P':
      case 'Q':
      case 'R':
      case 'S':
      case 'T':
      case 'U':
      case 'V':
      case 'W':
      case 'X':
      case 'Y':
      case 'Z':
      case 'a':
      case 'b':
      case 'c':
      case 'd':
      case 'e':
      case 'f':
      case 'g':
      case 'h':
      case 'i':
      case 'j':
      case 'k':
      case 'l':
      case 'm':
      case 'n':
      case 'o':
      case 'p':
      case 'q':
      case 'r':
      case 's':
      case 't':
      case 'u':
      case 'v':
      case 'w':
      case 'x':
      case 'y':
      case 'z':
      case '_':
        return advanceIdentifier();
      case '{':
        return LBRACK;
      case '}':
        return RBRACK;
      case '(':
        return LPAREN;
      case ')':
        return RPAREN;
      case '[':
        return LSQPAR;
      case ']':
        return RSQPAR;
      case '?':
        return QMARK;
      case '"':
        return advanceLiteral();
      case '\'':
        return advanceLetter();
      case '!':
        if (lookAhead('=')) return NOTEQUAL;
        return NOT;
      case '#':
        return HASH;
      case '.':
        if (lookAhead('.')) {
          if (lookAhead('.')) {
            return TRIPLET;
          }
          return RANGE;
        } else if (lookAhead('=')) {
          return MOVEQ;
        }
        return DOT;
      case ',':
        return COMMA;
      case '|':
        if (lookAhead('|')) {
          return LOR;
        } else if (lookAhead('=')) {
          return OREQ;
        } else {
          return OR;
        }
      case '&':
        if (lookAhead('&')) {
          return LAND;
        } else if (lookAhead('=')) {
          return ANDEQ;
        } else {
          return AND;
        }
      case '*': {
        if (lookAhead('=')) return STAREQ;
        return STAR;
      }
      case '+':
        if (lookAhead('+')) {
          return PLUSPLUS;
        } else if (lookAhead('=')) {
          return PLUSEQ;
        } else {
          return PLUS;
        }
      case '-':
        if (lookAhead('-')) {
          return MINUSMINUS;
        } else if (lookAhead('=')) {
          return MINUSEQ;
        } else if (lookAhead('>')) {
          return ARROW;
        } else {
          return MINUS;
        }
      case '%':
        if (lookAhead('=')) {
          return MODEQ;
        }
        return MOD;
      case '=':
        if (lookAhead('=')) {
          return EQUALEQUAL;
        } else if (lookAhead('>')) {
          return DARROW;
        } else {
          return EQUAL;
        }
      case '~':
        if (lookAhead('=')) {
          return TILDEEQ;
        }
        return TILDE;
      case '/':
        // If it's comment then skip..
        if (lookAhead('/') || lookAhead('*')) {
          return advanceComments();
        } else {
          if (lookAhead('=')) {
            return DIVEQ;
          }
          return DIV;
        }
      case '<':
        if (lookAhead('<')) {
          if (lookAhead('=')) {
            return LSHIFTEQ;
          } else {
            return LSHIFT;
          }
        } else if (lookAhead('=')) {
          return LTEQ;
        } else {
          return LT;
        }
      case '>':
        if (lookAhead('>')) {
          if (lookAhead('=')) {
            return RSHIFTEQ;
          } else {
            return RSHIFT;
          }
        } else if (lookAhead('=')) {
          return GTEQ;
        } else {
          return GT;
        }
      case '^':
        if (lookAhead('=')) {
          return XOREQ;
        }
        return XOR;
      case ';':
        return SEMICOLON;
      case ':':
        if (lookAhead('=')) {
          return TYPEINF;
        } else if (lookAhead(':')) {
          return COLONCOLON;
        }
        return COLON;
      case '$':
        return DOLLAR;
      case '@':
      case '`': {
        return UNHANDLED;
      }
      case '\n':
      case '\r': {
        return LINEFEED;
      }
      case '\0': {
        this->m_LexerFlags = LexerFlags::DONE;
        return _EOF;
      }
      case '\x20': {
        _c = nextChar();
        this->m_LastBegin = this->m_Index - 1;
        goto jump_retokenize;
      }
      default: {
        return UNKNOWN;
      }
    }

    return UNKNOWN;
  }

  const char *tokenAsStr(TokenKind token) {
    switch (token) {
      case SIZEOF:
        return "sizeof";
      case MOVE:
        return "move";
      case CAST:
        return "cast";
      case UNSAFE_CAST:
        return "unsafe_cast";
      case CONST:
        return "const";
      case CONSTPTR:
        return "constptr";
      case CONSTREF:
        return "constref";
      case REF:
        return "ref";
      case DEREF:
        return "deref";
      case SQUOTE:
        return "'";
      case DQUOTE:
        return "\"";
      case IDENT:
        return "IDENTIFIER";
      case SCALARI:
        return "INTEGER SCALAR";
      case SCALARD:
        return "FLOAT SCALAR";
      case LITERAL:
        return "LITERAL";
      case LETTER:
        return "LETTER";
      case LBRACK:
        return "{";
      case RBRACK:
        return "}";
      case LPAREN:
        return "(";
      case RPAREN:
        return ")";
      case LSQPAR:
        return "[";
      case RSQPAR:
        return "]";
      case COLONCOLON:
        return "::";
      case COLON:
        return ":";
      case TYPEINF:
        return ":=";
      case HASH:
        return "#";
      case DOLLAR:
        return "$";
      case DOT:
        return ".";
      case RANGE:
        return "..";
      case TRIPLET:
        return "...";
      case TYPEOF:
        return ":=";
      case SEMICOLON:
        return ";";
      case COMMA:
        return ",";
      case UNDERSCORE:
        return "_";
      case PLUS:
        return "+";
      case PLUSEQ:
        return "+=";
      case PLUSPLUS:
        return "++";
      case MINUS:
        return "-";
      case MINUSMINUS:
        return "--";
      case MINUSEQ:
        return "-=";
      case QMARK:
        return "?";
      case NOT:
        return "!";
      case ARROW:
        return "->";
      case DARROW:
        return "=>";
      case DIV:
        return "/";
      case DIVEQ:
        return "/=";
      case STAR:
        return "*";
      case STAREQ:
        return "*=";
      case MOD:
        return "%";
      case MODEQ:
        return "%=";
      case LSHIFT:
        return "<<";
      case LSHIFTEQ:
        return "<<=";
      case RSHIFT:
        return ">>";
      case RSHIFTEQ:
        return ">>=";
      case EQUAL:
        return "=";
      case EQUALEQUAL:
        return "==";
      case NOTEQUAL:
        return "!=";
      case TILDE:
        return "~";
      case MOVEQ:
        return ".=";
      case LAND:
        return "&&";
      case AND:
        return "&";
      case ANDEQ:
        return "&=";
      case LOR:
        return "||";
      case OR:
        return "|";
      case OREQ:
        return "|=";
      case XOR:
        return "^";
      case XOREQ:
        return "^=";
      case LTEQ:
        return "<=";
      case LT:
        return "<";
      case GTEQ:
        return ">=";
      case GT:
        return ">";
      case LINEFEED:
        return "\n";
      case COMMENT:
        return "COMMENT";
      case UNKNOWN:
        return "UNKNOWN";
      default:
        return "UNHANDLED";
    }
  }

  void lexerStats() const {
    time_t endTime = time(nullptr);
    std::cout << GRN "======= Lexical Analysis =======" RES << std::endl;
    double dif = difftime(endTime, this->m_StartTime);
    printf("-  Elapsed time : %.2lf seconds\n", dif);
    std::cout << "-  Total LoC    : " << this->m_Line + 1 << std::endl
              << std::endl;
  }

  ~CStarLexer() {}
};

#endif  // !LEXER_HPP
