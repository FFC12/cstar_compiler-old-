#ifndef LEXER_TOKEN_HPP
#define LEXER_TOKEN_HPP

#include <cstddef>
#include <memory>
#include <string>
#include <utility>

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
  AT,
  HASH,
  COLONCOLON,
  COLON,
  PAREQ,  // partial eq ===
  CONST,
  CONSTPTR,
  CONSTREF,
  READONLY,
  NOMOVE,
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
  PUBLIC,
  PRIVATE,
  STATIC,
  CAST,
  UNSAFE_CAST,
  SIZEOF,
  TYPEOF,
  MOVE,
  DROP,
  NEW,
  SHARED,
  OPERATOR,
  DYN,
  POLICY_ASSIGN,
  DYNAMIC,
  PROTOCOL,
  STATE,
  WITH,
  STRUCT,
  TRAIT,
  MACRO,
  CONSTRUCTOR,
  DESTRUCTOR,
  ALLOCATOR,
  EXCEPT,
  THROW,
  DEFER,
  ASYNC,
  AWAIT,
  SELF,
  IS,
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
  FLAGS,
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

#endif