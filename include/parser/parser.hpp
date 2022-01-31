#ifndef PARSER_HPP
#define PARSER_HPP
#include <ast/ast.hpp>
#include <ast/binary_op_ast.hpp>
#include <ast/cast_op_ast.hpp>
#include <ast/func_call_ast.hpp>
#include <ast/scalar_ast.hpp>
#include <ast/symbol_ast.hpp>
#include <ast/type_ast.hpp>
#include <ast/unary_op_ast.hpp>
#include <ast/var_ast.hpp>
#include <base.hpp>
#include <cassert>
#include <deque>
#include <lexer/lexer.hpp>
#include <memory>
#include <parser/op_prec.hpp>
#include <parser/type_specifiers.hpp>
#include <parser/visibility_specifiers.hpp>

class CStarParser {
  PrecedenceInfoTable m_PrecTableUnary, m_PrecTableBinary, m_PrecTableCast;
  PrecedenceTable m_PrecTable;
  CStarLexer m_Lexer;
  size_t m_TokenIndex;
  TokenInfo m_CurrToken;
  TokenInfo m_PrevToken;
  std::vector<TokenInfo> m_TokenStream;
  std::vector<ASTNode> m_AST;
  bool m_ErrorFlag;
  bool m_ParsingEndingFlag;

  const char* tokenToStr(TokenKind kind) noexcept {
    return m_Lexer.tokenAsStr(kind);
  }

  // INT,FLOAT,...
  bool isType(const TokenInfo& token);

  // IMPORT, EXPORT, STATIC
  bool isLinkageMark(const TokenInfo& token);

  // PACKAGE, PACKAGE INVOLVED
  bool isPackageMark(const TokenInfo& token);

  // Is it what we look for?
  bool is(TokenKind token) noexcept {
    return m_TokenStream[m_TokenIndex] == token;
  }

  bool isOperator(const TokenInfo& token);
  bool isCastableOperator(const TokenInfo& token);

  // Is it matched with that what we look for?
  bool expected(TokenKind expected) {
    if (is(expected)) {
      return true;
    } else {
      // Error message here..
      this->m_ErrorFlag = false;
      auto currTokenStr = this->m_CurrToken.getTokenAsStr();
      std::cerr << "Unexpected token \"" << currTokenStr << "\" instead \""
                << tokenToStr(expected) << "\"" << std::endl;
      assert(false && "Unexpected token");
      return false;
    }
  }

  bool expected(std::initializer_list<TokenKind> expectedTokens) {
    bool isOkay = false;
    for (auto& expected : expectedTokens) {
      if (is(expected)) {
        isOkay = true;
        break;
      } else {
        isOkay = false;
      }
    }

    if (!isOkay) {
      // Error message here..
      this->m_ErrorFlag = false;
      auto currTokenStr = this->m_CurrToken.getTokenAsStr();
      std::cerr << "Unexpected token \"" << currTokenStr
                << "\" instead one of them ";

      for (auto& expected : expectedTokens)
        std::cout << "\"" << tokenToStr(expected) << "\" ";

      std::cout << std::endl;
      assert(false && "Unexpected token");
    }

    return isOkay;
  }

  void advance() {
    if (m_TokenIndex < m_TokenStream.size()) {
      this->m_PrevToken = m_TokenStream[m_TokenIndex];
      this->m_CurrToken = m_TokenStream[++m_TokenIndex];
      this->m_ParsingEndingFlag = false;
    } else {
      this->m_ParsingEndingFlag = true;
    }
  }

  TokenInfo prevTokenInfo() const noexcept { return this->m_PrevToken; }

  TokenKind prevTokenKind() const noexcept {
    return this->m_PrevToken.getTokenKind();
  }

  TokenInfo currentTokenInfo() const noexcept { return this->m_CurrToken; }

  TokenKind currentTokenKind() const noexcept {
    return this->m_CurrToken.getTokenKind();
  }

  // It has not a token kind version since has an argument
  // and it need to a lot of work to handle it in another function
  /// which it'll be a param as well.
  TokenInfo nextTokenInfo(bool& outOfSize) const noexcept {
    if (m_TokenIndex + 1 <= m_TokenStream.size()) {
      outOfSize = false;
      return m_TokenStream[m_TokenIndex + 1];
    } else {
      outOfSize = true;
      return TokenInfo();
    }
  }

  std::string currentTokenStr() const noexcept {
    return this->m_CurrToken.getTokenAsStr();
  }

  void addToPrecTable(OpType op, TokenKind kind, int prec, bool isLeftToRight) {
    switch (op) {
      case OpType::OP_UNARY:
        m_PrecTableUnary.emplace(kind, PrecedenceInfo(prec, isLeftToRight));

        break;
      case OpType::OP_BINARY:
        m_PrecTableBinary.emplace(kind, PrecedenceInfo(prec, isLeftToRight));

        break;
      case OpType::OP_CAST:
        m_PrecTableCast.emplace(kind, PrecedenceInfo(prec, isLeftToRight));
        break;
      default:
        std::cerr << "Not implemented yet!\n";
        break;
    }
  }

  // parser.cpp
  void translationUnit();
  Type typeOf(const TokenInfo& token);
  PositionInfo getPosInfo(TokenInfo tokenInfo);
  void ParserError(std::string mesg);

  // variable.cpp
  void varDecl();
  ASTNode initializer();
  ASTNode initializerList();
  size_t advancePointerType(bool isUniquePtr);

  // expr.cpp
  bool isUnaryOp();
  bool isBinOp();
  bool isCastOp();
  ASTNode reduceExpression(std::deque<ASTNode>& exprBucket,
                           OpPrecBucket& opPrecBucket);
  ASTNode expression(bool isSubExpr);
  ASTNode advanceConstantOrLiteral();
  ASTNode advanceRef();
  ASTNode advanceIndirect();
  ASTNode advanceFunctionCall();
  ASTNode advanceArraySubscript();
  ASTNode advanceType();
  ASTNode advanceSymbol();

  TypeSpecifier typeResolver(TokenInfo token);

 public:
  CStarParser(const CStarLexer&& pLexer)
      : m_Lexer(std::move(pLexer)),
        m_TokenIndex(0),
        m_ErrorFlag(false),
        m_ParsingEndingFlag(false) {
    addToPrecTable(OpType::OP_BINARY, COLONCOLON, 16, true);
    addToPrecTable(OpType::OP_BINARY, LPAREN, 15, true);
    addToPrecTable(OpType::OP_BINARY, LSQPAR, 15, true);

    // those are functional cast
    addToPrecTable(OpType::OP_CAST, UNSAFE_CAST, 15, true);
    addToPrecTable(OpType::OP_CAST, CAST, 15, true);

    addToPrecTable(OpType::OP_UNARY, LT, 15, true);

    // inceremental or decremental operator as suffix/postfix
    addToPrecTable(OpType::OP_UNARY, PLUSPLUS, 15, true);
    addToPrecTable(OpType::OP_UNARY, MINUSMINUS, 15, true);

    // member access operators
    addToPrecTable(OpType::OP_UNARY, DOT, 14, true);
    addToPrecTable(OpType::OP_UNARY, ARROW, 14, true);

    // inceremental or decremental operator as prefix
    addToPrecTable(OpType::OP_UNARY, PLUSPLUS, 13, false);
    addToPrecTable(OpType::OP_UNARY, MINUSMINUS, 13, false);

    addToPrecTable(OpType::OP_UNARY, SIZEOF, 13, false);
    addToPrecTable(OpType::OP_UNARY, TYPEOF, 13, false);

    // Well not sure right now but this probably must to consume
    // as a special case rather than a normal operator.
    addToPrecTable(OpType::OP_UNARY, MOVE, 13, false);

    //    addToPrecTable(OpType::OP_UNARY, INSTANCEOF, 13, false);
    addToPrecTable(OpType::OP_UNARY, DEREF, 13, false);
    addToPrecTable(OpType::OP_UNARY, REF, 13, false);
    addToPrecTable(OpType::OP_CAST, AS, 13, true);

    addToPrecTable(OpType::OP_UNARY, PLUS, 13, false);
    addToPrecTable(OpType::OP_UNARY, MINUS, 13, false);
    addToPrecTable(OpType::OP_UNARY, NOT, 13, false);
    addToPrecTable(OpType::OP_UNARY, TILDE, 13, false);
    addToPrecTable(OpType::OP_UNARY, STAR, 13, false);
    addToPrecTable(OpType::OP_UNARY, AND, 13, false);

    addToPrecTable(OpType::OP_BINARY, STAR, 11, true);
    addToPrecTable(OpType::OP_BINARY, MOD, 11, true);
    addToPrecTable(OpType::OP_BINARY, DIV, 11, true);
    addToPrecTable(OpType::OP_BINARY, PLUS, 10, true);
    addToPrecTable(OpType::OP_BINARY, MINUS, 10, true);

    addToPrecTable(OpType::OP_BINARY, LSHIFT, 9, true);
    addToPrecTable(OpType::OP_BINARY, RSHIFT, 9, true);
    addToPrecTable(OpType::OP_BINARY, LT, 8, true);
    addToPrecTable(OpType::OP_BINARY, LTEQ, 8, true);
    addToPrecTable(OpType::OP_BINARY, GT, 8, true);
    addToPrecTable(OpType::OP_BINARY, GTEQ, 8, true);
    addToPrecTable(OpType::OP_BINARY, EQUALEQUAL, 7, true);
    addToPrecTable(OpType::OP_BINARY, NOTEQUAL, 7, true);
    addToPrecTable(OpType::OP_BINARY, AND, 6, true);
    addToPrecTable(OpType::OP_BINARY, XOR, 5, true);
    addToPrecTable(OpType::OP_BINARY, OR, 4, true);
    addToPrecTable(OpType::OP_BINARY, LAND, 3, true);
    addToPrecTable(OpType::OP_BINARY, LOR, 2, true);

    addToPrecTable(OpType::OP_BINARY, EQUAL, 1, false);
    addToPrecTable(OpType::OP_BINARY, PLUSEQ, 1, false);
    addToPrecTable(OpType::OP_BINARY, MINUSEQ, 1, false);
    addToPrecTable(OpType::OP_BINARY, STAREQ, 1, false);
    addToPrecTable(OpType::OP_BINARY, DIVEQ, 1, false);
    addToPrecTable(OpType::OP_BINARY, MODEQ, 1, false);
    addToPrecTable(OpType::OP_BINARY, LSHIFTEQ, 1, false);
    addToPrecTable(OpType::OP_BINARY, RSHIFTEQ, 1, false);
    addToPrecTable(OpType::OP_BINARY, ANDEQ, 1, false);
    addToPrecTable(OpType::OP_BINARY, XOREQ, 1, false);
    addToPrecTable(OpType::OP_BINARY, OREQ, 1, false);
    addToPrecTable(OpType::OP_BINARY, COMMA, 1, false);
    // ternary op
    addToPrecTable(OpType::OP_BINARY, QMARK, 1, false);

    m_PrecTable[OpType::OP_UNARY] = std::move(m_PrecTableUnary);
    m_PrecTable[OpType::OP_BINARY] = std::move(m_PrecTableBinary);
    m_PrecTable[OpType::OP_CAST] = std::move(m_PrecTableCast);
  }

  void parse();
};

#endif
