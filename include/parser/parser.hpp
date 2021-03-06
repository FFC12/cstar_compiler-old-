#ifndef PARSER_HPP
#define PARSER_HPP
#include <ast/assignment_ast.hpp>
#include <ast/ast.hpp>
#include <ast/binary_op_ast.hpp>
#include <ast/cast_op_ast.hpp>
#include <ast/fix_ast.hpp>
#include <ast/func_ast.hpp>
#include <ast/func_call_ast.hpp>
#include <ast/if_stmt.hpp>
#include <ast/loop_stmt.hpp>
#include <ast/param_ast.hpp>
#include <ast/ret_ast.hpp>
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
#include <parser/hint_qualifier.hpp>
#include <parser/op_prec.hpp>
#include <parser/type_specifiers.hpp>
#include <parser/visibility_specifiers.hpp>
#include <queue>

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
  time_t m_StartTime;
  bool m_LocalScopeFlag;

  const char* tokenToStr(TokenKind kind) noexcept {
    return m_Lexer.tokenAsStr(kind);
  }

  // INT,FLOAT,...
  bool isType(const TokenInfo& token);

  // IMPORT, EXPORT, STATIC
  bool isLinkageMark(const TokenInfo& token);

  // INCLUDE, INCLUDE INVOLVED
  bool isPackageMark(const TokenInfo& token);

  // CONST, CONSTREF ,...
  bool isTypeQualifier(const TokenInfo& token);

  // '+=', '-=', ...
  bool isShortcutOp(const TokenInfo& token);

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
      std::string mesg = "Unexpected token \"" + currTokenStr +
                         "\" instead of \"" + tokenToStr(expected) + "\"";

      ParserError(mesg, currentTokenInfo());
      //      assert(false && "Unexpected token");
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
      std::string mesg =
          "Unexpected token \"" + currTokenStr + "\" instead of ";

      for (int i = 0; i < expectedTokens.size(); i++) {
        mesg += std::string("\"") + tokenToStr(*(expectedTokens.begin() + i)) +
                std::string("\"");

        if (i != expectedTokens.size() - 1) {
          mesg += " or ";
        }
      }

      ParserError(mesg, currentTokenInfo());
      //      assert(false && "Unexpected token");
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

  TokenInfo prevTokenInfo() noexcept { return this->m_PrevToken; }

  TokenKind prevTokenKind() const noexcept {
    return this->m_PrevToken.getTokenKind();
  }

  TokenKind prevOfPrevTokenKind() const noexcept {
    return m_TokenStream[m_TokenIndex - 2].getTokenKind();
  }

  TokenInfo prevOfPrevTokenInfo() const noexcept {
    return m_TokenStream[m_TokenIndex - 2];
  }

  TokenInfo currentTokenInfo() const noexcept { return this->m_CurrToken; }

  TokenKind currentTokenKind() const noexcept {
    return this->m_CurrToken.getTokenKind();
  }

  // It has not a token kind version since has an argument
  // and it need to a lot of work to handle it in another function
  /// which it'll be a param as well.
  TokenInfo nextTokenInfo(bool& outOfSize) const {
    if (m_TokenIndex + 1 <= m_TokenStream.size()) {
      outOfSize = false;
      return m_TokenStream[m_TokenIndex + 1];
    } else {
      outOfSize = true;
      return {};
    }
  }

  TokenInfo nextTokenInfo(bool& outOfSize, size_t n) const {
    if (m_TokenIndex + 1 <= m_TokenStream.size()) {
      outOfSize = false;
      return m_TokenStream[m_TokenIndex + n];
    } else {
      outOfSize = true;
      return {};
    }
  }

  void restoreToken(size_t n) {
    this->m_TokenIndex -= n;
    m_CurrToken = this->m_TokenStream[this->m_TokenIndex];
    m_PrevToken = this->m_TokenStream[this->m_TokenIndex - 1];
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
  ShortcutOp typeOfShortcutOp(const TokenInfo& token);
  Type typeOf(const TokenInfo& token);
  TypeQualifier typeQualifierOf(const TokenInfo& tokenInfo);
  void ParserHint(std::string mesg, TokenInfo tokenInfo);
  void ParserHint(std::string mesg, TokenInfo tokenInfo, size_t new_begin);
  void ParserError(const std::string& mesg, TokenInfo tokenInfo);

  void ParserError(std::string mesg, TokenInfo tokenInfo, size_t new_begin);
  std::string_view::iterator viewLine(size_t line, size_t& rlbegin,
                                      size_t& rlend, size_t& offset);

  // variable.cpp
  void varDecl(TypeQualifier typeQualifier,
               VisibilitySpecifier visibilitySpecifier, bool definedType,
               bool isLocal, std::vector<ASTNode>* scope = nullptr);
  DeclKind getDeclKind(VisibilitySpecifier visibilitySpecifier);
  ASTNode initializer();
  bool advanceTypeSubscript(std::vector<ASTNode>&);
  ASTNode initializerList();
  size_t advancePointerType(bool isUniquePtr);
  TypeSpecifier typeSpecifierOf(const TokenInfo& tokenInfo);

  // function.cpp
  void funcDecl(VisibilitySpecifier visibilitySpecifier);
  void advanceParams(std::vector<ASTNode>& params, bool isForwardDecl);
  void advanceScope(std::vector<ASTNode>& scope);

  // branch.cpp
  void advanceIfStmt(std::vector<ASTNode>& scope);

  // loop.cpp
  void advanceLoopStmt(std::vector<ASTNode>& scope);

  // expr.cpp
  bool isUnaryOp();
  bool isBinOp();
  bool isCastOp();
  ASTNode reduceExpression(std::deque<ASTNode>& exprBucket,
                           OpPrecBucket& opPrecBucket);
  ASTNode expression(bool isSubExpr, int opFor = 0, bool isRet = false,
                     bool typeFlag = false, bool isAssignment = false);
  ASTNode advanceConstantOrLiteral();
  ASTNode advanceType();
  ASTNode advanceSymbol();

  TypeSpecifier typeResolver(TokenInfo token);

  void parserStats() const {
    time_t endTime = time(nullptr);
    std::cout << GRN "======= Syntantic Analysis =======" RES << std::endl;
    double dif = difftime(endTime, this->m_StartTime);
    printf("-  Elapsed time : %.2lf seconds\n\n", dif);
  }

 public:
  explicit CStarParser(CStarLexer&& pLexer)
      : m_Lexer(pLexer),
        m_TokenIndex(0),
        m_ErrorFlag(false),
        m_ParsingEndingFlag(false),
        m_LocalScopeFlag(false) {
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
    addToPrecTable(OpType::OP_BINARY, DOT, 14, true);
    addToPrecTable(OpType::OP_BINARY, ARROW, 14, true);

    // inceremental or decremental operator as prefix
    // we handle this in the isOperator check manually
    // addToPrecTable(OpType::OP_UNARY, PLUSPLUS, 13, false);
    // addToPrecTable(OpType::OP_UNARY, MINUSMINUS, 13, false);

    addToPrecTable(OpType::OP_UNARY, SIZEOF, 13, false);
    addToPrecTable(OpType::OP_UNARY, TYPEOF, 13, false);
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

    //    addToPrecTable(OpType::OP_BINARY, EQUAL, 1, false);
    /* addToPrecTable(OpType::OP_BINARY, PLUSEQ, 1, false);
     addToPrecTable(OpType::OP_BINARY, MINUSEQ, 1, false);
     addToPrecTable(OpType::OP_BINARY, STAREQ, 1, false);
     addToPrecTable(OpType::OP_BINARY, DIVEQ, 1, false);
     addToPrecTable(OpType::OP_BINARY, MODEQ, 1, false);
     addToPrecTable(OpType::OP_BINARY, LSHIFTEQ, 1, false);
     addToPrecTable(OpType::OP_BINARY, RSHIFTEQ, 1, false);
     addToPrecTable(OpType::OP_BINARY, ANDEQ, 1, false);
     addToPrecTable(OpType::OP_BINARY, XOREQ, 1, false);
     addToPrecTable(OpType::OP_BINARY, OREQ, 1, false);
     */
    addToPrecTable(OpType::OP_BINARY, COMMA, 1, false);

    // ternary op
    addToPrecTable(OpType::OP_BINARY, QMARK, 1, true);
    addToPrecTable(OpType::OP_BINARY, COLON, 0, false);

    m_PrecTable[OpType::OP_UNARY] = std::move(m_PrecTableUnary);
    m_PrecTable[OpType::OP_BINARY] = std::move(m_PrecTableBinary);
    m_PrecTable[OpType::OP_CAST] = std::move(m_PrecTableCast);
  }

  // for semantics analysis
  void ParserError(std::string mesg, size_t begin, size_t end, size_t line);
  void ParserWarning(std::string mesg, size_t newBegin, size_t newEnd,
                     size_t newLine);

  void parse();
  void ownedAST(std::vector<ASTNode>& newOwner) {
    newOwner = std::move(this->m_AST);
  }
};

#endif
