#ifndef PARSER_HPP
#define PARSER_HPP
#include <cassert>
#include <memory>
#include <base.hpp>
#include <lexer/lexer.hpp>
#include <parser/type_specifiers.hpp>
#include <parser/visibility_specifiers.hpp>
#include <parser/op_prec.hpp>
#include <ast/ast.hpp>
#include <ast/var_ast.hpp>
#include <ast/scalar_ast.hpp>

using ASTNode = std::unique_ptr<IAST>; 

class CStarParser {
	PrecedenceInfoTable m_PrecTableUnary, m_PrecTableBinary, m_PrecTableCast;
  PrecedenceTable m_PrecTable;
  CStarLexer m_Lexer;
  size_t m_TokenIndex;
  TokenInfo m_CurrToken;
  std::vector<TokenInfo> m_TokenStream;
  std::vector<ASTNode> m_AST;
  bool m_ErrorFlag;
  bool m_ParsingEndingFlag;

  const char* tokenToStr(TokenKind kind) {
    return m_Lexer.tokenAsStr(kind);
  }
  
  //INT,FLOAT,...
  bool isType(TokenInfo token);
  
  //IMPORT, EXPORT, STATIC
  bool isLinkageMark(TokenInfo token);
  
  //PACKAGE, PACKAGE INVOLVED
  bool isPackageMark(TokenInfo token);
  
  //Is it what we look for?
  bool is(TokenKind token) { 
    return m_TokenStream[m_TokenIndex] == token;
  }

  bool isOperator(TokenInfo token);

  //Is it matched with that what we look for?
  bool expected(TokenKind expected) {
    if(is(expected)) {
      return true;
    } else {
      //Error message here..
      this->m_ErrorFlag = false;
      auto currTokenStr = this->m_CurrToken.getTokenAsStr();
      std::cerr << "Unexpected token \"" << currTokenStr << "\" instead \"" << tokenToStr(expected) << "\"" <<  std::endl; 
      return false;
    }
  }

  void advance() {
    if (m_TokenIndex < m_TokenStream.size()) {
      this->m_CurrToken = m_TokenStream[++m_TokenIndex];
      this->m_ParsingEndingFlag = false;
    } else {
      this->m_ParsingEndingFlag = true;
    }
  }
	
	TokenKind currentTokenKind() const noexcept {
		return this->m_CurrToken.getTokenKind();
	}

	std::string currentTokenStr() const noexcept {
		return this->m_CurrToken.getTokenAsStr();
	}

  void addToPrecTable(OpType op,TokenKind kind, int prec, bool isLeftToRight) {
    switch(op) {
      case OpType::OP_UNARY:
        m_PrecTableUnary[kind] = PrecedenceInfo(prec,isLeftToRight);
        break;
      case OpType::OP_BINARY:
        m_PrecTableBinary[kind] = PrecedenceInfo(prec,isLeftToRight);
        break;
      case OpType::OP_CAST:
        m_PrecTableCast[kind] = PrecedenceInfo(prec,isLeftToRight);
        break;
      default:
        std::cerr << "Not implemented yet!\n";
        break;
    }
  }

	//parser.cpp
  void translationUnit();

	//variable.cpp
  void varDecl();
	ASTNode initializer();
	ASTNode initializerList();
	size_t advancePointerType(bool isUniquePtr);

	//expr.cpp
	ASTNode expression();
	ASTNode advanceConstantOrLiteral();
	ASTNode advanceRef();
	ASTNode advanceIndirect();
	ASTNode advanceBinOp();
	ASTNode advanceUnaryOp();
	ASTNode advanceFunctionCall();
	ASTNode advanceArraySubscript();

  TypeSpecifier typeResolver(TokenInfo token);

public:
  //Interesting topic.. When you overload an operator - what's happening to precedence -
  CStarParser(const CStarLexer && pLexer)
    : m_Lexer(std::move(pLexer)), m_TokenIndex(0),
      m_ErrorFlag(false), m_ParsingEndingFlag(false)
  {
    addToPrecTable(OpType::OP_BINARY, COLONCOLON, 16, true);
    //Paranthesis will be interpreted as sub-expression and will call the function itself again (recursively)
    //addToPrecTable(OpType:: ,LPAREN, 15, true);
    //addToPrecTable(OpType::OP_BINARY, LSQPAR, 15, true);

    //those are functional cast
    addToPrecTable(OpType::OP_CAST, UNSAFE_CAST, 15, true);
    addToPrecTable(OpType::OP_CAST, CAST, 15, true);

    //inceremental or decremental operator as suffix/postfix
    addToPrecTable(OpType::OP_UNARY, PLUSPLUS, 15, true);
    addToPrecTable(OpType::OP_UNARY, MINUSMINUS, 15, true);

    //member access operators
    addToPrecTable(OpType::OP_UNARY, DOT, 14, true);
    addToPrecTable(OpType::OP_UNARY, ARROW, 14, true);

    //inceremental or decremental operator as prefix
    addToPrecTable(OpType::OP_UNARY, PLUSPLUS, 13, false);
    addToPrecTable(OpType::OP_UNARY, MINUSMINUS, 13, false);

    addToPrecTable(OpType::OP_UNARY, SIZEOF, 13, false);
//    addToPrecTable(OpType::OP_UNARY, INSTANCEOF, 13, false);
    addToPrecTable(OpType::OP_UNARY, DEREF, 13, false);
    addToPrecTable(OpType::OP_UNARY, REF, 13, false);
    addToPrecTable(OpType::OP_CAST , AS, 13, true);

    addToPrecTable(OpType::OP_UNARY, PLUS, 13, false);
    addToPrecTable(OpType::OP_UNARY, MINUS, 13, false);
    addToPrecTable(OpType::OP_UNARY, NOT, 13, false);
    addToPrecTable(OpType::OP_UNARY, TILDE, 13, false);
    addToPrecTable(OpType::OP_UNARY, STAR, 13, false);
    addToPrecTable(OpType::OP_UNARY, AND, 13, false);

    addToPrecTable(OpType::OP_BINARY, STAR, 11, true);
    addToPrecTable(OpType::OP_BINARY, MOD, 11, true);
    addToPrecTable(OpType::OP_BINARY, DIV, 11, true);
    addToPrecTable(OpType::OP_BINARY, PLUS, 11, true);
    addToPrecTable(OpType::OP_BINARY, MINUS, 11, true);
    
    addToPrecTable(OpType::OP_BINARY, LSHIFT, 10, true);
    addToPrecTable(OpType::OP_BINARY, RSHIFT, 10, true);
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
    addToPrecTable(OpType::OP_UNARY, QMARK, 1, false);
		//can't show ternary operator here since it has 2 different part
		//it will be handled as an exception

    m_PrecTable[OpType::OP_UNARY] = m_PrecTableUnary;
    m_PrecTable[OpType::OP_BINARY] = m_PrecTableBinary;
    m_PrecTable[OpType::OP_CAST] = m_PrecTableCast;

  }

  void parse();
};


#endif
