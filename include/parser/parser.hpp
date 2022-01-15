#ifndef PARSER_HPP
#define PARSER_HPP
#include <lexer/lexer.hpp>
#include <parser/type_specifiers.hpp>
#include <parser/visibility_specifiers.hpp>
#include <ast/ast.hpp>
#include <cassert>

/*
  translation_unit :== 
                     var_decl 
		   | func_decl 
		   | proto_decl 
		   | attrib_decl 
		   | package
		   | macro
 

  var_decl        :==
                     type_resolver IDENT ';'
                   | type_resolver IDENT '=' initializer ';'
		   | storage_specifier type_resolver IDENT ';'
		   | storage_specifier type_resolver IDENT '=' initializer ';'

  type_resolver   :== 
                     type
                   | '[' type ']'
                   
  type            :==
                     {ANY_TYPE}
                   | {ANY_TYPE} pointer_type

  storage_specifier :== 		 
                     STATIC
		   | IMPORT
		   | EXPORT

  pointer_type    :==
                     *
                   | ^

  initializer     :== 
                     initializer_list
		   | expression  


  initializer_list :==
                   | ?

  
  expression      :==
                   | ?


  package         :== 
                     PACKAGE
                   | PACKAGE INVOLVED
                     

*/


class CStarParser {
  CStarLexer m_Lexer;
  size_t m_TokenIndex;
  TokenInfo m_CurrToken;
  std::vector<TokenInfo> m_TokenStream;
  std::vector<IAST> m_AST;
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
    std::cout << m_TokenStream[m_TokenIndex].getTokenAsStr() << std::endl;
    return m_TokenStream[m_TokenIndex] == token;
  }

  //Is it matched with we look for?
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

  void translationUnit();
  IAST varDecl();
  IAST initializer();
  TypeSpecifier typeResolver(TokenInfo token);

public:
  CStarParser(const CStarLexer && pLexer)
    : m_Lexer(std::move(pLexer)), m_TokenIndex(0),
      m_ErrorFlag(false), m_ParsingEndingFlag(false)
  {
    
  }

  void parse();
};


#endif
