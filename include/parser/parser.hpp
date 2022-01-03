#ifndef PARSER_HPP
#define PARSER_HPP
#include <lexer/lexer.hpp>
#include <parser/type_specifiers.hpp>
#include <parser/visibility_specifiers.hpp>
#include <ast/ast.hpp>


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
                     

*/


class CStarParser {
  CStarLexer m_Lexer;
  size_t m_TokenIndex;
  std::vector<TokenInfo> m_TokenStream;
  std::vector<IAST> m_AST;

  //INT,FLOAT,...
  bool isType(TokenInfo token);
  
  //IMPORT, EXPORT, STATIC
  bool isLinkageMark(TokenInfo token);
  
  //PACKAGE, PACKAGE INVOLVED
  bool isPackageMark(TokenInfo token);
  
  //IS TOKEN MATCHED WITH STREAM's CURRENT TOKEN HAS INDEXED
  bool is(TokenInfo token) {
    return m_TokenStream[m_TokenIndex] == token;
  }

  bool advance(TokenInfo* currToken) {
    if (m_TokenIndex < m_TokenStream.size()) {
      *currToken = m_TokenStream[m_TokenIndex++];
      return true;
    } else {
      currToken = nullptr;
      return false;
    }
  }

  void translationUnit();
  IAST varDecl();
  TypeSpecifier typeResolver(TokenInfo token);
public:
  CStarParser(const CStarLexer && pLexer)
    : m_Lexer(std::move(pLexer)), m_TokenIndex(0)
  {
  }

  void parse();
};


#endif
