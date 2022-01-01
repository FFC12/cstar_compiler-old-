#ifndef PARSER_HPP
#define PARSER_HPP
#include <lexer/lexer.hpp>

class CStarParser {
  CStarLexer m_Lexer;
  std::vector<TokenInfo> m_TokenStream;
  
public:
  CStarParser(const CStarLexer && pLexer): m_Lexer(std::move(pLexer))
  {
  }

  void parse();
};


#endif
