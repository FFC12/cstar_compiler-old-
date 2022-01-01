#include <parser/parser.hpp>


void CStarParser::parse() {
  m_TokenStream = this->m_Lexer.perform();
  for(auto &token: m_TokenStream) {
    std::cout << token.getTokenAsStr() << std::endl;
  }
}


