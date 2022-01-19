#include <parser/parser.hpp>

IAST CStarParser::varDecl() {
  //TypeSpecifier typeSpec = this->typeResolver()
  this->advance();
  if(expected(TokenKind::IDENT)){
    std::cout << "This is a name -> " << this->m_CurrToken.getTokenAsStr() << "\n";
    this->advance();
    switch(this->m_CurrToken.getTokenKind()) {
    case TokenKind::SEMICOLON:
      std::cout << "Variable declaration is done\n";
      this->advance();
      break;
    case TokenKind::EQUAL:
      this->advance();
      initializer();
      break;
    }
  }
}

//TODO: We need to have done that expression parsing rule.
IAST CStarParser::initializer() {
  std::cout << "Initializer\n";
	std::cout << "This is a value -> " << this->m_CurrToken.getTokenAsStr() << "\n";
  this->advance();
  expected(TokenKind::SEMICOLON);
  std::cout << "Variable declaration is done\n";
  this->advance();
}

IAST CStarParser::initializer_list() {
		
}

IAST CStarParser::expression() {
	
}
