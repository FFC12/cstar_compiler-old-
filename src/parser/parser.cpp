#include <parser/parser.hpp>

void CStarParser::parse() {
  m_TokenStream = this->m_Lexer.perform();
  this->translationUnit();
}


void CStarParser::translationUnit() {
  TokenInfo token;
  while(this->advance(&token)) {
    if(&token == nullptr)
      abort();

    std::cout << token.getTokenAsStr() << " ";
    //TODO(1): EOF cannot be processed
    if(token.getTokenKind() == TokenKind::_EOF) {
      std::cout << "EOF\n";
      break;
    }

    if(isPackageMark(token)){
      //package | package involved
    } else {
      if(isLinkageMark(token)){
	this->advance(&token);
	if(this->isType(token)){
	  
	}
      } else {
	
      }
    }
  }
}

bool CStarParser::isType(TokenInfo token) {
  switch(token.getTokenKind()) {
  case I8:
  case I16:
  case I32:
  case I64:
  case U8:
  case U16:
  case U32:
  case U64:
  case U128:
  case ISIZE:
  case USIZE:
  case F32:
  case F64:
  case UCHAR:
  case CHAR:
  case BOOL:
  case VEC2:
  case VEC3:
  case VEC4:
  case IDENT:
    return true;
  default:
    return false;
  }
}

bool CStarParser::isLinkageMark(TokenInfo token){
  switch(token.getTokenKind()) {
  case EXPORT:
  case IMPORT:
    return true;
  default:
    return false;
  }
}

bool CStarParser::isPackageMark(TokenInfo token){
  switch(token.getTokenKind()){
  case PACKAGE:
    return true;
  default:
    return false;
  }
}

