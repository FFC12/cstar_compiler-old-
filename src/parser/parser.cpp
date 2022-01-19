#include <parser/parser.hpp>

void CStarParser::parse() {
  m_TokenStream = this->m_Lexer.perform();
  if(this->m_TokenStream.size() >= 1) {
    this->m_CurrToken = m_TokenStream[m_TokenIndex]; //[0]
    this->translationUnit();
  } else {
    assert(true && "Parser token stream is not enough to parse!\n");
  }
}


void CStarParser::translationUnit() {
  while(!this->m_ParsingEndingFlag) {
    //TODO(1): EOF cannot be processed
    if(this->m_CurrToken.getTokenKind() == TokenKind::_EOF) {
      std::cout << "EOF\n";
      break;
    }

    if(is(TokenKind::COMMENT)) {
      this->advance();
      continue;
    }

    if(isPackageMark(this->m_CurrToken)){
      //package | package involved
    } else {
      //export | import
      if(isLinkageMark(this->m_CurrToken)){
	this->advance();
	if(this->isType(this->m_CurrToken)){

	}
      } else {
	// int* | float* | uint* ...
	if(this->isType(this->m_CurrToken)){
		varDecl();	  
	}
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

