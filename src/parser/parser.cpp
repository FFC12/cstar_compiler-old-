#include <parser/parser.hpp>

void CStarParser::parse() {
  m_TokenStream = this->m_Lexer.perform();
  if(this->m_TokenStream.size() >= 1) {
    this->m_CurrToken = m_TokenStream[m_TokenIndex]; //[0]
    this->translationUnit();
  } else {
    assert(false && "Parser token stream is not enough to parse!\n");
  }
}


void CStarParser::translationUnit() {
  while(!this->m_ParsingEndingFlag) {
		//std::cout << this->m_CurrToken.getTokenAsStr() << "\n";
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
				} else {
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
	case INT:
  case U8:
  case U16:
  case U32:
  case U64:
  case U128:
	case UINT:
  case ISIZE:
  case USIZE:
  case F32:
  case F64:
	case FLOAT:
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

bool CStarParser::isCastableOperator(TokenInfo token) {
	switch(token.getTokenKind()) {
		case UNSAFE_CAST:
		case CAST:
		case AS:
			return true;
		default:
			return false;
	}	
}


bool CStarParser::isOperator(TokenInfo token) {
  switch(token.getTokenKind()) {
    case LPAREN:
    case COLONCOLON:
    case UNSAFE_CAST:
    case CAST:
    case PLUSPLUS:
    case MINUSMINUS:
    case DOT:
    case ARROW:
    case SIZEOF:
    case DEREF:
    case REF:
    case AS:
    case PLUS:
    case MINUS:
    case NOT:
    case TILDE:
    case STAR:
    case MOD:
    case DIV:
    case LSHIFT:
    case RSHIFT:
    case LT:
    case LTEQ:
    case GT:
    case GTEQ:
    case EQUALEQUAL:
    case NOTEQUAL:
    case AND:
    case XOR:
    case OR:
    case LAND:
    case LOR:
    case EQUAL:
    case PLUSEQ:
    case MINUSEQ:
    case STAREQ:
    case DIVEQ:
    case MODEQ:
    case LSHIFTEQ:
    case RSHIFTEQ:
    case ANDEQ:
    case XOREQ:
    case OREQ:
    case COMMA:
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

