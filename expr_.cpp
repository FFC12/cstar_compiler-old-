#include <parser/parser.hpp>

/*
        int p = x + 2 / 3;
        int* r = &p;
        uint k = unsafe_cast<uint>(p) + 25 + (*r * 2) + (true ? 1 : 2) +
   funcCall(0,1,2,3) + arr_[2] + arr[2][3];
*/

// if there is an operator before it
// or it's the first operator in the expression
// then [probably] it's an unary operator
bool CStarParser::isUnaryOp() {
  if (isOperator(this->prevTokenInfo())) {
    return true;
  } else {
    return false;
  }
}

// if it's not a unary or cast operator then it is binary operator
bool CStarParser::isBinOp() {
  return (this->isUnaryOp() && this->isCastOp()) ? false : true;
}

// check if it's cast operator
bool CStarParser::isCastOp() {
  return this->isCastableOperator(this->currTokenInfo());
}

// Note: When we get '?' token from stream, we'll look for first ':'
// and then try to parse it as ternary operator ofc
// but in our custom algorithm, that might be a little confusing
// to deal with that so here's the sub-algorithm that can be followed:
//- first advance '?' until ':'
//- take all relevant atoms with ops (?, :)
//- and reduce that ternary operator with atoms to an ast_node
ASTNode CStarParser::expression() {
  // advance the EQUAL here.
  this->advance();

  ExprPrecBucket exprBucket;
  OpPrecBucket opBucket;
  bool isFirst = true;
  bool isLast = false;

  size_t i = 0;
  while (true) {
    this->advance();

    // When we have LPAREN, that means we need to
    // call the function recursively for returning ASTNode
    // this is for parsing subexpression in harmony with OpPrec.
    // When we have any Op, that means we need to
    //
    if (is(TokenKind::LPAREN)) {
      auto subExpr = this->expression();

      bool isOutOfSize = false;
      auto nextToken = this->nextTokenInfo(isOutOfSize).getTokenKind();
      if (isOutOfSize)
        assert(false && "The stream is done but still yet paranthesis '(' did "
                        "not closed ')'");

      bool isLast = (nextToken == TokenKind::RPARAN) ? true : false;
                        exprBucket.push_back(PrecedenceEntry(this->currentTokenKind(),PrecedenceInfo(),i,false,true,isFirst,isLast);
                } else if(isOperator(this->currentTokenInfo()) {
      PrecedenceInfo precInfo;
      OpType opType = OpType::OP_UNARY;

      if (this->isUnaryOp()) {
        opType = OpType::OP_UNARY;
      } else if (this->isBinOp()) {
        opType = OpType::OP_BINARY;
      } else if (this->isCastOp) {
        opType = OpType::OP_CAST;
      } else {
        assert(false && "Operator Prec: unreachable!");
      }

      // TODO: check the precInfo, it might be '0' and not found in the hashmap.
      precInfo = this->m_PrecTable[opType][this->currentTokenKind()];

      bool isOutOfSize = false;
      auto nextToken = this->nextTokenInfo(isOutOfSize).getTokenKind();
      if (isOutOfSize)
        assert(false && "The stream is done but still yet paranthesis '(' did "
                        "not closed ')'");

      bool isFirst = i == 0 ? true : false;
      bool isLast =
          (nextToken == TokenKind::RPARAN || nextToken == TokenKind::SEMICOLON)
              ? true
              : false;
                        exprBucket.push_back(PrecedenceEntry(this->currentTokenKind(), precInfo, i, isFirst, isLast), 

			if(is(TokenKind::SEMICOLON) || is(TokenKind::RPAREN)) break;
		} else {
			
		}

		i += 1;
  }

  // and perform	parsing the expression by recursive-descent way.
  return this->advanceConstantOrLiteral();
}

ASTNode CStarParser::advanceConstantOrLiteral() {
  // this is obviously a scalar or literal(matrix and vec?)
  if (is(TokenKind::SCALARD) || is(TokenKind::SCALARI) ||
      is(TokenKind::LITERAL)) {
    bool isIntegral = is(TokenKind::LITERAL) ? false : true;
    bool isFloat = is(TokenKind::SCALARD) ? true : false;

    auto value = this->currentTokenStr();
    return std::make_unique<ScalarAST>(value, isIntegral, isFloat);
  }

  return this->advanceRef();
}

ASTNode CStarParser::advanceRef() {
  //& ref
  if (is(TokenKind::AND) || is(TokenKind::REF)) {
  }
}

ASTNode CStarParser::advanceIndirect() {
  //* ^ deref
  if (is(TokenKind::STAR) || is(TokenKind::XOR) || is(TokenKind::DEREF)) {
  }
}

ASTNode CStarParser::advanceBinOp() {}

ASTNode CStarParser::advanceUnaryOp() {}

ASTNode CStarParser::advanceFunctionCall() {}

ASTNode CStarParser::advanceArraySubscript() {}
