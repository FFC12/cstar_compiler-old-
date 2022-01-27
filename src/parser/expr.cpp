#include <algorithm>
#include <parser/parser.hpp>

// if there is an operator before it (rely on this maybe not good idea?)
// or it's the first operator in the expression
// then [probably] it's an unary operator
bool CStarParser::isUnaryOp() {
  if (isOperator(this->prevTokenInfo()) && !this->isCastOp()) {
    return true;
  } else {
    return false;
  }
}

// if it's not a unary or cast operator then it is binary operator
bool CStarParser::isBinOp() {
  return this->isUnaryOp() ? false : (!this->isCastOp() ? true : false);
}

// check if it's cast operator
bool CStarParser::isCastOp() {
  return this->isCastableOperator(this->currentTokenInfo());
}

// Note: When we get '?' token from stream, we'll look for the first ':'
// and then try to parse it as ternary operator ofc
// but in our custom algorithm, that might be a little confusing
// to deal with that so here's the sub-algorithm that can be followed:
//- first advance '?' until ':'
//- take all relevant atoms with ops (?, :)
//- and reduce that ternary operator with atoms to a simple ast_node
ASTNode CStarParser::expression(bool isSubExpr) {
  // advance the EQUAL here.
  this->advance();

  std::unordered_map<size_t, ASTNode> exprBucket;

  OpPrecBucket opBucket;

  bool isFirst = true;
  bool isLast = false;
  bool typeAttrib = false;

  size_t closedPar = isSubExpr ? 1 : 0;
  size_t i = 0;

  while (true) {
    // this->advance();

    // well RPAREN checking is a little bit confused since LPAREN is not
    // included to the expression itself example: if( expr )
    if (is(TokenKind::SEMICOLON) || is(TokenKind::RPAREN) ||
        is(TokenKind::GT)) {
      if (is(TokenKind::RPAREN)) closedPar += 1;
      break;
    }

    if (is(TokenKind::LPAREN) && this->isUnaryOp()) {
      closedPar += 1;
      auto subExpr = this->expression(true);
      closedPar += 1;  // it's always returning back after handled that
                       // paranthesis '(' ')'.

      bool isOutOfSize = false;
      auto nextToken = this->nextTokenInfo(isOutOfSize).getTokenKind();
      if (isOutOfSize)
        assert(false &&
               "The stream is done but still yet paranthesis '(' did "
               "not closed ')'");

      bool isLast = (nextToken == TokenKind::RPAREN) ? true : false;
      exprBucket[i] = std::move(subExpr);
    } else if (is(TokenKind::LT) && this->isUnaryOp()) {  // <TYPE>
      // advance '<'
      auto typeAst = this->expression(true);
      exprBucket[i] = std::move(typeAst);

    } else if (isOperator(this->currentTokenInfo())) {
      PrecedenceInfo precInfo;
      PrecedenceInfoTable precTableType;
      OpType opType = OpType::OP_UNARY;

      if (this->isUnaryOp()) {
        opType = OpType::OP_UNARY;
      } else if (this->isBinOp()) {
        opType = OpType::OP_BINARY;
      } else if (this->isCastOp()) {
        opType = OpType::OP_CAST;
      } else {
        assert(false && "Operator Prec: unreachable!");
      }

      // check the operator existence, it might be '0' and not found in the
      // hashmap.
      precTableType = this->m_PrecTable[opType];
      if (precTableType.count(this->currentTokenKind()) == 0)
        assert(false && "Operator Prec: critical unreacheable!");

      precInfo = precTableType[this->currentTokenKind()];

      // debuggin purpose
      std::cout << precInfo.getPrec() << std::endl;

      bool isOutOfSize = false;
      auto nextToken = this->nextTokenInfo(isOutOfSize).getTokenKind();
      if (isOutOfSize)
        assert(
            false &&
            "The stream is done but left paranthesis '(' did not closed ')'");

      bool isFirst = i == 0 ? true : false;
      bool isLast =
          (nextToken == TokenKind::RPAREN || nextToken == TokenKind::SEMICOLON)
              ? true
              : false;
      bool hasTypeAttrib = (nextToken == TokenKind::LT) ? true : false;

      opBucket.push_back(PrecedenceEntry(this->currentTokenKind(), opType,
                                         std::move(precInfo), i, isFirst,
                                         isLast, hasTypeAttrib));

      this->advance();
    } else {
      // and perform	parsing the expression by recursive-descent way.
      auto node = this->advanceConstantOrLiteral();
      exprBucket[i] = std::move(node);
    }

    i += 1;
  }

  // sorted the operators by preceding level
  std::sort(opBucket.rbegin(), opBucket.rend());

  // decide: is it belongs to the initialization expression or statement
  // expression
  if (is(TokenKind::SEMICOLON)) {  // initialization
    // let's be sure that all the open parenthesis are closed or not?
    if (closedPar % 2 != 0) {
      assert(false && ") mismatch");
    }

    // build top-level AST here..
    auto expr = this->reduceExpression(exprBucket, opBucket);
  } else if (is(TokenKind::RPAREN)) {
    this->advance();

    if (opBucket.size() == 0)
      return std::move(exprBucket[0]);
    else {
      auto expr = this->reduceExpression(exprBucket, opBucket);
      return std::move(expr);
    }
  } else if (is(TokenKind::GT)) {
    // must be only 1 ast node as TypeAST
    if (exprBucket.size() > 1 || exprBucket.size() == 0)
      std::cerr << "It must be only type\n";

    // advance >
    this->advance();

    //    ASTNode expr = std::move(exprBucket[0]);

    return std::move(exprBucket[0]);
  } else {
    assert(false && "Operator Prec: unreacheable 2!");
  }

  // return this->advanceConstantOrLiteral();
}

ASTNode CStarParser::reduceExpression(
    const std::unordered_map<size_t, ASTNode>& exprBucket,
    const OpPrecBucket& opPrecBucket) {

	auto boundCheck = [&](size_t i) {
		if(exprBucket.count(i) == 0) {
			return false;
		} else {
			return true;
		}
	};

  for (auto& op: opPrecBucket) {
		if (op.entryOpType() == OpType::OP_UNARY) {

		} else if (op.entryOpType() == OpType::OP_BINARY) {

		} else if (op.entryOpType() == OpType::OP_CAST) {
		
		} else {
			assert(false && "Unreacheable!");
			return nullptr; //for compiler warning...
		}
  }
}

ASTNode CStarParser::advanceConstantOrLiteral() {
  // this is obviously a scalar or literal or others(matrix and vec?)
  if (is(TokenKind::SCALARD) || is(TokenKind::SCALARI) ||
      is(TokenKind::LITERAL)) {
    bool isIntegral = is(TokenKind::LITERAL) ? false : true;
    bool isFloat = is(TokenKind::SCALARD) ? true : false;

    auto value = this->currentTokenStr();
    this->advance();
    return std::make_unique<ScalarAST>(value, isIntegral, isFloat);
  }

  return this->advanceRef();
}

ASTNode CStarParser::advanceRef() {
  //& ref
  if (is(TokenKind::AND) || is(TokenKind::REF)) {
  }

  return this->advanceType();
}

ASTNode CStarParser::advanceType() {
  if (this->isType(this->currentTokenInfo())) {
    ASTNode typeAst =
        std::make_unique<TypeAST>(typeOf(this->currentTokenInfo()));

    // advance type
    this->advance();

    // expected >
    expected(TokenKind::GT);

    return std::move(typeAst);
  }

  return this->advanceIndirect();
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
