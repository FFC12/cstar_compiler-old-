#include <algorithm>
#include <parser/parser.hpp>

// if there is an operator before it (i rely on this and maybe not good idea?)
// or if it's the first operator in the expression
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
// Note 2 : For function call and  subscript, wee need to think that operators
// are binary operators and just like '?' operator, we can consume and reduce
// them into one single atom.
ASTNode CStarParser::expression(bool isSubExpr) {
  // advance EQUAL.
  this->advance();

  // std::unordered_map<size_t, ASTNode> exprBucket;
  std::deque<ASTNode> exprBucket;

  OpPrecBucket opBucket;

  size_t closedPar = isSubExpr ? 1 : 0;
  size_t lastTypeAttribPos = -1;
  size_t lastCastOpPos = -1;
  size_t i = 0;
  size_t stride = 0;

  while (true) {
    // well RPAREN checking is a little bit confused since LPAREN is not
    // included to the expression itself example: if( [expr )]
    if (is(TokenKind::SEMICOLON) || is(TokenKind::RPAREN) ||
        is(TokenKind::GT)) {
      if (is(TokenKind::RPAREN)) closedPar += 1;
      break;
    }

    // this is for function call
    // Todo: lastCastOpPos and TypeAttribPos is not as expected
    if (is(TokenKind::LPAREN) &&
        (this->isBinOp() ||
         (lastTypeAttribPos + 1 == i && lastCastOpPos + 2 != i))) {
      // assert(false && "Function call");
      closedPar += 1;
      auto prevCurrToken = this->currentTokenKind();
      auto args = this->expression(true);
      closedPar += 1;

      auto opType = OpType::OP_BINARY;
      auto precTableType = this->m_PrecTable[opType];
      if (precTableType.count(prevCurrToken) == 0)
        assert(false && "Operator Prec: critical unreacheable!");

      auto precInfo = precTableType[prevCurrToken];

      bool isOutOfSize = false;
      auto nextToken = this->nextTokenInfo(isOutOfSize).getTokenKind();
      if (isOutOfSize)
        assert(
            false &&
            "The stream is done but left parenthesis '(' did not closed ')'");

      bool isFirst = i == 0;
      bool isLast =
          (nextToken == TokenKind::RPAREN || nextToken == TokenKind::SEMICOLON);
      bool hasTypeAttrib = false;

      // We're consuming function but we want to be sure it has a type attrib or
      // not. This makes the code a little bit messy but no other choicce.
      // [i - 1] since array indexes starts by 0
      // stride is increased 'cause of <TYPE> attrib
      if (lastTypeAttribPos + 1 == i) {
        hasTypeAttrib = true;
        stride += 1;
      }

      stride += 1;
      opBucket.push_back(PrecedenceEntry(prevCurrToken, opType,
                                         precInfo, stride, i,
                                         isFirst, isLast, hasTypeAttrib));
      exprBucket.push_back(std::move(args));
    } else if (is(TokenKind::LPAREN) &&
               this->isUnaryOp()) {  // this is for functional casts and
                                     // expression reducing (recursively)
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

      exprBucket.push_back(std::move(subExpr));
    } else if (is(TokenKind::LT) && this->isUnaryOp()) {  // <TYPE>
      // TODO: Don't let accept the any nodes, it's okay with only TypeAST
      //  advance '<'
      // this->advance();
      lastTypeAttribPos = i;
      auto typeAst = this->expression(
          true);  // this->advanceType();  // this->expression(true);
      exprBucket.push_back(std::move(typeAst));
    } else if (is(TokenKind::LT) && this->isBinOp()) {
      // This is for binary '<'. But we have to be sure it's
      // a function type attrib or not
      if (exprBucket[i - 1].get()->getExprKind() == ExprKind::SymbolExpr) {
        lastTypeAttribPos = i;
        auto typeAst = this->expression(true);
        exprBucket.push_back(std::move(typeAst));
      } else {
        // well we're sure this is not binary op for the type attrib
        // so go to jump_operator and keep going from there
        if(isOperator(this->currentTokenInfo()))
          goto jump_operator;
      }
    } else if (isOperator(this->currentTokenInfo())) {
    jump_operator:
      PrecedenceInfo precInfo;
      PrecedenceInfoTable precTableType;
      OpType opType = OpType::OP_UNARY;

      // stride checking perfomed 'cause if it's first operator then we should
      // not increase
      if (this->isUnaryOp()) {
        opType = OpType::OP_UNARY;
      } else if (this->isBinOp()) {
        opType = OpType::OP_BINARY;
        stride += 1;  // i != 0 ? 1 : 0;
      } else if (this->isCastOp()) {
        opType = OpType::OP_CAST;
        lastCastOpPos = i;
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


      bool isOutOfSize = false;
      auto nextToken = this->nextTokenInfo(isOutOfSize).getTokenKind();
      if (isOutOfSize)
        assert(
            false &&
            "The stream is done but left paranthesis '(' did not closed ')'");

      bool isFirst = i == 0;
      bool isLast =
          (nextToken == TokenKind::RPAREN || nextToken == TokenKind::SEMICOLON);
      bool hasTypeAttrib = (nextToken == TokenKind::LT);

      // well <type> is not orphon so we should not think that
      // it'll be reduced by cast operators which has high precedence
      // stride += hasTypeAttrib ? 1 : 0;

      opBucket.emplace_back(PrecedenceEntry(this->currentTokenKind(), opType,
                                         precInfo, stride, i,
                                         isFirst, isLast, hasTypeAttrib));
      this->advance();
    } else {
      // and perform	parsing the expression by recursive-descent way.
      auto node = this->advanceConstantOrLiteral();
      exprBucket.push_back(std::move(node));
    }

    i += 1;
  }

  // sort the operators by preceding level (from high to low)
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
    return std::move(expr);
  } else if (is(TokenKind::RPAREN)) {
    this->advance();

    // if there's only one atom exist in the ExprBucket
    // pop it to return quickly.
    if (opBucket.empty()) {
      auto atom = std::move(exprBucket.front());
      exprBucket.pop_front();
      return std::move(atom);
    } else {
      auto expr = this->reduceExpression(exprBucket, opBucket);
      return std::move(expr);
    }
  } else if (is(TokenKind::GT)) {
    // must be only 1 ast node as TypeAST
    if ((exprBucket.size() > 1 || exprBucket.empty()))
      std::cerr << "It must be only type. Invalid type attribute.\n";

    // advance >
    this->advance();

    // we inside the expression only for <TYPE> so we done and
    // have to return the atom
    auto atom = std::move(exprBucket.front());
    exprBucket.pop_front();

    return std::move(atom);
  } else {
    assert(false && "Operator Prec: unreacheable 2!");
  }

  // return this->advanceConstantOrLiteral();
}

ASTNode CStarParser::reduceExpression(std::deque<ASTNode>& exprBucket,
                                      OpPrecBucket& opPrecBucket) {
  auto popAtom = [&](size_t pos) -> ASTNode {
    auto atom = std::move(exprBucket[pos]);
    exprBucket.erase(exprBucket.begin() + pos);
    return std::move(atom);
  };

  auto insertNode = [&](size_t pos, ASTNode node) {
    if (exprBucket.empty())
      exprBucket.push_back(std::move(node));
    else
      exprBucket.insert(exprBucket.begin() + pos, std::move(node));
  };

  int i = 0;
  for (auto& op : opPrecBucket) {
    if (op.entryOpType() == OpType::OP_UNARY) {
      UnaryOpKind unaryOpKind = UnaryOpKind::U_SIZEOF;

      switch (op.entryTokenKind()) {
        case TokenKind::SIZEOF:
          unaryOpKind = UnaryOpKind::U_SIZEOF;
          break;
        case TokenKind::TYPEOF:
          unaryOpKind = UnaryOpKind::U_TYPEOF;
          break;
        case TokenKind::MOVE:
          unaryOpKind = UnaryOpKind::U_MOVE;
          break;
        default: {
          std::cout << "Iteration : " << i << std::endl;
          std::cout << "Op Count: " << opPrecBucket.size() << std::endl;
          for(auto &op_d : opPrecBucket) {
            op_d.print();
            std::cout << " ---------------- \n";
          }
          assert(false && "Unreacheable!");
        }
      }
      size_t pos = op.entryStride();
      auto expr = std::move(popAtom(pos));
      auto node = std::make_unique<UnaryOpAST>(std::move(expr), unaryOpKind);

      insertNode(pos, std::move(node));
    } else if (op.entryOpType() == OpType::OP_BINARY) {
      BinOpKind binOpKind = BinOpKind::B_ADD;

      // Debuggin purpose
      char opCharacter = this->m_Lexer.tokenAsStr(op.entryTokenKind())[0];
     // std::cout << opCharacter << std::endl;

      bool funcCall = false;

      // select the op kind
      switch (op.entryTokenKind()) {
        case LPAREN:
          funcCall = true;
          break;
        case COMMA:
          binOpKind = BinOpKind::B_COMM;
          break;
        case PLUS:
          binOpKind = BinOpKind::B_ADD;
          break;
        case MINUS:
          binOpKind = BinOpKind::B_SUB;
          break;
        case STAR:
          binOpKind = BinOpKind::B_MUL;
          break;
        case DIV:
          binOpKind = BinOpKind::B_DIV;
          break;
        case MOD:
          binOpKind = BinOpKind::B_MOD;
          break;
        case AND:
          binOpKind = BinOpKind::B_AND;
          break;
          // TODO: Add other binary ops
        default: {
          std::cout << "Iteration : " << i << std::endl;
          std::cout << "Op Count: " << opPrecBucket.size() << std::endl;
          for(auto &op_d : opPrecBucket) {
            op_d.print();
            std::cout << " ---------------- \n";
          }
          assert(false && "Unreacheable");
        }
      }
      size_t pos = op.entryStride();
      // pos - 1 since it's binary op
      // Popping the element at the same loc [0,1,2] -> pop(1) -> [0,2] ->
      // pop(1) -> [0]
      if (!funcCall) {
        auto lhs = std::move(popAtom(pos - 1));
        auto rhs = std::move(popAtom(pos - 1));

        for (auto& walkOp : opPrecBucket) {
          if (walkOp.entryStride() > pos) walkOp.decreaseStride();
        }

        auto node = std::make_unique<AdditionNode>(
            std::move(lhs), std::move(rhs), binOpKind, opCharacter);
        insertNode(pos - 1, std::move(node));
      } else {
        // Maybe it has type attrib
        if (op.entryHasTypeAttrib()) {
          auto lhs = std::move(popAtom(pos - 2));
          auto type = std::move(popAtom(pos - 2));
          auto rhs = std::move(popAtom(pos - 2));

          for (auto& walkOp : opPrecBucket) {
            if (walkOp.entryStride() > pos) {
              walkOp.decreaseStride();
              walkOp.decreaseStride();
            }
          }
          // lhs -> must be symbol name -> func name
          // if(!is(static_cast<>lhs.get()))
          auto node = std::make_unique<FuncCallAST>(
              std::move(lhs), std::move(type), std::move(rhs));
          insertNode(pos - 2, std::move(node));
        } else {
          auto lhs = std::move(popAtom(pos - 1));
          auto rhs = std::move(popAtom(pos - 1));

          for (auto& walkOp : opPrecBucket) {
            if (walkOp.entryStride() > pos) {
              walkOp.decreaseStride();
            }
          }
          // lhs -> must be symbol name -> func name
          // if(!is(static_cast<>lhs.get()))
          auto node = std::make_unique<FuncCallAST>(std::move(lhs), nullptr,
                                                    std::move(rhs));
          insertNode(pos - 1, std::move(node));
        }
      }

    } else if (op.entryOpType() == OpType::OP_CAST) {
      CastOpKind castOpKind = CastOpKind::C_CAST;

      switch (op.entryTokenKind()) {
        case UNSAFE_CAST:
          castOpKind = CastOpKind::C_UNSAFE_CAST;
          break;
        case CAST:
          castOpKind = CastOpKind::C_CAST;
          break;
        case AS:
          break;
        default:
          // unreacheable
          assert(false && "Unreacheable");
      }
      // sizeof type + 10 + unsafe_cast<int>(10 - 30 + 40);
      // atom => [type, 10, int, 10, 30, 40 ]
      // op => [ (sizeof), (+,1), (+,2), (unsafe_cast,2)

      if (op.entryHasTypeAttrib()) {
        size_t pos = op.entryStride();
        auto type = std::move(popAtom(pos));
        auto expr = std::move(popAtom(pos));

        auto node = std::make_unique<CastNode>(std::move(expr), std::move(type),
                                               castOpKind, true);
        insertNode(pos, std::move(node));
      } else {
        size_t pos = op.entryStride();
        auto expr = std::move(popAtom(pos));

        auto node = std::make_unique<CastNode>(std::move(expr), nullptr,
                                               castOpKind, false);
        insertNode(pos, std::move(node));
      }
    }
    else {
      std::cout << "Iteration : " << i << std::endl;
      std::cout << "Op Count: " << opPrecBucket.size() << std::endl;
      for(auto &op_d : opPrecBucket) {
        op_d.print();
        std::cout << " ---------------- \n";
      }
      assert(false && "Unreacheable!");
    }
    i++;
  }

  if (exprBucket.empty() || exprBucket.size() > 1) {
    assert(false && "PANIC");
  }

  return std::move(exprBucket[0]);
}

ASTNode CStarParser::advanceConstantOrLiteral() {
  // this is obviously a scalar or literal or others(matrix and vec?)
  if (is(TokenKind::SCALARD) || is(TokenKind::SCALARI) ||
      is(TokenKind::LITERAL)) {
    bool isIntegral = !is(TokenKind::LITERAL);
    bool isFloat = is(TokenKind::SCALARD);

    auto value = this->currentTokenStr();
    this->advance();
    return std::make_unique<ScalarAST>(value, isIntegral, isFloat);
  }

  return std::move(this->advanceRef());
}

ASTNode CStarParser::advanceRef() {
  //& ref
  if (is(TokenKind::AND) || is(TokenKind::REF)) {
  }

  return std::move(this->advanceType());  // this->advanceSymbol();
}

// Every IDENT will be evaulated here.
// Our Symbol are might be DEFINED.
// So they might have POINTER LEVEL(s)
ASTNode CStarParser::advanceSymbol() {
  if (is(TokenKind::IDENT)) {
    auto symbolName = this->currentTokenStr();

    this->advance();

    auto symbolNode = std::make_unique<SymbolAST>(symbolName);
    bool transitionFlag = false;
    bool isUniquePtr = false;
    size_t indirectionLevel = 0;

    // This is symbol to type transition (Actually this can be done while
    // semantically analysis this node but we make things easier or maybe not...
    // Not sure really)
    // * | ^
    while (is(TokenKind::STAR) || is(TokenKind::XOR)) {
      isUniquePtr = this->currentTokenKind() == TokenKind::XOR;
      transitionFlag = true;

      indirectionLevel = advancePointerType(isUniquePtr);
      std::cout << "Symbol to Type Transition Indirection Level: "
                << indirectionLevel << "\n";
    }

    if (transitionFlag) {
      return std::make_unique<TypeAST>(Type::T_DEFINED, std::move(symbolNode),
                                       isUniquePtr, true, indirectionLevel);
    } else {
      return std::move(symbolNode);
    }
  }

  return std::move(this->advanceIndirect());
}

ASTNode CStarParser::advanceType() {
  if (this->isType(this->currentTokenInfo())) {
    TokenInfo prevTokenInfo = this->currentTokenInfo();
    size_t indirectionLevel = 0;
    bool isUniquePtr = false;

    this->advance();

    //* | ^
    while (is(TokenKind::STAR) || is(TokenKind::XOR)) {
      isUniquePtr = this->currentTokenKind() == TokenKind::XOR;

      indirectionLevel = advancePointerType(isUniquePtr);
      // std::cout << "Type Indirection level: " << indirectionLevel << "\n";
    }

    ASTNode typeAst = std::make_unique<TypeAST>(
        typeOf(prevTokenInfo), nullptr, isUniquePtr, true, indirectionLevel);

    // expected >
    // expected({TokenKind::GT, TokenKind::RPAREN});

    return std::move(typeAst);
  }

  return std::move(this->advanceSymbol());
}

ASTNode CStarParser::advanceIndirect() {
  //* ^ deref
  if (is(TokenKind::STAR) || is(TokenKind::XOR) || is(TokenKind::DEREF)) {
  }
}

ASTNode CStarParser::advanceFunctionCall() {}

ASTNode CStarParser::advanceArraySubscript() {}
