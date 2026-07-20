#include <parser/parser_private.hpp>

using namespace cstar::parser_private;

ASTNode CStarParser::expression(bool isSubExpr, int opFor, bool isRet,
                                bool typeFlag, bool isAssignment) {
  m_LastExpressionEndedByParen = false;

  // advance EQUAL or last expr before it came here if subexpr
  this->advance();

  if (!isSubExpr) {
    if (is(TokenKind::SEMICOLON)) {
      ParserHint("You must initialize the variable before ended up with ';'",
                 prevOfPrevTokenInfo());
      ParserError("Expression expected before ", currentTokenInfo());
    }
  }

  // std::unordered_map<size_t, ASTNode> exprBucket;
  std::deque<ASTNode> exprBucket;

  OpPrecBucket opBucket;

  size_t closedPar = isSubExpr && opFor == 1 ? 1 : 0;
  size_t closedTernary = isSubExpr && opFor == 2 ? 1 : 0;
  size_t closedSPar = isSubExpr && opFor == 3 ? 1 : 0;
  size_t lastTypeAttribPos = -1;
  size_t lastCastOpPos = -1;
  size_t i = 0;
  size_t stride = 0;

  bool castOpFlag = false;
  bool typeOpFlag = isSubExpr ? typeFlag : false;
  TypeFlag = typeOpFlag;

  // this is for offset from beginning of parenthesis
  std::deque<size_t> parenthesesPos;
  std::deque<size_t> ternaryPos;
  std::deque<size_t> sparenthesesPos;

  auto nextSignificantTokenInfo = [&](bool &outOfSize,
                                      size_t offset = 1) -> TokenInfo {
    size_t index = m_TokenIndex + offset;
    while (index < m_TokenStream.size() &&
           IsIgnorableExprToken(m_TokenStream[index].getTokenKind())) {
      index += 1;
    }

    if (index < m_TokenStream.size()) {
      outOfSize = false;
      return m_TokenStream[index];
    }

    outOfSize = true;
    return {};
  };

  auto previousSignificantTokenKind = [&]() -> TokenKind {
    if (m_TokenIndex == 0) {
      return TokenKind::UNKNOWN;
    }

    size_t index = m_TokenIndex - 1;
    while (index > 0 &&
           IsIgnorableExprToken(m_TokenStream[index].getTokenKind())) {
      index -= 1;
    }

    return m_TokenStream[index].getTokenKind();
  };

  auto isExpressionOperandStart = [&](const TokenInfo &token) {
    const auto kind = token.getTokenKind();
    return kind == TokenKind::IDENT || kind == TokenKind::SELF ||
           kind == TokenKind::STATE ||
           kind == TokenKind::NEW || kind == TokenKind::SHARED ||
           kind == TokenKind::SCALARD ||
           kind == TokenKind::SCALARI || kind == TokenKind::LITERAL ||
           kind == TokenKind::TRUE || kind == TokenKind::FALSE ||
           kind == TokenKind::NIL ||
           isType(token);
  };

  while (true) {
    if (is(TokenKind::COMMENT)) {
      this->advance();
      continue;
    }

    if (is(TokenKind::LINEFEED) &&
        (isSubExpr ||
         CanContinueExpressionAcrossLine(previousSignificantTokenKind()))) {
      this->advance();
      continue;
    }

    if (is(TokenKind::DYNAMIC)) {
      auto node = advanceDynamicTraitEraseExpression();
      exprBucket.push_back(std::move(node));
      i += 1;
      continue;
    }

    if (is(TokenKind::IDENT) && currentTokenStr() == "copy") {
      auto begin = currentTokenInfo().getTokenPositionInfo().begin;
      auto line = currentTokenInfo().getTokenPositionInfo().line;
      this->advance();
      while (is(TokenKind::COMMENT) || is(TokenKind::LINEFEED)) {
        this->advance();
      }
      if (!is(TokenKind::IDENT) && !is(TokenKind::STATE) &&
          !is(TokenKind::SELF)) {
        ParserError("`copy` expects a value operand", currentTokenInfo());
      }
      auto operand = this->advanceSymbol();
      auto end = prevTokenInfo().getTokenPositionInfo().end;
      auto semLoc = SemanticLoc(begin, end, line);
      auto node = std::make_unique<UnaryOpAST>(
          std::move(operand), UnaryOpKind::U_COPY, UnaryNotationSign::S_POS,
          semLoc);
      exprBucket.push_back(std::move(node));
      i += 1;
      continue;
    }

    if (is(TokenKind::IDENT) && currentTokenStr() == "span") {
      auto node = advanceSpanExpression();
      exprBucket.push_back(std::move(node));
      i += 1;
      continue;
    }

    if (is(TokenKind::NEW) || is(TokenKind::SHARED)) {
      auto node = advanceNewExpression(is(TokenKind::SHARED));
      exprBucket.push_back(std::move(node));
      i += 1;
      continue;
    }

    if (is(TokenKind::AWAIT)) {
      ParserHint(
          "`await` belongs to the C* async/task proposal. Await points are "
          "thread-boundary ownership checkpoints and require the future "
          "Send/Sync-capability model.",
          currentTokenInfo());
      ParserError(
          "`await` is part of the C* proposal, but async/task lowering is not "
          "implemented yet.",
          currentTokenInfo());

      while (!is(TokenKind::SEMICOLON) && !is(TokenKind::RPAREN) &&
             !is(TokenKind::_EOF)) {
        this->advance();
      }
      return nullptr;
    }

    // For subscripts
    if (is(COLON)) {
      bool outOfSize = false;
      auto nextToken = nextTokenInfo(outOfSize);

      if (outOfSize) {
        ParserError("Unexpected token end of file 'EOF'.", currentTokenInfo());
      } else {
        // That means we're indexing array
        if (closedSPar % 2 == 1) {
          // Check the condition if matched for the
          // manual preceding of ? and : (as binop)
          if ((isSubExpr && closedTernary % 2 == 0) ||
              (!isSubExpr && closedTernary % 2 == 1)) {
            goto jump_operator;
          } else
            goto jump_ternary;
        }
      }
    }

    // well RPAREN checking is a little bit confusing since LPAREN is not
    // included to the expression itself as seen in this example: if( [expr )]
    if (is(TokenKind::SEMICOLON) || is(TokenKind::RPAREN) ||
        (is(TokenKind::COMMA) && !isSubExpr) || is(TokenKind::RSQPAR) ||
        (is(TokenKind::COMMA) && isSubExpr && opFor == 1) ||
        is(TokenKind::COLON) || (is(TokenKind::RANGE) && isSubExpr && opFor == 4) ||
        (is(TokenKind::GT) && typeOpFlag) ||
        is(TokenKind::_EOF) || is(TokenKind::LINEFEED)) {
    jump_ternary:
      // well we're out of token and not consumed semicolon
      // or ) so probably missing semicolon or )
      if ((is(TokenKind::_EOF) || is(TokenKind::LINEFEED)) &&
          !is(TokenKind::RPAREN) && !is(TokenKind::SEMICOLON)) {
        ParserError(
            "Unexpected token end of line '\\n'. Probably you missed the ';' "
            "or ')' at the end of line '\\n'.",
            prevTokenInfo());
      }

      if (is(TokenKind::COMMA) && !isSubExpr &&
              (this->prevTokenKind() == TokenKind::EQUAL ||
               (this->prevTokenKind() == TokenKind::RET && isRet)) ||
          (isShortcutOp(this->prevTokenInfo()) && isAssignment)) {
        ParserError(std::string("Unexpected token '") +
                        tokenToStr(currentTokenKind()) + "'",
                    currentTokenInfo());
      }

      if (is(TokenKind::RPAREN)) closedPar += 1;
      if (is(TokenKind::COLON)) closedTernary += 1;
      if (is(TokenKind::RSQPAR)) closedSPar += 1;
      break;
    }

    // this is for function call
    if (is(TokenKind::LPAREN) &&
        (this->isBinOp() ||
         (lastTypeAttribPos + 1 == i && lastCastOpPos + 2 != i) &&
             !castOpFlag && typeOpFlag)) {
      // assert(false && "Function call");
      if ((lastTypeAttribPos + 1 == i && lastCastOpPos + 2 != i &&
           !castOpFlag && typeOpFlag))
        typeOpFlag = false;

      if ((this->prevTokenKind() == TokenKind::EQUAL ||
           (this->prevTokenKind() == TokenKind::RET && isRet) ||
           (isShortcutOp(this->prevTokenInfo()) && isAssignment)))
        goto jump_unary_paranthesis;
      parenthesesPos.push_back(currentTokenInfo().getTokenPositionInfo().begin);

      closedPar += 1;
      auto prevCurrToken = this->currentTokenInfo();

      bool outOfSize = false;
      auto nextToken = this->nextTokenInfo(outOfSize).getTokenKind();
      if (outOfSize) {
      }

      auto args = this->advanceArgumentList();
      // empty parenthesis block like ()
      if (args == nullptr && nextToken != RPAREN)
        ParserError("Unexpected token '" +
                        std::string(tokenToStr(prevOfPrevTokenKind())) +
                        "'. You missed operator or name before parenthesis!",
                    prevOfPrevTokenInfo());

      closedPar += 1;

      auto opType = OpType::OP_BINARY;
      auto precTableType = this->m_PrecTable[opType];
      if (precTableType.count(prevCurrToken.getTokenKind()) == 0)
        assert(false && "Operator Prec: critical unreacheable!");

      auto precInfo = precTableType[prevCurrToken.getTokenKind()];

      bool isOutOfSize = false;
      nextToken = this->nextTokenInfo(isOutOfSize).getTokenKind();
      if (isOutOfSize)
        ParserError("')' mismatched parentheses.", currentTokenInfo());

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
      opBucket.push_back(PrecedenceEntry(prevCurrToken, opType, precInfo,
                                         stride, i, isFirst, isLast,
                                         hasTypeAttrib));
      exprBucket.push_back(std::move(args));
    } else if (is(TokenKind::LSQPAR) && this->isBinOp()) {
      // if (prevTokenKind() == TokenKind::EQUAL) goto jump_unary_paranthesis;
      sparenthesesPos.push_back(
          currentTokenInfo().getTokenPositionInfo().begin);

      closedSPar += 1;
      auto prevCurrToken = this->currentTokenInfo();

      bool outOfSize = false;
      auto nextToken = this->nextTokenInfo(outOfSize).getTokenKind();
      if (outOfSize) {
      }

      auto indexExpr = this->expression(true, 3);
      // empty parenthesis block like []

      if (indexExpr == nullptr) {
        if (nextToken == COLON && is(TokenKind::SEMICOLON)) {
          ParserHint(
              "This will be implemented as 'arr[:]' which'll equal to "
              "'arr[0:0]'",
              prevOfPrevTokenInfo());
          ParserError("Unexpected token '" +
                          std::string(tokenToStr(prevOfPrevTokenKind())) +
                          "'. You missed index(es) for subscript.",
                      prevOfPrevTokenInfo());
        } else if (nextToken != TokenKind::RSQPAR) {
          ParserError("Unexpected token '" +
                          std::string(tokenToStr(prevOfPrevTokenKind())) +
                          "'. You missed closed by ']' subscript operator.",
                      prevOfPrevTokenInfo());
        }

        // This is arr[] interpretering as arr[0]
        // TODO: Impelement arr[]
      }

      closedSPar += 1;

      auto opType = OpType::OP_BINARY;
      auto precTableType = this->m_PrecTable[opType];
      if (precTableType.count(prevCurrToken.getTokenKind()) == 0)
        assert(false && "Operator Prec: critical unreacheable!");

      auto precInfo = precTableType[prevCurrToken.getTokenKind()];

      bool isOutOfSize = false;
      nextToken = this->nextTokenInfo(isOutOfSize).getTokenKind();
      if (isOutOfSize)
        ParserError("']' mismatched subscript operator.", currentTokenInfo());

      bool isFirst = i == 0;
      bool isLast =
          (nextToken == TokenKind::RPAREN || nextToken == TokenKind::SEMICOLON);
      bool hasTypeAttrib = false;

      stride += 1;
      opBucket.push_back(PrecedenceEntry(prevCurrToken, opType, precInfo,
                                         stride, i, isFirst, isLast,
                                         hasTypeAttrib));
      exprBucket.push_back(std::move(indexExpr));
    } else if (is(TokenKind::QMARK) && this->isBinOp()) {
      // if (prevTokenKind() == TokenKind::EQUAL) goto jump_unary_paranthesis;
      ternaryPos.push_back(currentTokenInfo().getTokenPositionInfo().begin);

      closedTernary += 1;
      auto prevCurrToken = this->currentTokenInfo();

      bool outOfSize = false;
      auto nextToken = this->nextTokenInfo(outOfSize).getTokenKind();
      if (outOfSize) {
      }

      auto cond0 = this->expression(true, 2);
      // empty parenthesis block like ()
      if (cond0 == nullptr && nextToken != COLON)
        ParserError("Unexpected token '" +
                        std::string(tokenToStr(prevOfPrevTokenKind())) +
                        "'. You missed ':' part of ternary operator.",
                    prevOfPrevTokenInfo());

      closedTernary += 1;

      auto opType = OpType::OP_BINARY;
      auto precTableType = this->m_PrecTable[opType];
      if (precTableType.count(prevCurrToken.getTokenKind()) == 0)
        assert(false && "Operator Prec: critical unreacheable!");

      auto precInfo = precTableType[prevCurrToken.getTokenKind()];

      bool isOutOfSize = false;
      nextToken = this->nextTokenInfo(isOutOfSize).getTokenKind();
      if (isOutOfSize)
        ParserError("':' mismatched ternary operator.", currentTokenInfo());

      bool isFirst = i == 0;
      bool isLast =
          (nextToken == TokenKind::RPAREN || nextToken == TokenKind::SEMICOLON);
      bool hasTypeAttrib = false;

      stride += 2;
      opBucket.push_back(PrecedenceEntry(prevCurrToken, opType, precInfo,
                                         stride, i, isFirst, isLast,
                                         hasTypeAttrib));
      exprBucket.push_back(std::move(cond0));
    } else if (is(TokenKind::LPAREN) &&
               this->isUnaryOp()) {  // this is for functional casts and
                                     // expression reducing (recursively)
    jump_unary_paranthesis:
      // if (prevTokenKind() == TokenKind::COMMA)
      //   ParserError("Unexpected token '" +
      //                   std::string(tokenToStr(prevTokenKind())) + "'",
      //               prevTokenInfo());
      parenthesesPos.push_back(currentTokenInfo().getTokenPositionInfo().begin);

      closedPar += 1;
      auto subExpr = this->expression(true, 1);
      closedPar += 1;  // it's always returning back after handled that
                       // paranthesis '(' ')'.

      bool isOutOfSize = false;
      auto nextToken = this->nextTokenInfo(isOutOfSize).getTokenKind();
      if (isOutOfSize)
        ParserError("')' mismatched parentheses.", currentTokenInfo());

      exprBucket.push_back(std::move(subExpr));
    } else if (is(TokenKind::LT) && this->isUnaryOp()) {  // <TYPE>
      lastTypeAttribPos = i;
      typeOpFlag = true;
      if ((this->prevTokenKind() == TokenKind::EQUAL ||
           (this->prevTokenKind() == TokenKind::RET && isRet) ||
           (isShortcutOp(this->prevTokenInfo()) && isAssignment)) &&
          (!isCastableOperator(prevTokenInfo()) ||
           this->prevTokenKind() != IDENT))
        ParserError("Unexpected token '" +
                        std::string(tokenToStr(currentTokenKind())) +
                        "'. Type attribute must be used after functional cast "
                        "operators or function call (generic).",
                    currentTokenInfo());

      auto typeAst = this->expression(
          true, 0, false,
          typeOpFlag);  // this->advanceType();  // this->expression(true);
      exprBucket.push_back(std::move(typeAst));
    } else if (is(TokenKind::LT) && this->isBinOp()) {
      // This is for binary '<'. But we have to be sure it's
      // a function type attrib or not
      auto genericCandidateIndex = this->m_TokenIndex;
      auto genericCandidatePrevToken = this->m_PrevToken;
      this->advance();

      bool clearType = false;
      bool isIdent = false;
      if (isType(currentTokenInfo())) clearType = true;
      if (is(TokenKind::IDENT)) isIdent = true;

      auto previousTypeFlag = TypeFlag;
      TypeFlag = true;
      auto typeOrSymbol = clearType || isIdent ? this->advanceType() : nullptr;
      TypeFlag = previousTypeFlag;

      bool outOfSize = false;
      auto nextToken = nextTokenInfo(outOfSize).getTokenKind();
      if (outOfSize) ParserError("Unexpected token", currentTokenInfo());

      if ((currentTokenKind() == TokenKind::GT) &&
          (nextToken ==
           TokenKind::LPAREN)) {  //(exprBucket[i - 1].get()->getExprKind() ==
                                  // ExprKind::SymbolExpr) {
        lastTypeAttribPos = i;
        typeOpFlag = true;
        this->advance();
        // auto typeAst = this->expression(true, 0, false, typeOpFlag);
        exprBucket.push_back(std::move(typeOrSymbol));
      } else {
        // well we're sure this is not binary op for the type attrib
        // so go to jump_operator and keep going from there
        this->m_TokenIndex = genericCandidateIndex;
        this->m_CurrToken = this->m_TokenStream[this->m_TokenIndex];
        this->m_PrevToken = genericCandidatePrevToken;
        if (isOperator(this->currentTokenInfo())) goto jump_operator;
      }
    } else if (is(TokenKind::AS)) {
      auto prevCurrToken = this->currentTokenInfo();
      auto opType = OpType::OP_CAST;
      auto precTableType = this->m_PrecTable[opType];
      if (precTableType.count(prevCurrToken.getTokenKind()) == 0) {
        assert(false && "Operator Prec: critical unreacheable!");
      }

      auto precInfo = precTableType[prevCurrToken.getTokenKind()];
      this->advance();

      if (!isType(currentTokenInfo()) && !is(TokenKind::IDENT)) {
        ParserError("'as' cast expects a target type", currentTokenInfo());
      }

      auto typeAst = this->advanceType();
      exprBucket.push_back(std::move(typeAst));
      const size_t typeAtomIndex = exprBucket.size() - 1;
      opBucket.push_back(PrecedenceEntry(prevCurrToken, opType, precInfo,
                                         typeAtomIndex, i, false, false,
                                         true));
    } else if (isOperator(this->currentTokenInfo())) {
    jump_operator:
      PrecedenceInfo precInfo;
      PrecedenceInfoTable precTableType;
      OpType opType = OpType::OP_UNARY;
      std::string opCharacter = tokenToStr(this->currentTokenKind());
      bool prefixFlag = false;

      if (is(TokenKind::PLUSPLUS) || is(TokenKind::MINUSMINUS)) {
        // prefix flag will be used to set prec info lower value
        // and from ltr to rtl
        if (isUnaryOp()) prefixFlag = true;

        opType = OpType::OP_UNARY;
      } else {
        if (this->isUnaryOp()) {
          if (prevTokenInfo() == RSQPAR && closedSPar > 0 &&
              closedSPar % 2 == 0) {
            opType = OpType::OP_BINARY;
            stride += 1;
          } else {
            if (is(TokenKind::PLUS) || is(TokenKind::MINUS)) {
              if (prevTokenKind() == TokenKind::PLUSPLUS ||
                  prevTokenKind() == TokenKind::MINUSMINUS ||
                  prevTokenKind() == TokenKind::RSQPAR) {
                opType = OpType::OP_BINARY;
                stride += 1;
              } else {
                opType = OpType::OP_UNARY;
              }
            }
          }
        } else if (this->isBinOp()) {
          opType = OpType::OP_BINARY;
          stride += 1;  // i != 0 ? 1 : 0;
        } else if (this->isCastOp()) {
          opType = OpType::OP_CAST;
          lastCastOpPos = i;
          castOpFlag = true;
        } else {
          assert(false && "Operator Prec: unreachable!");
        }
      }

      // check the operator existence, it might be '0' and not found in the
      // hashmap. If does not exist and not a op then must be ended declaration
      // so give an error for expecting ';'
      precTableType = this->m_PrecTable[opType];
      if (precTableType.count(this->currentTokenKind()) == 0) {
        // Trying to use unary operator as binary operator.
        // This is invalid
        if (isOperator(currentTokenInfo())) {
          ParserError(
              "Unexpected token '" +
                  std::string(tokenToStr(currentTokenKind())) +
                  "' instead of ';' or ')'. Probably you're trying to use an "
                  "unary operator as binary operator which is not allowed!",
              currentTokenInfo());
        } else {
          ParserError("Unexpected token '" +
                          std::string(tokenToStr(currentTokenKind())) +
                          "' instead of ';' or ')'",
                      currentTokenInfo());
        }
        // assert(false && "Operator Prec: critical unreacheable!");
      }

      precInfo = precTableType[this->currentTokenKind()];

      // for ++p
      if (prefixFlag) {
        precInfo.setLtr(false);
        precInfo.setPrec(13);
      }

      // debuggin purpose

      bool isOutOfSize = false;
      auto nextTokenInfo = nextSignificantTokenInfo(isOutOfSize);
      auto nextToken = nextTokenInfo.getTokenKind();
      if (isOutOfSize)
        ParserError("')' mismatched parentheses.", currentTokenInfo());

      bool isFirst = i == 0;
      bool isLast =
          (nextToken == TokenKind::RPAREN || nextToken == TokenKind::SEMICOLON);
      bool hasTypeAttrib = (nextToken == TokenKind::LT);

      if (!isOperator(nextTokenInfo) &&
          !isExpressionOperandStart(nextTokenInfo)) {
        ParserError("Unexpected token '" + std::string(tokenToStr(nextToken)) +
                        "' after binary operator '" + opCharacter + "'",
                    nextTokenInfo);
      }

      opBucket.push_back(PrecedenceEntry(this->currentTokenInfo(), opType,
                                         precInfo, stride, i, isFirst, isLast,
                                         hasTypeAttrib));
      this->advance();
    } else {
      // Maybe inline comment put the between expressions
      // skip it and continue from where you were.
      if (this->currentTokenKind() == COMMENT) {
        this->advance();
        continue;
      } else if (this->currentTokenKind() == LINEFEED) {
        this->advance();
        continue;
      } else if (this->currentTokenKind() == IDENT ||
                 this->currentTokenKind() == SELF || is(SCALARI) ||
                 is(SCALARD) || is(LITERAL)) {
        bool outOfSize = false;
        auto nextTokenInfo = this->nextTokenInfo(outOfSize);
        if (outOfSize) {
          ParserError(
              "Unexpected token '" +
                  std::string(tokenToStr(nextTokenInfo.getTokenKind())) + "'",
              nextTokenInfo);
        }

        auto nextToken = nextTokenInfo.getTokenKind();
        if (nextToken == IDENT || nextToken == SELF || nextToken == SCALARI ||
            nextToken == SCALARD || nextToken == LITERAL) {
          auto val = currentTokenInfo().getTokenPositionInfo().begin;
          ParserHint(
              "You probably missed the binary operator between two operands",
              nextTokenInfo, val);
          ParserError(
              "Unexpected token '" + nextTokenInfo.getTokenAsStr() + "'",
              nextTokenInfo);
        }
      }

      // and perform	parsing the expression by recursive-descent way.
      auto node = this->advanceConstantOrLiteral();
      if (node == nullptr) {
        if (is(TokenKind::LSQPAR)) {
          ParserHint(
              "Double or more subscripts should not be expressed like in the C "
              "or C++. "
              "Use the "
              "':' for each index. (ex: arr[0:1:2] )",
              prevOfPrevTokenInfo());
        }
        ParserError(
            "Unexpected token '" + currentTokenInfo().getTokenAsStr() + "'",
            currentTokenInfo());
      }
      exprBucket.push_back(std::move(node));
    }

    i += 1;
  }

  // sort the operators by preceding level (from high to low)
  std::sort(opBucket.rbegin(), opBucket.rend());

  // decide: is it belongs to the initialization expression or statement
  // expression
  if (is(TokenKind::SEMICOLON) ||
      (is(TokenKind::COMMA) && !isSubExpr)) {  // initialization
    m_LastExpressionEndedByParen = false;
    if (closedTernary % 2 != 0) {
      // assert(false && ") mismatch");
      if (!ternaryPos.empty()) {
        auto val = ternaryPos.front();
        ParserError("':' mismatched ternary operator.", prevTokenInfo(), val);
      } else {
        ParserError("':' mismatched ternary operator.", prevTokenInfo());
      }
    }

    // let's be sure that all the open parenthesis are closed or not?
    if (closedPar % 2 != 0) {
      // assert(false && ") mismatch");
      if (!parenthesesPos.empty()) {
        auto val = parenthesesPos.front();
        ParserError("'(' mismatched parentheses.", prevTokenInfo(), val);
      } else {
        ParserError("')' mismatched parentheses.", prevTokenInfo());
      }
    }

    // build top-level AST here..
    auto expr = this->reduceExpression(exprBucket, opBucket);
    return std::move(expr);
  } else if (is(TokenKind::COMMA) && isSubExpr && opFor == 1) {
    m_LastExpressionEndedByParen = false;
    if (closedPar % 2 != 1) {
      ParserError("')' mismatched function argument expression.",
                  currentTokenInfo());
    }

    if (opBucket.empty() && exprBucket.size() == 1) {
      auto atom = std::move(exprBucket.front());
      exprBucket.pop_front();
      return std::move(atom);
    }

    return this->reduceExpression(exprBucket, opBucket);
  } else if (is(TokenKind::RPAREN)) {
    this->advance();
    m_LastExpressionEndedByParen = true;

    // this is for statements
    if (!parenthesesPos.empty()) {
      prevTokenInfo().getTokenPositionInfo().setBegin(parenthesesPos.front());
    }
    if (closedPar % 2 != 0) {
      // assert(false && ") mismatch");
      if (!parenthesesPos.empty()) {
        auto val = parenthesesPos.front();
        ParserError("'(' mismatched parentheses.", prevTokenInfo(), val);
      } else {
        ParserError("')' mismatched parentheses.", prevTokenInfo());
      }
    }

    // if there's only one atom exist in the ExprBucket
    // pop it for returning quickly.
    if (opBucket.empty() && exprBucket.size() == 1) {
      auto atom = std::move(exprBucket.front());
      exprBucket.pop_front();
      return std::move(atom);
    } else {
      if (!exprBucket.empty()) {
        auto expr = this->reduceExpression(exprBucket, opBucket);
        return std::move(expr);
      } else {
        return nullptr;
      }
      //{ return nullptr; }
    }
  } else if (is(TokenKind::GT)) {
    m_LastExpressionEndedByParen = false;
    // must be only 1 ast node as TypeAST
    if ((exprBucket.size() > 1 || exprBucket.empty()))
      ParserError("There must be only type. Invalid type attribute.\n",
                  currentTokenInfo());

    // advance >
    this->advance();

    // we're inside the expression only for <TYPE> so we done and
    // have to return the atom
    auto atom = std::move(exprBucket.front());
    exprBucket.pop_front();

    return std::move(atom);
  } else if (is(TokenKind::RSQPAR)) {
    m_LastExpressionEndedByParen = false;
    this->advance();

    // this is for statements
    if (!sparenthesesPos.empty()) {
      prevTokenInfo().getTokenPositionInfo().setBegin(sparenthesesPos.front());
    }
    if (closedSPar % 2 != 0) {
      // assert(false && ") mismatch");
      if (!sparenthesesPos.empty()) {
        auto val = sparenthesesPos.front();
        ParserError("']' mismatched subscript operator.", prevTokenInfo(), val);
      } else {
        ParserError("']' mismatched subscript operator.", prevTokenInfo());
      }
    }

    // if there's only one atom exist in the ExprBucket
    // pop it for returning quickly.
    if (opBucket.empty() && exprBucket.size() == 1) {
      auto atom = std::move(exprBucket.front());
      exprBucket.pop_front();
      return std::move(atom);
    } else {
      if (!exprBucket.empty()) {
        auto expr = this->reduceExpression(exprBucket, opBucket);
        return std::move(expr);
      } else {
        return nullptr;
      }
      //{ return nullptr; }
    }
  } else if (is(TokenKind::COLON)) {
    m_LastExpressionEndedByParen = false;
    this->advance();

    // this is for statements
    if (!ternaryPos.empty()) {
      prevTokenInfo().getTokenPositionInfo().setBegin(ternaryPos.front());
    }
    if (closedTernary % 2 != 0) {
      // assert(false && ") mismatch");
      if (!ternaryPos.empty()) {
        auto val = ternaryPos.front();
        ParserError("':' mismatched ternary operator.", prevTokenInfo(), val);
      } else {
        ParserError("':' mismatched ternary operator.", prevTokenInfo());
      }
    }

    // if there's only one atom exist in the ExprBucket
    // pop it for returning quickly.
    if (opBucket.empty() && exprBucket.size() == 1) {
      auto atom = std::move(exprBucket.front());
      exprBucket.pop_front();
      return std::move(atom);
    } else {
      if (!exprBucket.empty()) {
        auto expr = this->reduceExpression(exprBucket, opBucket);
        return std::move(expr);
      } else {
        return nullptr;
      }
      //{ return nullptr; }
    }
  } else if (is(TokenKind::RANGE)) {
    m_LastExpressionEndedByParen = false;
    if (opBucket.empty() && exprBucket.size() == 1) {
      auto atom = std::move(exprBucket.front());
      exprBucket.pop_front();
      return std::move(atom);
    }
    if (!exprBucket.empty()) {
      auto expr = this->reduceExpression(exprBucket, opBucket);
      return std::move(expr);
    }
    return nullptr;
  } else {
    assert(false && "Operator Prec: unreacheable 2!");
  }

  return nullptr;
}
