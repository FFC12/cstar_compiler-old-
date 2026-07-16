#include <algorithm>
#include <parser/parser.hpp>

static bool StartsUnaryExpressionAfter(TokenKind kind) {
  switch (kind) {
    case TokenKind::LSQPAR:
      return true;
    default:
      return false;
  }
}

// if there is an operator before it (i rely on this and maybe not good idea?)
// or if it's the first operator in the expression
// then [probably] it's an unary operator
bool CStarParser::isUnaryOp() {
  if ((isOperator(this->prevSignificantTokenInfo()) ||
       StartsUnaryExpressionAfter(this->prevSignificantTokenKind())) &&
      !this->isCastOp()) {
    return true;
  } else {
    return false;
  }
}

// if it's not a unary or cast operator then it is binary operator
bool CStarParser::isBinOp() {
  bool a = this->isUnaryOp();
  bool b = this->isCastOp();

  bool z = !this->isUnaryOp() && !this->isCastOp();
  return z;
}

// check if it's cast operator
bool CStarParser::isCastOp() {
  return this->isCastableOperator(this->currentTokenInfo());
}

static bool TypeFlag = false;

static bool IsIgnorableExprToken(TokenKind kind) {
  return kind == TokenKind::COMMENT || kind == TokenKind::LINEFEED;
}

static bool CanContinueExpressionAcrossLine(TokenKind previous) {
  return previous == TokenKind::EQUAL || previous == TokenKind::RET ||
         previous == TokenKind::COMMA || previous == TokenKind::LPAREN ||
         previous == TokenKind::LSQPAR || previous == TokenKind::COLON ||
         previous == TokenKind::QMARK || previous == TokenKind::TYPEINF ||
         previous == TokenKind::PLUS || previous == TokenKind::MINUS ||
         previous == TokenKind::STAR || previous == TokenKind::DIV ||
         previous == TokenKind::MOD || previous == TokenKind::AND ||
         previous == TokenKind::LAND || previous == TokenKind::OR ||
         previous == TokenKind::LOR || previous == TokenKind::XOR ||
         previous == TokenKind::LT || previous == TokenKind::LTEQ ||
         previous == TokenKind::GT || previous == TokenKind::GTEQ ||
         previous == TokenKind::EQUALEQUAL ||
         previous == TokenKind::NOTEQUAL || previous == TokenKind::LSHIFT ||
         previous == TokenKind::RSHIFT || previous == TokenKind::ARROW ||
         previous == TokenKind::DOT || previous == TokenKind::COLONCOLON;
}

// parsing expressions
// opFor means the subexpr will be
// parsing of which kind of operator:
// 0 - None
// 1 - ( )
// 2 - ? :
// 3 - [ ]

