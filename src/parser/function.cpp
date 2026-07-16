#include <parser/parser.hpp>

#include <algorithm>

static bool IsValueOperatorToken(TokenKind kind) {
  switch (kind) {
    case PLUS:
    case MINUS:
    case STAR:
    case DIV:
    case MOD:
    case EQUALEQUAL:
    case NOTEQUAL:
    case LT:
    case LTEQ:
    case GT:
    case GTEQ:
      return true;
    default:
      return false;
  }
}

