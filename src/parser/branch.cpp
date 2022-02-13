#include <parser/parser.hpp>

// if - elif - else

void CStarParser::advanceIfStmt(std::vector<ASTNode> &scope) {
  // advance if
  this->advance();

  expected(TokenKind::LPAREN);

  auto cond = this->expression(true, 1);

  expected(TokenKind::LBRACK);

  std::vector<ASTNode> ifBody;

  this->advanceScope(ifBody);

}
