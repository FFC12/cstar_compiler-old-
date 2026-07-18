#include <parser/parser_private.hpp>

using namespace cstar::parser_private;

ASTNode CStarParser::advanceArgumentList() {
  expected(TokenKind::LPAREN);

  bool outOfSize = false;
  auto nextToken = this->nextTokenInfo(outOfSize).getTokenKind();
  if (outOfSize) {
    ParserError("Incomplete function call expression", currentTokenInfo());
  }

  if (nextToken == TokenKind::RPAREN) {
    this->advance();
    this->advance();
    return nullptr;
  }

  ASTNode args = nullptr;
  TokenInfo pendingComma;
  bool hasPendingComma = false;

  while (true) {
    auto arg = this->expression(true, 1);
    if (arg == nullptr) {
      ParserError("Expected function argument expression", currentTokenInfo());
    }

    if (args == nullptr) {
      args = std::move(arg);
    } else {
      auto commaPos = pendingComma.getTokenPositionInfo();
      auto semLoc = SemanticLoc(commaPos.begin, commaPos.end, commaPos.line);
      std::string comma = ",";
      args = std::make_unique<BinaryOpAST>(
          std::move(args), std::move(arg), nullptr, BinOpKind::B_COMM, comma,
          semLoc);
    }

    if (m_LastExpressionEndedByParen) {
      break;
    }

    if (is(TokenKind::COMMA)) {
      pendingComma = currentTokenInfo();
      hasPendingComma = true;
      bool commaOutOfSize = false;
      auto tokenAfterComma =
          this->nextTokenInfo(commaOutOfSize).getTokenKind();
      if (commaOutOfSize || tokenAfterComma == TokenKind::RPAREN) {
        ParserError("Expected function argument after ','",
                    currentTokenInfo());
      }
      continue;
    }

    break;
  }

  (void)hasPendingComma;
  return args;
}
