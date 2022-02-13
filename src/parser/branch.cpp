#include <parser/parser.hpp>

// if - elif - else

void CStarParser::advanceIfStmt(std::vector<ASTNode> &scope) {
  size_t begin = currentTokenInfo().getTokenPositionInfo().begin;
  size_t line = currentTokenInfo().getTokenPositionInfo().line;

  // advance if
  this->advance();

  expected(TokenKind::LPAREN);

  auto cond = this->expression(true, 1);

  expected(TokenKind::LBRACK);

  std::vector<ASTNode> condBody{};

  this->advanceScope(condBody);

  ConditionBlock conditionBlock{}, elseIfsBlock{};
  Scope elseScope{};

  conditionBlock[std::move(cond)] = std::move(condBody);

  bool hasElif = false;
  while (is(TokenKind::ELIF)) {
    this->advance();

    hasElif = true;

    cond = this->expression(true, 1);

    expected(TokenKind::LBRACK);

    std::vector<ASTNode> elseIfBody{};
    this->advanceScope(elseIfBody);
    elseIfsBlock[std::move(cond)] = std::move(elseIfBody);
  }

  bool hasElse = false;
  while (is(TokenKind::ELSE)) {
    this->advance();

    hasElse = true;

    expected(TokenKind::LBRACK);

    std::vector<ASTNode> elseBody{};
    this->advanceScope(elseBody);
    elseScope = std::move(elseBody);
  }

  ASTNode ifStmt = nullptr;

  size_t end = currentTokenInfo().getTokenPositionInfo().end;
  SemanticLoc semLoc = SemanticLoc(begin, end, line);

  if (hasElif && hasElse) {
    ifStmt = std::make_unique<IfStmtAST>(std::move(conditionBlock),
                                         std::move(elseIfsBlock),
                                         std::move(elseScope), semLoc);
  } else if (hasElif && !hasElse) {
    ifStmt = std::make_unique<IfStmtAST>(std::move(conditionBlock),
                                         std::move(elseIfsBlock), semLoc);
  } else if (!hasElif && hasElse) {
    ifStmt = std::make_unique<IfStmtAST>(std::move(conditionBlock),
                                         std::move(elseScope), semLoc);
  } else {
    ifStmt = std::make_unique<IfStmtAST>(std::move(conditionBlock), semLoc);
  }

  scope.emplace_back(std::move(ifStmt));
}
