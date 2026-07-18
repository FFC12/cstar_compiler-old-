#include <parser/parser.hpp>
#include <ast/option_stmt.hpp>

// if - elif - else

void CStarParser::advanceIfStmt(std::vector<ASTNode> &scope) {
  size_t begin = currentTokenInfo().getTokenPositionInfo().begin;
  size_t line = currentTokenInfo().getTokenPositionInfo().line;

  // advance if
  this->advance();

  expected(TokenKind::LPAREN);

  auto cond = this->expression(true, 1);

  expectBlockStart();

  std::vector<ASTNode> condBody{};

  this->advanceScope(condBody);
  skipTopLevelTrivia();

  ConditionBlock conditionBlock{}, elseIfsBlock{};
  Scope elseScope{};

  conditionBlock.insert(
      {"if", std::pair<ASTNode, Scope>(std::move(cond), std::move(condBody))});

  bool hasElif = false;
  while (is(TokenKind::ELIF)) {
    this->advance();

    hasElif = true;

    cond = this->expression(true, 1);

    expectBlockStart();

    std::vector<ASTNode> elseIfBody{};
    this->advanceScope(elseIfBody);
    skipTopLevelTrivia();
    elseIfsBlock.insert({"elif", std::pair<ASTNode, Scope>(
                                     std::move(cond), std::move(elseIfBody))});
  }

  bool hasElse = false;
  while (is(TokenKind::ELSE)) {
    this->advance();

    hasElse = true;

    expectBlockStart();

    std::vector<ASTNode> elseBody{};
    this->advanceScope(elseBody);
    skipTopLevelTrivia();
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

void CStarParser::advanceOptionStmt(std::vector<ASTNode> &scope) {
  auto posInfo = currentTokenInfo().getTokenPositionInfo();
  const size_t begin = posInfo.begin;
  const size_t line = posInfo.line;

  this->advance();
  expected(TokenKind::LPAREN);
  auto value = this->expression(true, 1);

  expectBlockStart();
  this->advance();

  std::vector<OptionCase> cases;
  while (!is(TokenKind::RBRACK) && !is(TokenKind::_EOF)) {
    while (is(TokenKind::LINEFEED) || is(TokenKind::COMMENT) ||
           is(TokenKind::COMMA)) {
      this->advance();
    }
    if (is(TokenKind::RBRACK)) {
      break;
    }

    auto casePosInfo = currentTokenInfo().getTokenPositionInfo();
    SemanticLoc caseLoc(casePosInfo.begin, casePosInfo.end, casePosInfo.line);
    ASTNode pattern = nullptr;
    bool isDefault = false;

    if (is(TokenKind::IDENT) && currentTokenStr() == "_") {
      isDefault = true;
      this->advance();
    } else if (is(TokenKind::IDENT)) {
      const size_t patternBegin = currentTokenInfo().getTokenPositionInfo().begin;
      const size_t patternLine = currentTokenInfo().getTokenPositionInfo().line;
      auto enumSymbol = this->advanceSymbol();
      expected(TokenKind::DOT);
      this->advance();
      expected(TokenKind::IDENT);
      auto memberSymbol = this->advanceSymbol();
      std::string dot = ".";
      auto patternEnd = prevTokenInfo().getTokenPositionInfo().end;
      SemanticLoc patternLoc(patternBegin, patternEnd, patternLine);
      pattern = std::make_unique<BinaryOpAST>(
          std::move(enumSymbol), std::move(memberSymbol), nullptr, B_DOT, dot,
          patternLoc);
    } else {
      ParserError("option pattern must be an enum member or '_'",
                  currentTokenInfo());
    }

    expected(TokenKind::COLON);
    this->advance();
    expectBlockStart();

    Scope caseScope;
    this->advanceScope(caseScope);
    cases.emplace_back(std::move(pattern), std::move(caseScope), isDefault,
                       caseLoc);

    while (is(TokenKind::LINEFEED) || is(TokenKind::COMMENT)) {
      this->advance();
    }
    if (is(TokenKind::COMMA)) {
      this->advance();
    }
  }

  expected(TokenKind::RBRACK);
  posInfo = currentTokenInfo().getTokenPositionInfo();
  SemanticLoc semLoc(begin, posInfo.end, line);
  this->advance();

  scope.emplace_back(
      std::make_unique<OptionStmtAST>(std::move(value), std::move(cases),
                                      semLoc));
}
