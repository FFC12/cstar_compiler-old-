#include <parser/parser_private.hpp>

using namespace cstar::parser_private;

static bool IsProtocolTrivia(TokenKind kind) {
  return kind == TokenKind::COMMENT || kind == TokenKind::LINEFEED;
}

static void SkipProtocolTrivia(CStarParser& parser) {
  // This file is compiled as a CStarParser member implementation unit, so the
  // helper stays intentionally tiny and parser methods do the real work.
}

void CStarParser::applyStateQualifiers(
    ASTNode& typeNode, const std::vector<std::string>& states) {
  if (states.empty() || typeNode == nullptr ||
      typeNode->getExprKind() != ExprKind::TypeExpr) {
    return;
  }

  auto* typeAst = static_cast<TypeAST*>(typeNode.get());
  typeAst->setStateQualifiers(states);
}

std::vector<std::string> CStarParser::collectStateQualifiersBeforeType() {
  std::vector<std::string> states;

  while (is(TokenKind::IDENT)) {
    bool outOfSize = false;
    const auto next = nextTokenInfo(outOfSize).getTokenKind();
    if (outOfSize || next != TokenKind::IDENT) {
      break;
    }

    bool secondOut = false;
    const auto afterNext = nextTokenInfo(secondOut, 2).getTokenKind();
    if (secondOut) {
      break;
    }

    if (afterNext != TokenKind::IDENT && afterNext != TokenKind::STAR &&
        afterNext != TokenKind::XOR && afterNext != TokenKind::AND &&
        afterNext != TokenKind::LSQPAR && afterNext != TokenKind::LBRACK) {
      break;
    }

    states.push_back(currentTokenStr());
    advance();
  }

  return states;
}

void CStarParser::parseProtocolDecl(DeclarationModifiers declarationModifiers,
                                    bool isDynamic) {
  auto start = currentTokenInfo().getTokenPositionInfo();
  expected(TokenKind::PROTOCOL);
  advance();
  skipTopLevelTrivia();

  expected(TokenKind::IDENT);
  auto protocolName = currentTokenStr();
  advance();
  skipTopLevelTrivia();

  expected(TokenKind::FOR);
  advance();
  skipTopLevelTrivia();

  expected(TokenKind::IDENT);
  auto targetTypeName = advanceDefinedTypeName();
  skipTopLevelTrivia();

  expected(TokenKind::LBRACK);
  advance();

  std::vector<std::string> states;
  std::string defaultState;
  std::vector<ProtocolTransitionASTInfo> transitions;
  std::vector<ProtocolForbiddenCallASTInfo> forbiddenCalls;
  std::vector<ProtocolTransitionASTInfo> scopeExitTransitions;

  auto skipBodyTrivia = [&]() {
    while (IsProtocolTrivia(currentTokenKind())) {
      advance();
    }
  };

  auto parseMethodName = [&]() -> std::string {
    expected(TokenKind::IDENT);
    auto methodName = currentTokenStr();
    advance();
    if (is(TokenKind::LPAREN)) {
      skipBalanced(TokenKind::LPAREN, TokenKind::RPAREN);
    }
    return methodName;
  };

  while (!is(TokenKind::RBRACK)) {
    skipBodyTrivia();
    if (is(TokenKind::RBRACK)) {
      break;
    }

    if (is(TokenKind::STATE)) {
      advance();
      skipBodyTrivia();
      while (!is(TokenKind::SEMICOLON) && !is(TokenKind::_EOF)) {
        skipBodyTrivia();
        expected(TokenKind::IDENT);
        states.push_back(currentTokenStr());
        advance();
        skipBodyTrivia();
        if (is(TokenKind::COMMA)) {
          advance();
        }
      }
      expected(TokenKind::SEMICOLON);
      advance();
      continue;
    }

    if (is(TokenKind::DEFAULT)) {
      advance();
      skipBodyTrivia();
      expected(TokenKind::IDENT);
      defaultState = currentTokenStr();
      advance();
      skipBodyTrivia();
      expected(TokenKind::SEMICOLON);
      advance();
      continue;
    }

    const bool isScopeExit = is(TokenKind::IDENT) && currentTokenStr() == "scope_exit";
    if (isScopeExit) {
      advance();
      skipBodyTrivia();
      expected(TokenKind::IDENT);
      auto fromState = currentTokenStr();
      advance();
      skipBodyTrivia();
      expected(TokenKind::ARROW);
      advance();
      skipBodyTrivia();
      expected(TokenKind::IDENT);
      auto toState = currentTokenStr();
      advance();
      skipBodyTrivia();
      expected(TokenKind::COLONCOLON);
      advance();
      skipBodyTrivia();
      auto methodName = parseMethodName();
      skipBodyTrivia();
      expected(TokenKind::SEMICOLON);
      advance();
      scopeExitTransitions.push_back({fromState, toState, methodName});
      continue;
    }

    if (is(TokenKind::IDENT)) {
      auto first = currentTokenStr();
      advance();
      skipBodyTrivia();
      if (is(TokenKind::ARROW)) {
        advance();
        skipBodyTrivia();
        expected(TokenKind::IDENT);
        auto toState = currentTokenStr();
        advance();
        skipBodyTrivia();
        expected(TokenKind::COLONCOLON);
        advance();
        skipBodyTrivia();
        auto methodName = parseMethodName();
        skipBodyTrivia();
        expected(TokenKind::SEMICOLON);
        advance();
        transitions.push_back({first, toState, methodName});
        continue;
      }

      if (is(TokenKind::LPAREN)) {
        skipBalanced(TokenKind::LPAREN, TokenKind::RPAREN);
        skipBodyTrivia();
        expected(TokenKind::COLONCOLON);
        advance();
        skipBodyTrivia();
        expected(TokenKind::NOT);
        advance();
        skipBodyTrivia();
        expected(TokenKind::IDENT);
        auto forbiddenState = currentTokenStr();
        advance();
        skipBodyTrivia();
        expected(TokenKind::SEMICOLON);
        advance();
        forbiddenCalls.push_back({first, forbiddenState});
        continue;
      }
    }

    ParserError("Unexpected token in protocol declaration", currentTokenInfo());
  }

  expected(TokenKind::RBRACK);
  auto end = currentTokenInfo().getTokenPositionInfo();
  advance();

  if (declarationModifiers.isStatic) {
    ParserError("protocol is static/provable by default; `static protocol` is "
                "not canonical C* syntax",
                currentTokenInfo());
  }

  SemanticLoc semLoc(start.begin, end.end, start.line);
  auto protocol = std::make_unique<ProtocolAST>(
      protocolName, targetTypeName, std::move(states), defaultState,
      std::move(transitions), std::move(forbiddenCalls),
      std::move(scopeExitTransitions), isDynamic, semLoc);
  m_AST.emplace_back(std::move(protocol));
}
