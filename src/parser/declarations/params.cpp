#include <parser/parser_private.hpp>

using namespace cstar::parser_private;

void CStarParser::advanceParams(std::vector<ASTNode>& params,
                                bool isForwardDecl, bool& isVariadic) {
param_again:
  while (is(TokenKind::LINEFEED) || is(TokenKind::COMMENT)) {
    this->advance();
  }

  if (is(TokenKind::TRIPLET)) {
    isVariadic = true;
    this->advance();
    if (!is(TokenKind::RPAREN)) {
      ParserError("Variadic marker '...' must be the last parameter",
                  currentTokenInfo());
    }
    return;
  }

  ASTNode typeNode;
  TypeQualifier typeQualifier = TypeQualifier::Q_NONE;
  bool isNoMove = false;
  ASTNode symbol0, symbol1;

  if (is(TokenKind::NOMOVE)) {
    isNoMove = true;
    this->advance();
  }

  if (isTypeQualifier(currentTokenInfo())) {
    typeQualifier = typeQualifierOf(currentTokenInfo());
    this->advance();
  }

  if (is(TokenKind::NOMOVE)) {
    isNoMove = true;
    this->advance();
  }

  size_t beginLoc = currentTokenInfo().getTokenPositionInfo().begin;
  size_t line = currentTokenInfo().getTokenPositionInfo().line;

  if (isType(currentTokenInfo())) {  // cast allowed
    auto type = this->advanceType();

    std::vector<ASTNode> arrayDimensions;
    bool arrayFlag = this->advanceTypeSubscript(arrayDimensions);
    if (is(TokenKind::AND)) {
      this->advance();
      auto typeAst = dynamic_cast<TypeAST*>(type.get());
      typeAst->setIsRef(true);
    }

    if (!isForwardDecl) {
      // expected param name
      expected(TokenKind::IDENT);

      // get the param name
      symbol0 = this->advanceSymbol();
    } else if (is(TokenKind::IDENT)) {
      symbol0 = this->advanceSymbol();
    }

    size_t endLoc = currentTokenInfo().getTokenPositionInfo().end;
    auto semLoc = SemanticLoc(beginLoc, endLoc, line);
    if (isForwardDecl && symbol0 == nullptr) {
      symbol0 = std::make_unique<SymbolAST>(
          "__cstar_param" + std::to_string(params.size()), semLoc);
    }
    auto param =
        std::make_unique<ParamAST>(std::move(symbol0), nullptr, std::move(type),
                                   std::move(arrayDimensions), arrayFlag, true,
                                   false, false, true, typeQualifier, semLoc,
                                   isNoMove);
    params.emplace_back(std::move(param));
  } else if (is(TokenKind::IDENT)) {
    bool outOfSize = false;
    auto nextTokenInfo = this->nextTokenInfo(outOfSize);
    if (outOfSize) {
      ParserError("Unexpected token", currentTokenInfo());
    }

    auto nextToken = nextTokenInfo.getTokenKind();
    if (nextToken == TokenKind::STAR || nextToken == TokenKind::XOR ||
        nextToken == TokenKind::LSQPAR) {  // cast allowed
      // %100 defined type
      auto type = this->advanceDefinedType();

      std::vector<ASTNode> arrayDimensions;
      bool arrayFlag = this->advanceTypeSubscript(arrayDimensions);
      if (is(TokenKind::AND)) {
        this->advance();
        auto typeAst = dynamic_cast<TypeAST*>(type.get());
        typeAst->setIsRef(true);
      }

      if (!isForwardDecl) {
        expected(TokenKind::IDENT);
        // get the param name
        symbol0 = std::move(this->advanceSymbol());
      } else if (is(TokenKind::IDENT)) {
        symbol0 = std::move(this->advanceSymbol());
      }

      size_t endLoc = currentTokenInfo().getTokenPositionInfo().end;
      auto semLoc = SemanticLoc(beginLoc, endLoc, line);
      if (isForwardDecl && symbol0 == nullptr) {
        symbol0 = std::make_unique<SymbolAST>(
            "__cstar_param" + std::to_string(params.size()), semLoc);
      }

      auto param = std::make_unique<ParamAST>(
          std::move(symbol0), nullptr, std::move(type),
          std::move(arrayDimensions), arrayFlag, true, false, false, false,
          typeQualifier, semLoc, isNoMove);
      params.emplace_back(std::move(param));
    } else {
      // if the next token is primitive type...
      if (isType(nextTokenInfo)) {  // cast not allowed
        // get the param name
        if (!isForwardDecl) {
          expected(TokenKind::IDENT);
          symbol0 = this->advanceSymbol();
        } else if (is(TokenKind::IDENT)) {
          symbol0 = this->advanceSymbol();
        }

        auto type = this->advanceType();

        std::vector<ASTNode> arrayDimensions;
        bool arrayFlag = this->advanceTypeSubscript(arrayDimensions);
        if (is(TokenKind::AND)) {
          this->advance();
          auto typeAst = dynamic_cast<TypeAST*>(type.get());
          typeAst->setIsRef(true);
        }

        size_t endLoc = currentTokenInfo().getTokenPositionInfo().end;
        auto semLoc = SemanticLoc(beginLoc, endLoc, line);
        if (isForwardDecl && symbol0 == nullptr) {
          symbol0 = std::make_unique<SymbolAST>(
              "__cstar_param" + std::to_string(params.size()), semLoc);
        }

        auto param = std::make_unique<ParamAST>(
            std::move(symbol0), nullptr, std::move(type),
            std::move(arrayDimensions), arrayFlag, false, false, false, true,
            typeQualifier, semLoc, isNoMove);
        params.emplace_back(std::move(param));
      } else {
        auto prevNextTokenInfo = nextTokenInfo;
        outOfSize = false;
        nextTokenInfo = this->nextTokenInfo(outOfSize, 2);
        if (outOfSize) {
          ParserError("Unexpected token", currentTokenInfo());
        }

        nextToken = nextTokenInfo.getTokenKind();
        if (nextToken == TokenKind::STAR || nextToken == TokenKind::XOR ||
            nextToken == TokenKind::LSQPAR) {  // cast allowed
          // expect param name
          if (!isForwardDecl) {
            symbol0 = std::move(this->advanceSymbol());
          }

          expected(TokenKind::IDENT);
          // get the defined type
          auto type = std::move(this->advanceSymbol());
          std::vector<ASTNode> arrayDimensions;
          bool arrayFlag = this->advanceTypeSubscript(arrayDimensions);
          if (is(TokenKind::AND)) {
            this->advance();
            auto typeAst = dynamic_cast<TypeAST*>(type.get());
            typeAst->setIsRef(true);
          }

          size_t endLoc = currentTokenInfo().getTokenPositionInfo().end;
          auto semLoc = SemanticLoc(beginLoc, endLoc, line);

          auto param = std::make_unique<ParamAST>(
              std::move(symbol0), nullptr, std::move(type),
              std::move(arrayDimensions), arrayFlag, false, false, true, false,
              typeQualifier, semLoc, isNoMove);
          params.emplace_back(std::move(param));
        } else {
          auto type = this->advanceDefinedType();

          if (!isForwardDecl) {
            expected(TokenKind::IDENT);
            symbol0 = this->advanceSymbol();
          } else if (is(TokenKind::IDENT)) {
            symbol0 = this->advanceSymbol();
          }

          size_t endLoc = currentTokenInfo().getTokenPositionInfo().end;
          auto semLoc = SemanticLoc(beginLoc, endLoc, line);
          if (isForwardDecl && symbol0 == nullptr) {
            symbol0 = std::make_unique<SymbolAST>(
                "__cstar_param" + std::to_string(params.size()), semLoc);
          }

          auto param = std::make_unique<ParamAST>(
              std::move(symbol0), nullptr, std::move(type),
              std::vector<ASTNode>(), false, false, false, false, false,
              typeQualifier, semLoc, isNoMove);
          params.emplace_back(std::move(param));
        }
      }
    }

  } else {
    if (!is(TokenKind::RPAREN))
      ParserError("Expected a primitive or defined type", currentTokenInfo());
  }

  if (is(TokenKind::COMMA)) {
    // skip ','
    this->advance();
    while (is(TokenKind::LINEFEED) || is(TokenKind::COMMENT)) {
      this->advance();
    }
    if (isVariadic) {
      ParserError("Unexpected parameter after variadic marker '...'",
                  currentTokenInfo());
    }
    goto param_again;
  }
}
