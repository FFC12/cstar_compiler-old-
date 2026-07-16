#include <parser/parser_private.hpp>

using namespace cstar::parser_private;

ASTNode CStarParser::advanceSymbol() {
  if (is(TokenKind::IDENT) || is(TokenKind::SELF)) {
    auto symbolName = this->currentTokenStr();

    auto tokenPos = currentTokenInfo().getTokenPositionInfo();
    auto semLoc = SemanticLoc(tokenPos.begin, tokenPos.end, tokenPos.line);

    this->advance();

    auto symbolNode = std::make_unique<SymbolAST>(symbolName, semLoc);
    bool transitionFlag = false;
    bool isUniquePtr = false;
    size_t indirectionLevel = 0;
    bool isRef = false;

    if (TypeFlag) {
      if (is(TokenKind::AND)) {
        isRef = true;
        this->advance();
        semLoc.end += indirectionLevel;
      } else {
        // This is symbol to type transition (Actually this can be done when
        // performed semantic analysis for this node but we make things easier
        // or maybe we not... Not sure)
        // * | ^
        while (is(TokenKind::STAR) || is(TokenKind::XOR)) {
          isUniquePtr = this->currentTokenKind() == TokenKind::XOR;
          transitionFlag = true;

          indirectionLevel = advancePointerType(isUniquePtr);
          // std::cout << "Symbol to Type Transition Indirection Level: "
          //           << indirectionLevel << "\n";
          semLoc.end += indirectionLevel;
        }
      }
    }

    if (transitionFlag) {
      return std::make_unique<TypeAST>(TypeSpecifier::SPEC_DEFINED,
                                       std::move(symbolNode), isUniquePtr, true,
                                       isRef, indirectionLevel, semLoc);
    } else {
      return std::move(symbolNode);
    }
  }

  return nullptr;
}
