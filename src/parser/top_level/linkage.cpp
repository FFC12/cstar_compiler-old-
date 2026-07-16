#include <parser/parser_private.hpp>

using namespace cstar::parser_private;

std::string CStarParser::parseLinkSource() {
  expected(TokenKind::FROM);
  this->advance();
  skipTopLevelTrivia();

  expected(TokenKind::LITERAL);
  auto source = currentTokenStr();
  this->advance();
  return source;
}

void CStarParser::parseLinkageBlock(
    VisibilitySpecifier visibilitySpecifier) {
  std::string library;
  if (is(TokenKind::FROM)) {
    library = parseLinkSource();
    registerNativeLinkLibrary(library);
    skipTopLevelTrivia();
  }

  expected(TokenKind::LBRACK);
  this->advance();

  while (!is(TokenKind::RBRACK)) {
    skipTopLevelTrivia();
    if (is(TokenKind::RBRACK)) {
      break;
    }
    expected(TokenKind::IDENT);
    DeclarationModifiers modifiers;
    modifiers.linkage = visibilitySpecifier;
    funcDecl(modifiers, true);
    if (!library.empty()) {
      registerNativeLinkLibrary(library);
    }
    skipTopLevelTrivia();
  }

  expected(TokenKind::RBRACK);
  this->advance();
  skipTopLevelTrivia();
  if (is(TokenKind::SEMICOLON)) {
    this->advance();
  }
}

StructFieldInfo CStarParser::parseStructField(
    DeclarationModifiers modifiers) {
  if (modifiers.isStatic) {
    ParserError("static data members are not part of the current struct model; "
                "use module-level static state",
                prevTokenInfo());
  }

  StructFieldInfo field;
  field.isPublic = modifiers.access == ACCESS_PUBLIC;

  if (!isType(currentTokenInfo()) && !is(TokenKind::IDENT)) {
    ParserError("Expected field type in struct declaration",
                currentTokenInfo());
  }

  if (is(TokenKind::IDENT)) {
    field.type = TypeSpecifier::SPEC_DEFINED;
    field.definedTypeName = advanceDefinedTypeName();
  } else {
    field.type = typeSpecifierOf(currentTokenInfo());
    this->advance();
  }

  while (is(TokenKind::STAR) || is(TokenKind::XOR)) {
    const bool currentPointerIsUnique =
        this->currentTokenKind() == TokenKind::XOR;
    field.indirectionLevel = advancePointerType(currentPointerIsUnique);
    if (currentPointerIsUnique) {
      field.isUnique = true;
    }
  }

  if (is(TokenKind::AND)) {
    field.indirectionLevel = 1;
    field.isRef = true;
    this->advance();
  }

  expected(TokenKind::IDENT);
  field.name = currentTokenStr();
  this->advance();

  if (is(TokenKind::EQUAL) || is(TokenKind::TYPEINF)) {
    ParserError("struct field initializers are not implemented yet",
                currentTokenInfo());
  }

  expected(TokenKind::SEMICOLON);
  this->advance();
  return field;
}

TraitRequirementInfo CStarParser::parseTraitRequirement() {
  TraitRequirementInfo requirement;

  if (is(TokenKind::OPERATOR)) {
    ParserError(
        "operator requirements are not implemented yet; lifecycle allocation "
        "operators are compiler-reserved",
        currentTokenInfo());
  }

  expected(TokenKind::IDENT);
  requirement.name = currentTokenStr();
  this->advance();

  if (is(TokenKind::LPAREN)) {
    size_t depth = 0;
    do {
      if (is(TokenKind::LPAREN)) {
        depth += 1;
      } else if (is(TokenKind::RPAREN)) {
        if (depth == 0) {
          ParserError("Unexpected ')' in trait requirement",
                      currentTokenInfo());
        }
        depth -= 1;
      }
      this->advance();
    } while (depth > 0 && !is(TokenKind::_EOF));
  }

  if (is(TokenKind::COLONCOLON)) {
    this->advance();
    if (!isType(currentTokenInfo()) && !is(TokenKind::IDENT) &&
        !is(TokenKind::VOID)) {
      ParserError("Expected return type in trait requirement",
                  currentTokenInfo());
    }
    this->advance();
    while (is(TokenKind::STAR) || is(TokenKind::XOR) ||
           is(TokenKind::AND)) {
      this->advance();
    }
  }

  expected(TokenKind::SEMICOLON);
  this->advance();
  return requirement;
}
