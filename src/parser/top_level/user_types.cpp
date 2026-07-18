#include <parser/parser_private.hpp>

using namespace cstar::parser_private;

void CStarParser::parseTraitDecl(DeclarationModifiers declarationModifiers) {
  if (declarationModifiers.linkage == VisibilitySpecifier::VIS_IMPORT ||
      declarationModifiers.linkage == VisibilitySpecifier::VIS_EXPORT) {
    ParserError("trait visibility uses public/private, not import/export",
                currentTokenInfo());
  }
  if (declarationModifiers.isStatic) {
    ParserError("static trait declarations are not valid", currentTokenInfo());
  }

  auto posInfo = currentTokenInfo().getTokenPositionInfo();
  size_t begin = posInfo.begin;
  size_t line = posInfo.line;

  this->advance();
  expected(TokenKind::IDENT);
  auto traitName = currentTokenStr();
  this->advance();

  if (is(TokenKind::LT)) {
    ParserError("generic traits are proposal-only in this compiler phase",
                currentTokenInfo());
  }

  expectBlockStart();
  this->advance();

  std::vector<TraitRequirementInfo> requirements;
  while (!is(TokenKind::RBRACK) && !is(TokenKind::_EOF)) {
    skipTopLevelTrivia();
    if (is(TokenKind::RBRACK)) {
      break;
    }
    requirements.push_back(parseTraitRequirement());
  }

  expected(TokenKind::RBRACK);
  posInfo = currentTokenInfo().getTokenPositionInfo();
  SemanticLoc semLoc(begin, posInfo.end, line);
  this->advance();

  const auto languageItem = m_PendingLanguageItem;
  m_PendingLanguageItem.clear();
  this->m_AST.emplace_back(std::make_unique<TraitAST>(
      traitName, std::move(requirements), declarationModifiers.access,
      languageItem, semLoc));

  skipTopLevelTrivia();
  if (is(TokenKind::SEMICOLON)) {
    this->advance();
  }
}

static bool IsEnumUnderlyingType(TypeSpecifier type) {
  switch (type) {
    case TypeSpecifier::SPEC_I8:
    case TypeSpecifier::SPEC_I16:
    case TypeSpecifier::SPEC_I32:
    case TypeSpecifier::SPEC_I64:
    case TypeSpecifier::SPEC_INT:
    case TypeSpecifier::SPEC_U8:
    case TypeSpecifier::SPEC_U16:
    case TypeSpecifier::SPEC_U32:
    case TypeSpecifier::SPEC_U64:
    case TypeSpecifier::SPEC_UINT:
    case TypeSpecifier::SPEC_ISIZE:
    case TypeSpecifier::SPEC_USIZE:
      return true;
    default:
      return false;
  }
}

static uint64_t EnumUnderlyingMax(TypeSpecifier type) {
  switch (type) {
    case TypeSpecifier::SPEC_I8:
      return static_cast<uint64_t>(std::numeric_limits<int8_t>::max());
    case TypeSpecifier::SPEC_I16:
      return static_cast<uint64_t>(std::numeric_limits<int16_t>::max());
    case TypeSpecifier::SPEC_I32:
      return static_cast<uint64_t>(std::numeric_limits<int32_t>::max());
    case TypeSpecifier::SPEC_I64:
    case TypeSpecifier::SPEC_INT:
    case TypeSpecifier::SPEC_ISIZE:
      return static_cast<uint64_t>(std::numeric_limits<int64_t>::max());
    case TypeSpecifier::SPEC_U8:
      return std::numeric_limits<uint8_t>::max();
    case TypeSpecifier::SPEC_U16:
      return std::numeric_limits<uint16_t>::max();
    case TypeSpecifier::SPEC_U32:
      return std::numeric_limits<uint32_t>::max();
    case TypeSpecifier::SPEC_U64:
    case TypeSpecifier::SPEC_U128:
    case TypeSpecifier::SPEC_UINT:
    case TypeSpecifier::SPEC_USIZE:
      return std::numeric_limits<uint64_t>::max();
    default:
      return 0;
  }
}

static bool IsZeroOrPowerOfTwo(uint64_t value) {
  return value == 0 || (value & (value - 1)) == 0;
}

void CStarParser::parseEnumDecl(DeclarationModifiers declarationModifiers,
                                bool isFlags) {
  if (declarationModifiers.linkage == VisibilitySpecifier::VIS_IMPORT ||
      declarationModifiers.linkage == VisibilitySpecifier::VIS_EXPORT) {
    ParserError("enum visibility uses public/private, not import/export",
                currentTokenInfo());
  }
  if (declarationModifiers.isStatic) {
    ParserError("static enum declarations are not valid", currentTokenInfo());
  }

  auto posInfo = currentTokenInfo().getTokenPositionInfo();
  size_t begin = posInfo.begin;
  size_t line = posInfo.line;

  if (isFlags) {
    expected(TokenKind::FLAGS);
    this->advance();
    skipTopLevelTrivia();
    expected(TokenKind::ENUM);
  }

  this->advance();
  expected(TokenKind::IDENT);
  auto enumName = currentTokenStr();
  this->advance();

  expected(TokenKind::COLON);
  this->advance();
  if (!isType(currentTokenInfo())) {
    ParserError("Expected enum underlying integer type", currentTokenInfo());
  }

  auto underlyingType = typeSpecifierOf(currentTokenInfo());
  if (!IsEnumUnderlyingType(underlyingType)) {
    ParserError("Enum underlying type must be an integer type",
                currentTokenInfo());
  }
  const auto maxValue = EnumUnderlyingMax(underlyingType);
  this->advance();

  expectBlockStart();
  this->advance();

  uint64_t nextValue = 0;
  bool implicitValueOverflow = false;
  std::set<std::string> memberNames;
  std::set<uint64_t> memberValues;
  std::vector<EnumMemberInfo> members;
  while (!is(TokenKind::RBRACK) && !is(TokenKind::_EOF)) {
    skipTopLevelTrivia();
    if (is(TokenKind::RBRACK)) {
      break;
    }

    expected(TokenKind::IDENT);
    EnumMemberInfo member;
    member.name = currentTokenStr();
    member.value = nextValue;
    if (memberNames.count(member.name) != 0) {
      ParserError("Redefinition of enum member '" + member.name + "'",
                  currentTokenInfo());
    }
    memberNames.insert(member.name);
    this->advance();

    if (is(TokenKind::EQUAL)) {
      this->advance();
      expected(TokenKind::SCALARI);
      try {
        member.value = std::stoull(currentTokenStr());
      } catch (const std::exception&) {
        ParserError("Enum member value '" + currentTokenStr() +
                        "' is not a valid unsigned integer literal",
                    currentTokenInfo());
      }
      this->advance();
    } else if (isFlags) {
      ParserError("Flags enum member '" + member.name +
                      "' must declare an explicit bit value",
                  prevTokenInfo());
    } else if (implicitValueOverflow) {
      ParserError("Implicit enum member value for '" + member.name +
                      "' overflows the enum underlying type",
                  prevTokenInfo());
    }

    if (member.value > maxValue) {
      ParserError("Enum member '" + member.name + "' value '" +
                      std::to_string(member.value) +
                      "' overflows the enum underlying type",
                  prevTokenInfo());
    }

    if (isFlags && !IsZeroOrPowerOfTwo(member.value)) {
      ParserError("Flags enum member '" + member.name +
                      "' must be 0 or a power-of-two bit value",
                  prevTokenInfo());
    }

    if (memberValues.count(member.value) != 0) {
      ParserError("Duplicate enum member value '" +
                      std::to_string(member.value) + "' in enum '" +
                      enumName + "'",
                  prevTokenInfo());
    }
    memberValues.insert(member.value);

    members.push_back(std::move(member));
    if (members.back().value == maxValue) {
      implicitValueOverflow = true;
    } else {
      nextValue = members.back().value + 1;
      implicitValueOverflow = false;
    }
    skipTopLevelTrivia();

    if (is(TokenKind::COMMA)) {
      this->advance();
      continue;
    }

    if (!is(TokenKind::RBRACK)) {
      expected({TokenKind::COMMA, TokenKind::RBRACK});
    }
  }

  expected(TokenKind::RBRACK);
  posInfo = currentTokenInfo().getTokenPositionInfo();
  SemanticLoc semLoc(begin, posInfo.end, line);
  this->advance();

  if (members.empty()) {
    ParserError("Enum must declare at least one member", currentTokenInfo());
  }

  this->m_AST.emplace_back(std::make_unique<EnumAST>(
      enumName, underlyingType, std::move(members),
      declarationModifiers.access, semLoc, isFlags));

  skipTopLevelTrivia();
  if (is(TokenKind::SEMICOLON)) {
    this->advance();
  }
}

void CStarParser::parseStructDecl(DeclarationModifiers declarationModifiers) {
  if (declarationModifiers.linkage == VisibilitySpecifier::VIS_IMPORT ||
      declarationModifiers.linkage == VisibilitySpecifier::VIS_EXPORT) {
    ParserError("struct visibility uses public/private, not import/export",
                currentTokenInfo());
  }
  if (declarationModifiers.isStatic) {
    ParserError("static struct declarations are not implemented yet",
                currentTokenInfo());
  }

  auto posInfo = currentTokenInfo().getTokenPositionInfo();
  size_t begin = posInfo.begin;
  size_t line = posInfo.line;

  this->advance();
  expected(TokenKind::IDENT);
  auto structName = currentTokenStr();
  this->advance();

  if (is(TokenKind::LT) || is(TokenKind::FROM)) {
    ParserError(
        "generic/attribute struct syntax is proposal-only; use "
        "`struct Name { field; ... }` for the current MVP",
        currentTokenInfo());
  }

  std::vector<std::string> traits;
  if (is(TokenKind::WITH)) {
    this->advance();
    do {
      skipTopLevelTrivia();
      expected(TokenKind::IDENT);
      traits.push_back(advanceDefinedTypeName());
      skipTopLevelTrivia();
      if (!is(TokenKind::COMMA)) {
        break;
      }
      this->advance();
    } while (!is(TokenKind::_EOF));
  }

  expectBlockStart();
  this->advance();

  std::vector<StructFieldInfo> fields;
  std::vector<ASTNode> methods;
  while (!is(TokenKind::RBRACK) && !is(TokenKind::_EOF)) {
    skipTopLevelTrivia();
    if (is(TokenKind::RBRACK)) {
      break;
    }

    DeclarationModifiers memberModifiers = parseDeclarationModifiers(false);

    if (is(TokenKind::ALLOCATOR)) {
      ParserError("struct allocator hooks require the allocator/trait phase",
                  currentTokenInfo());
    }

    if (is(TokenKind::OPERATOR)) {
      const auto astSizeBeforeMethod = m_AST.size();
      funcDecl(memberModifiers, false, structName);
      while (m_AST.size() > astSizeBeforeMethod) {
        methods.push_back(std::move(m_AST.back()));
        m_AST.pop_back();
      }
      continue;
    }

    bool outOfSize = false;
    auto nextToken = nextTokenInfo(outOfSize).getTokenKind();
    if (!outOfSize &&
        (is(TokenKind::IDENT) || is(TokenKind::CONSTRUCTOR) ||
         is(TokenKind::DESTRUCTOR) || is(TokenKind::NEW) ||
         is(TokenKind::OPERATOR)) &&
        (nextToken == TokenKind::LPAREN ||
         nextToken == TokenKind::COLONCOLON ||
         nextToken == TokenKind::LBRACK)) {
      const auto astSizeBeforeMethod = m_AST.size();
      funcDecl(memberModifiers, false, structName);
      while (m_AST.size() > astSizeBeforeMethod) {
        methods.push_back(std::move(m_AST.back()));
        m_AST.pop_back();
      }
      continue;
    }

    fields.push_back(parseStructField(memberModifiers));
  }

  expected(TokenKind::RBRACK);
  posInfo = currentTokenInfo().getTokenPositionInfo();
  SemanticLoc semLoc(begin, posInfo.end, line);
  this->advance();

  auto structAst =
      std::make_unique<StructAST>(structName, std::move(fields),
                                  std::move(traits),
                                  declarationModifiers.access, semLoc);
  this->m_AST.emplace_back(std::move(structAst));
  for (auto &method : methods) {
    this->m_AST.emplace_back(std::move(method));
  }

  skipTopLevelTrivia();
  if (is(TokenKind::SEMICOLON)) {
    this->advance();
  }
}
