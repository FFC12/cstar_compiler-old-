#include <parser/parser_private.hpp>

using namespace cstar::parser_private;

bool CStarParser::isType(const TokenInfo& token) {
  switch (token.getTokenKind()) {
    case VOID:
    case I8:
    case I16:
    case I32:
    case I64:
    case INT:
    case U8:
    case U16:
    case U32:
    case U64:
    case U128:
    case UINT:
    case ISIZE:
    case USIZE:
    case F32:
    case F64:
    case FLOAT:
    case UCHAR:
    case CHAR:
    case BOOL:
    case VEC2:
    case VEC3:
      // Every single IDENT is going to be interpreted as Symbol
      // case IDENT:
    case VEC4:
      return true;
    default:
      return false;
  }
}

bool CStarParser::isCastableOperator(const TokenInfo& token) {
  switch (token.getTokenKind()) {
    case UNSAFE_CAST:
    case CAST:
    case AS:
      return true;
    default:
      return false;
  }
}

bool CStarParser::isOperator(const TokenInfo& token) {
  switch (token.getTokenKind()) {
    case LPAREN:
    case QMARK:
      // those are because detecting is binary op
    case COLON:
      // suspicious
   // case LSQPAR:
    case RSQPAR:
      //
    case COLONCOLON:
    case UNSAFE_CAST:
    case CAST:
    case PLUSPLUS:
    case MINUSMINUS:
    case DOT:
    case ARROW:
    case SIZEOF:
    case TYPEOF:
    case MOVE:
    case DEREF:
    case REF:
    case AS:
    case PLUS:
    case MINUS:
    case NOT:
    case TILDE:
    case STAR:
    case MOD:
    case DIV:
    case LSHIFT:
    case RSHIFT:
    case LT:
    case LTEQ:
    case GT:
    case GTEQ:
    case EQUALEQUAL:
    case NOTEQUAL:
    case AND:
    case XOR:
    case OR:
    case LAND:
    case LOR:
    case EQUAL:
    case RET:
    case PLUSEQ:
    case MINUSEQ:
    case STAREQ:
    case DIVEQ:
    case MODEQ:
    case LSHIFTEQ:
    case RSHIFTEQ:
    case ANDEQ:
    case XOREQ:
    case OREQ:
    case COMMA:
      return true;
    default:
      return false;
  }
}

bool CStarParser::isLinkageMark(const TokenInfo& token) {
  switch (token.getTokenKind()) {
    case EXPORT:  // extern
    case IMPORT:  // extern
      return true;
    default:
      return false;
  }
}

bool CStarParser::isDeclarationModifier(const TokenInfo& token) {
  switch (token.getTokenKind()) {
    case PUBLIC:
    case PRIVATE:
    case STATIC:
    case EXPORT:
    case IMPORT:
      return true;
    default:
      return false;
  }
}

DeclarationModifiers CStarParser::parseDeclarationModifiers(bool localScope) {
  DeclarationModifiers modifiers;

  while (isDeclarationModifier(currentTokenInfo())) {
    switch (currentTokenKind()) {
      case PUBLIC:
        if (localScope) {
          ParserError("'public' is only valid on module declarations",
                      currentTokenInfo());
        }
        modifiers.access = ACCESS_PUBLIC;
        break;
      case PRIVATE:
        if (localScope) {
          ParserError("'private' is only valid on module declarations",
                      currentTokenInfo());
        }
        modifiers.access = ACCESS_PRIVATE;
        break;
      case STATIC:
        if (localScope) {
          ParserError("'static' local declarations are not part of the current "
                      "lifetime model; use module-level static state",
                      currentTokenInfo());
        }
        if (modifiers.linkage == VisibilitySpecifier::VIS_EXPORT ||
            modifiers.linkage == VisibilitySpecifier::VIS_IMPORT) {
          ParserError("'static' cannot be combined with import/export linkage",
                      currentTokenInfo());
        }
        modifiers.isStatic = true;
        if (modifiers.linkage == VisibilitySpecifier::VIS_DEFAULT) {
          modifiers.linkage = VisibilitySpecifier::VIS_STATIC;
        }
        break;
      case EXPORT:
        if (localScope) {
          ParserError("'export' is only valid on module declarations",
                      currentTokenInfo());
        }
        if (modifiers.isStatic) {
          ParserError("'export' cannot be combined with static linkage",
                      currentTokenInfo());
        }
        modifiers.linkage = VisibilitySpecifier::VIS_EXPORT;
        break;
      case IMPORT:
        if (localScope) {
          ParserError("'import' is only valid on module declarations",
                      currentTokenInfo());
        }
        if (modifiers.isStatic) {
          ParserError("'import' cannot be combined with static linkage",
                      currentTokenInfo());
        }
        modifiers.linkage = VisibilitySpecifier::VIS_IMPORT;
        break;
      default:
        assert(false && "Unreachable declaration modifier");
    }
    this->advance();
    skipTopLevelTrivia();
  }

  return modifiers;
}

bool CStarParser::isPackageMark(const TokenInfo& token) {
  switch (token.getTokenKind()) {
    case INCLUDE:
      return true;
    default:
      return false;
  }
}

bool CStarParser::isTypeQualifier(const TokenInfo& token) {
  switch (token.getTokenKind()) {
    case CONST:
    case CONSTREF:
    case CONSTPTR:
    case READONLY:
      return true;
    default:
      return false;
  }
}

bool CStarParser::isShortcutOp(const TokenInfo& token) {
  switch (token.getTokenKind()) {
    case PLUSEQ:
    case MINUSEQ:
    case STAREQ:
    case DIVEQ:
    case MODEQ:
    case RSHIFTEQ:
    case LSHIFTEQ:
    case ANDEQ:
    case OREQ:
    case XOREQ:
    case EQUAL:
    case TYPEINF:
      return true;
    default:
      return false;
  }
}

ShortcutOp CStarParser::typeOfShortcutOp(const TokenInfo& token) {
  switch (token.getTokenKind()) {
    case PLUSEQ:
      return S_PLUS;
    case MINUSEQ:
      return S_MIN;
    case STAREQ:
      return S_STA;
    case DIVEQ:
      return S_DIV;
    case MODEQ:
      return S_MOD;
    case RSHIFTEQ:
      return S_SHR;
    case LSHIFTEQ:
      return S_SHL;
    case ANDEQ:
      return S_AND;
    case OREQ:
      return S_OR;
    case XOREQ:
      return S_XOR;
    case TYPEINF:
      return S_MOV;
    case EQUAL:
      return S_NONE;
    default:
      assert(false && "Unreacheable!");
      return S_NONE;
  }
}

Type CStarParser::typeOf(const TokenInfo& token) {
  switch (token.getTokenKind()) {
    case VOID:
      return Type::T_VOID;
    case I8:
      return Type::T_I8;
    case I16:
      return Type::T_I16;
    case I32:
      return Type::T_I32;
    case I64:
      return Type::T_I64;
    case INT:
      return Type::T_INT;
    case U8:
      return Type::T_U8;
    case U16:
      return Type::T_U16;
    case U32:
      return Type::T_U32;
    case U64:
      return Type::T_U64;
    case U128:
      return Type::T_U128;
    case UINT:
      return Type::T_UINT;
    case ISIZE:
      return Type::T_ISIZE;
    case USIZE:
      return Type::T_USIZE;
    case F32:
      return Type::T_F32;
    case F64:
      return Type::T_F64;
    case FLOAT:
      return Type::T_FLOAT;
    case UCHAR:
      return Type::T_UCHAR;
    case CHAR:
      return Type::T_CHAR;
    case BOOL:
      return Type::T_BOOL;
    case VEC2:
      return Type::T_VEC2;
    case VEC3:
      return Type::T_VEC3;
    case VEC4:
      return Type::T_VEC4;
      // every single IDENT is going to be interpreted as Symbol...
      // case IDENT:
      //   return Type::T_DEFINED;
    default:
      assert(false && "Unreacheable");
      return Type::T_VOID;
  }
}

TypeQualifier CStarParser::typeQualifierOf(const TokenInfo& tokenInfo) {
  switch (tokenInfo.getTokenKind()) {
    case CONST:
      return TypeQualifier::Q_CONST;
    case CONSTREF:
      return TypeQualifier::Q_CONSTREF;
    case CONSTPTR:
      return TypeQualifier::Q_CONSTPTR;
    case READONLY:
      return TypeQualifier::Q_READONLY;
    default:
      assert(false && "Unreacheable!");
      return TypeQualifier::Q_NONE;
  }
}
