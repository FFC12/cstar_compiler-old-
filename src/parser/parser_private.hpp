#ifndef CSTAR_PARSER_PRIVATE_HPP
#define CSTAR_PARSER_PRIVATE_HPP

#include <cstar_config.hpp>
#include <lexer/token.hpp>
#include <parser/parser.hpp>

#include <algorithm>
#include <cstdint>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <string>
#include <string_view>

namespace cstar::parser_private {

inline bool TypeFlag = false;

inline bool IsIgnorableExprToken(TokenKind kind) {
  return kind == TokenKind::COMMENT || kind == TokenKind::LINEFEED;
}

inline bool CanContinueExpressionAcrossLine(TokenKind previous) {
  return previous == TokenKind::EQUAL || previous == TokenKind::RET ||
         previous == TokenKind::COMMA || previous == TokenKind::LPAREN ||
         previous == TokenKind::LSQPAR || previous == TokenKind::COLON ||
         previous == TokenKind::QMARK || previous == TokenKind::TYPEINF ||
         previous == TokenKind::PLUS || previous == TokenKind::MINUS ||
         previous == TokenKind::STAR || previous == TokenKind::DIV ||
         previous == TokenKind::MOD || previous == TokenKind::AND ||
         previous == TokenKind::LAND || previous == TokenKind::OR ||
         previous == TokenKind::LOR || previous == TokenKind::XOR ||
         previous == TokenKind::LT || previous == TokenKind::LTEQ ||
         previous == TokenKind::GT || previous == TokenKind::GTEQ ||
         previous == TokenKind::EQUALEQUAL ||
         previous == TokenKind::NOTEQUAL || previous == TokenKind::LSHIFT ||
         previous == TokenKind::RSHIFT || previous == TokenKind::ARROW ||
         previous == TokenKind::DOT || previous == TokenKind::COLONCOLON;
}

inline bool IsValueOperatorToken(TokenKind kind) {
  switch (kind) {
    case PLUS:
    case MINUS:
    case STAR:
    case DIV:
    case MOD:
    case EQUALEQUAL:
    case NOTEQUAL:
    case LT:
    case LTEQ:
    case GT:
    case GTEQ:
      return true;
    default:
      return false;
  }
}

inline bool IsProposalOnlyTopLevel(TokenKind kind) {
  switch (kind) {
    case PROTOCOL:
    case STATE:
    case DYN:
    case CONSTRUCTOR:
    case DESTRUCTOR:
    case ALLOCATOR:
    case EXCEPT:
    case THROW:
    case DEFER:
    case ASYNC:
    case AWAIT:
    case SELF:
    case IS:
    case PROTO:
      return true;
    default:
      return false;
  }
}

inline bool IsMacroParamKindName(const std::string& name) {
  return name == "expr" || name == "stmt" || name == "item" ||
         name == "type" || name == "ident" || name == "tokens";
}

inline bool IsMacroReturnKindName(const std::string& name) {
  return name == "expr" || name == "stmt" || name == "item" ||
         name == "type";
}

inline std::filesystem::path ResolveLogicalIncludeSource(
    const std::string& includeName) {
  if (includeName.size() >= 6 &&
      includeName.substr(includeName.size() - 6) == ".cstar") {
    return std::filesystem::path(includeName);
  }

  constexpr std::string_view kStdPrefix = "std:";
  if (includeName.rfind(std::string(kStdPrefix), 0) == 0) {
    auto logical = includeName.substr(kStdPrefix.size());
    const auto memberSeparator = logical.find(':');
    if (memberSeparator != std::string::npos) {
      logical = logical.substr(0, memberSeparator);
    }
    if (logical.empty()) {
      logical = "core";
    }
    return std::filesystem::path("std") / (logical + ".cstar");
  }

  return {};
}

inline bool IsStdSourceIncludePath(const std::filesystem::path& path) {
  return path.begin() != path.end() && *path.begin() == "std";
}

inline std::filesystem::path ResolveConfiguredStdlibRoot() {
  if (const char* stdlibPath = std::getenv("CSTAR_STDLIB_PATH")) {
    if (stdlibPath[0] != '\0') {
      return std::filesystem::path(stdlibPath);
    }
  }

  if (const char* sourceRoot = std::getenv("CSTAR_SOURCE_ROOT")) {
    if (sourceRoot[0] != '\0') {
      return std::filesystem::path(sourceRoot) / "std";
    }
  }

  return std::filesystem::path(CSTAR_SOURCE_ROOT) / "std";
}

inline std::filesystem::path ResolveSourceIncludePath(
    const std::filesystem::path& includePath,
    const std::filesystem::path& includingFile) {
  if (!includePath.is_relative()) {
    return std::filesystem::absolute(includePath).lexically_normal();
  }

  std::filesystem::path resolved;
  if (IsStdSourceIncludePath(includePath)) {
    auto relativeInsideStd = includePath;
    relativeInsideStd = relativeInsideStd.lexically_relative("std");
    resolved = ResolveConfiguredStdlibRoot() / relativeInsideStd;
  } else {
    resolved = includingFile.parent_path() / includePath;
  }

  return std::filesystem::absolute(resolved).lexically_normal();
}

}  // namespace cstar::parser_private

#endif
