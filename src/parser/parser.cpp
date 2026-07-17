#include <parser/parser.hpp>
#include <cstar_config.hpp>

#include <algorithm>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <filesystem>
#include <fstream>
#include <limits>
#include <set>
#include <sstream>
#include <string>

static bool IsProposalOnlyTopLevel(TokenKind kind) {
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

static bool IsMacroParamKindName(const std::string& name) {
  return name == "expr" || name == "stmt" || name == "item" ||
         name == "type" || name == "ident" || name == "tokens";
}

static bool IsMacroReturnKindName(const std::string& name) {
  return name == "expr" || name == "stmt" || name == "item" ||
         name == "type";
}

static std::filesystem::path ResolveLogicalIncludeSource(
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

static bool IsStdSourceIncludePath(const std::filesystem::path& path) {
  return path.begin() != path.end() && *path.begin() == "std";
}

static std::filesystem::path ResolveConfiguredStdlibRoot() {
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

static std::filesystem::path ResolveSourceIncludePath(
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

void CStarParser::parse() {
  m_TokenStream = this->m_Lexer.perform();
  collectPublicMacrosFromSourceIncludes();
  preprocessCompileTimeSurface();
  if (m_StatsEnabled) {
    this->m_Lexer.lexerStats();
  }
  this->m_StartTime = time(nullptr);

  if (!this->m_TokenStream.empty()) {
    // auto eof = std::move(m_TokenStream[-1]);
    this->m_CurrToken = m_TokenStream[m_TokenIndex];  //[0]
    this->translationUnit();
  } else {
    assert(false && "Parser token stream is not enough to parse!\n");
  }

  /*for(auto &node: this->m_AST) {
    node->debugNode();
  }*/

  this->parserStats();
}
