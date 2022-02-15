#ifndef BASE_HPP
#define BASE_HPP
#include <iostream>

#define REDISH "\x1B[31m"
#define GRN "\x1B[32m"
#define YEL "\x1B[33m"
#define BLU "\x1B[34m"
#define MAG "\x1B[35m"
#define CYN "\x1B[36m"
#define WHT "\x1B[37m"
#define BWHT "\u001b[37;1m"
#define RES "\x1B[0m"

static void LogError(const char* mesg) {
  std::cout << REDISH << mesg << RES << std::endl;
}

static std::string ExtractFilenameFromPath(std::string& s,
                                           std::string delimiter) {
  size_t pos = 0;
  std::string token;
  s += "/";
  while ((pos = s.find(delimiter)) != std::string::npos) {
    token = s.substr(0, pos);
    s.erase(0, pos + delimiter.length());
  }

  return token;
}

enum LexerStatus { LEXER_OK, LEXER_ERR };

enum ParserStatus { PARSER_OK, PARSER_ERR };

#endif
