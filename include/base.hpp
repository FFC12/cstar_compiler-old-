#ifndef BASE_HPP
#define BASE_HPP
#include <diagnostics/console.hpp>
#include <filesystem>
#include <iostream>

#define REDISH cstar::console::paint(cstar::console::Style::Red)
#define GRN cstar::console::paint(cstar::console::Style::Green)
#define YEL cstar::console::paint(cstar::console::Style::Yellow)
#define BLU cstar::console::paint(cstar::console::Style::Blue)
#define MAG cstar::console::paint(cstar::console::Style::Magenta)
#define CYN cstar::console::paint(cstar::console::Style::Cyan)
#define WHT cstar::console::paint(cstar::console::Style::White)
#define BWHT cstar::console::paint(cstar::console::Style::BrightWhite)
#define RES cstar::console::paint(cstar::console::Style::Reset)

static void LogError(const char* mesg) {
  std::cout << REDISH << mesg << RES << std::endl;
}

static std::string ExtractFilenameFromPath(std::string& s,
                                           std::string delimiter) {
  auto filename = std::filesystem::path(s).filename().string();
  if (!filename.empty()) return filename;

  size_t pos = 0;
  std::string token;
  s += delimiter;
  while ((pos = s.find(delimiter)) != std::string::npos) {
    token = s.substr(0, pos);
    s.erase(0, pos + delimiter.length());
  }

  return token;
}

enum LexerStatus { LEXER_OK, LEXER_ERR };

enum ParserStatus { PARSER_OK, PARSER_ERR };

#endif
