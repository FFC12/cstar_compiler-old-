#include <stdlib.h>  // for realpath

#include <fstream>
#include <iostream>
#include <lexer/lexer.hpp>
#include <parser/parser.hpp>
#include <sstream>
#include <vector>

int main(int argc, char** argv) {
  if (argc > 1) {
    char* relativePath = argv[1];

    std::ifstream file;
    file.open(relativePath);
    std::stringstream ss;
    ss << file.rdbuf();

    // Initialize unique_ptr
    const std::shared_ptr<char> realPath(realpath(relativePath, NULL));

    CStarLexer lexer(ss.str(), realPath);
    CStarParser parser(std::move(lexer));
    parser.parse();

  } else {
    LogError("No argument specified!\n");
    LogError("--- Usage: ./CSTAR file.cstar\n");
  }
  return 0;
}
