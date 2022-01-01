#include <iostream>
#include <vector>
#include <fstream>
#include <sstream>
#include <lexer/lexer.hpp>
#include <parser/parser.hpp>

int main(int argc, char** argv) {
  if(argc > 1) {
    std::ifstream file;
    file.open(argv[1]);
    std::stringstream ss;
    ss << file.rdbuf(); 
    CStarLexer lexer(ss.str());
    CStarParser parser(std::move(lexer));
    parser.parse();

  } else {
    std::cerr << "No argument specified!\n\n";
    std::cerr << "  Usage: ./CSTAR.exe file.cstar";
  }
  return 0;
}
