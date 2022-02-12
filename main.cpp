#include <stdlib.h>  // for realpath

#include <fstream>
#include <iostream>
#include <lexer/lexer.hpp>
#include <parser/parser.hpp>
#include <sstream>
#include <vector>

int main(int argc, char** argv) {
  std::cout << YEL
      "\n"
      "________/\\\\\\\\\\\\\\\\\\________/\\\\\\_______        \n"
      " _____/\\\\\\////////___/\\\\\\_\\/\\\\\\__/\\\\\\_       \n"
      "  ___/\\\\\\/___________\\////\\\\\\\\\\\\\\\\\\//__      \n"
      "   __/\\\\\\________________\\////\\\\\\//_____     \n"
      "    _\\/\\\\\\____Alpha v0.1___/\\\\\\\\\\\\\\\\\\____    \n"
      "     _\\//\\\\\\_____________/\\\\\\///\\\\\\///\\\\\\_   \n"
      "      __\\///\\\\\\__________\\///__\\/\\\\\\_\\///__  \n"
      "       ____\\////\\\\\\\\\\\\\\\\\\_______\\///________ \n"
      "        _______\\/////////___Author: FFC12____\n"
      "" RESET "\n\n";

  std::cout << RED
      "[!] " RESET CYN
      "This version of the compiler is not ready to use in production stage. \n"
      "Be aware of this may cause to bugs. And if you encountered an "
      "unexpected \n"
      "error or behaviour please let me know by fatihsaika@gmail.com or feel "
      "free \n"
      "to open an issue on Github!\n\n" RESET;

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
    std::cout << GRN "======= COMPILATION FINISHED =====" << RESET << "\n\n";

  } else {
    LogError("No argument specified!\n");
    LogError("--- Usage: ./CSTAR file.cstar\n");
  }

  return 0;
}
