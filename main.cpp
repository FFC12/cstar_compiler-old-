#include <stdlib.h>  // for realpath

#include <codegen/codegen.hpp>
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
      "    _\\/\\\\\\____Pre-alpha___/\\\\\\\\\\\\\\\\\\____    \n"
      "     _\\//\\\\\\_____________/\\\\\\///\\\\\\///\\\\\\_   \n"
      "      __\\///\\\\\\__________\\///__\\/\\\\\\_\\///__  \n"
      "       ____\\////\\\\\\\\\\\\\\\\\\_______\\///________ \n"
      "        _______\\/////////___Author: FFC12____\n"
      "" RES "\n\n";

  std::cout << REDISH
      "[!] " RES CYN
      "This version of the compiler is in early development stage (pre-alpha). So I don't recommend it to use in production stage yet. \n"
      RES YEL "[?] " RES CYN "If you encountered an unexpected error let me know by fatihsaika@gmail.com\n\n" RES;

  if (argc > 1) {
    char* relativePath = argv[1];

    std::ifstream file;
    file.open(relativePath);
    std::stringstream ss;
    ss << file.rdbuf();

    auto realPath = realpath(relativePath, nullptr);
    auto realPathStr = std::string(realPath);

    // Initialize unique_ptr
    const std::shared_ptr<char> realPathPtr(realPath);

    time_t startTime = time(nullptr);
    CStarLexer lexer(ss.str(), realPathPtr);
    CStarParser parser(std::move(lexer));
    CStarCodegen codegen(std::move(parser), realPathStr);
    codegen.build();
    // codegen.printAST();

    codegen.showStats(startTime,"Total Time");
    std::cout << GRN "======= COMPILATION FINISHED =====" << RES << "\n\n";
  } else {
    LogError("No argument specified!\n");
    LogError("--- Usage: ./CSTAR file.cstar\n");
  }

  return 0;
}
