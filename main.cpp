#include <codegen/codegen.hpp>
#include <cstdlib>
#include <cstring>
#include <diagnostics/banner.hpp>
#include <driver/driver.hpp>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <lexer/lexer.hpp>
#include <parser/parser.hpp>
#include <sstream>
#include <vector>

#ifdef _WIN32
using WinDword = unsigned long;
extern "C" __declspec(dllimport) WinDword __stdcall
GetConsoleProcessList(WinDword* processList, WinDword processCount);

static bool IsExplorerLaunchedConsole() {
  WinDword processList[2];
  return GetConsoleProcessList(processList, 2) == 1;
}
#endif

int main(int argc, char** argv) {
  cstar::diagnostics::CompilerBanner::print(std::cout);

  cstar::driver::DriverArgs driverArgs;
  if (cstar::driver::parseArgs(argc, argv, driverArgs)) {

    std::ifstream file;
    file.open(driverArgs.filepath);
    if (!file.is_open()) {
      const auto message = "Cannot open input file: " + driverArgs.filepath + "\n";
      LogError(message.c_str());
      return 1;
    }

    std::stringstream ss;
    ss << file.rdbuf();

    auto realPathStr = std::filesystem::absolute(driverArgs.filepath).string();
    auto realPath = static_cast<char*>(std::malloc(realPathStr.size() + 1));
    std::memcpy(realPath, realPathStr.c_str(), realPathStr.size() + 1);

    const std::shared_ptr<char> realPathPtr(realPath, std::free);

    time_t startTime = time(nullptr);
    CStarLexer lexer(ss.str(), realPathPtr);
    CStarParser parser(std::move(lexer), driverArgs.codegenOptions.stats);
    CStarCodegen codegen(std::move(parser), realPathStr,
                         driverArgs.codegenOptions);
    codegen.build();
    // codegen.printAST();

    if (driverArgs.codegenOptions.stats) {
      codegen.showStats(startTime, "Total Time");
      std::cout << GRN << "======= COMPILATION FINISHED =====" << RES << "\n\n";
    }
  } else {
#ifdef _WIN32
    if (IsExplorerLaunchedConsole()) {
      std::cout << "Press Enter to close...";
      std::cin.get();
    }
#endif
    return driverArgs.showedHelp ? 0 : 1;
  }

  return 0;
}
