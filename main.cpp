#include <codegen/codegen.hpp>
#include <cstdlib>
#include <cstring>
#include <diagnostics/banner.hpp>
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

static void ShowUsage() {
  std::cout
      << "Usage: cstar <file.cstar> [options]\n\n"
      << "Options:\n"
      << "  --emit=<ir|asm|obj|exe>   Select final artifact. Default: exe\n"
      << "  --emit-llvm               Alias for --emit=ir\n"
      << "  --emit-asm                Alias for --emit=asm\n"
      << "  --build-exe               Alias for --emit=exe\n"
      << "  --run                     Run generated executable after build\n"
      << "  --no-run                  Do not run generated executable\n"
      << "  --output-dir <path>       Write generated files into this directory\n"
      << "  --verbose, -v             Print backend/link commands\n"
      << "  --stats                   Print compiler pass timings and LoC\n"
      << "  --help, -h                Show this help\n";
}

static bool ParseEmitKind(const std::string& value,
                          CStarEmitKind& emitKind) {
  if (value == "ir" || value == "llvm" || value == "ll") {
    emitKind = CStarEmitKind::IR;
    return true;
  }
  if (value == "asm" || value == "s") {
    emitKind = CStarEmitKind::Assembly;
    return true;
  }
  if (value == "obj" || value == "object" || value == "o") {
    emitKind = CStarEmitKind::Object;
    return true;
  }
  if (value == "exe" || value == "bin" || value == "executable") {
    emitKind = CStarEmitKind::Executable;
    return true;
  }

  return false;
}

static bool ParseDriverArgs(int argc, char** argv, std::string& filepath,
                            CStarCodegenOptions& options, bool& showedHelp) {
  for (int i = 1; i < argc; ++i) {
    std::string arg = argv[i];

    if (arg == "--help" || arg == "-h") {
      ShowUsage();
      showedHelp = true;
      return false;
    }

    if (arg == "--verbose" || arg == "-v") {
      options.verbose = true;
      continue;
    }

    if (arg == "--stats") {
      options.stats = true;
      continue;
    }

    if (arg == "--run") {
      options.runAfterBuild = true;
      continue;
    }

    if (arg == "--no-run") {
      options.runAfterBuild = false;
      continue;
    }

    if (arg == "--emit-llvm") {
      options.emitKind = CStarEmitKind::IR;
      continue;
    }

    if (arg == "--emit-asm") {
      options.emitKind = CStarEmitKind::Assembly;
      continue;
    }

    if (arg == "--build-exe") {
      options.emitKind = CStarEmitKind::Executable;
      continue;
    }

    constexpr const char* emitPrefix = "--emit=";
    if (arg.rfind(emitPrefix, 0) == 0) {
      const std::string value = arg.substr(std::strlen(emitPrefix));
      if (!ParseEmitKind(value, options.emitKind)) {
        const auto message = "Unknown emit kind: " + value + "\n";
        LogError(message.c_str());
        ShowUsage();
        return false;
      }
      continue;
    }

    constexpr const char* outputDirPrefix = "--output-dir=";
    if (arg.rfind(outputDirPrefix, 0) == 0) {
      options.outputDir = arg.substr(std::strlen(outputDirPrefix));
      continue;
    }

    if (arg == "--output-dir") {
      if (i + 1 >= argc) {
        LogError("--output-dir requires a path\n");
        ShowUsage();
        return false;
      }
      options.outputDir = argv[++i];
      continue;
    }

    if (!arg.empty() && arg[0] == '-') {
      const auto message = "Unknown option: " + arg + "\n";
      LogError(message.c_str());
      ShowUsage();
      return false;
    }

    if (!filepath.empty()) {
      LogError("Multiple input files are not supported yet\n");
      ShowUsage();
      return false;
    }

    filepath = arg;
  }

  if (filepath.empty()) {
    LogError("No input file specified!\n");
    ShowUsage();
    return false;
  }

  return true;
}

int main(int argc, char** argv) {
  cstar::diagnostics::CompilerBanner::print(std::cout);

  std::string relativePath;
  CStarCodegenOptions codegenOptions;
  bool showedHelp = false;
  if (ParseDriverArgs(argc, argv, relativePath, codegenOptions, showedHelp)) {

    std::ifstream file;
    file.open(relativePath);
    if (!file.is_open()) {
      const auto message = "Cannot open input file: " + relativePath + "\n";
      LogError(message.c_str());
      return 1;
    }

    std::stringstream ss;
    ss << file.rdbuf();

    auto realPathStr = std::filesystem::absolute(relativePath).string();
    auto realPath = static_cast<char*>(std::malloc(realPathStr.size() + 1));
    std::memcpy(realPath, realPathStr.c_str(), realPathStr.size() + 1);

    const std::shared_ptr<char> realPathPtr(realPath, std::free);

    time_t startTime = time(nullptr);
    CStarLexer lexer(ss.str(), realPathPtr);
    CStarParser parser(std::move(lexer), codegenOptions.stats);
    CStarCodegen codegen(std::move(parser), realPathStr, codegenOptions);
    codegen.build();
    // codegen.printAST();

    if (codegenOptions.stats) {
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
    return showedHelp ? 0 : 1;
  }

  return 0;
}
