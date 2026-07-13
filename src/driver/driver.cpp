#include <driver/driver.hpp>

#include <cstring>
#include <diagnostics/console.hpp>
#include <iostream>

namespace cstar::driver {

void showUsage(std::ostream& out) {
  out << "Usage: cstar <file.cstar> [options]\n\n"
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

static bool parseEmitKind(const std::string& value, CStarEmitKind& emitKind) {
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

bool parseArgs(int argc, char** argv, DriverArgs& args) {
  for (int i = 1; i < argc; ++i) {
    std::string arg = argv[i];

    if (arg == "--help" || arg == "-h") {
      showUsage(std::cout);
      args.showedHelp = true;
      return false;
    }

    if (arg == "--verbose" || arg == "-v") {
      args.codegenOptions.verbose = true;
      continue;
    }

    if (arg == "--stats") {
      args.codegenOptions.stats = true;
      continue;
    }

    if (arg == "--run") {
      args.codegenOptions.runAfterBuild = true;
      continue;
    }

    if (arg == "--no-run") {
      args.codegenOptions.runAfterBuild = false;
      continue;
    }

    if (arg == "--emit-llvm") {
      args.codegenOptions.emitKind = CStarEmitKind::IR;
      continue;
    }

    if (arg == "--emit-asm") {
      args.codegenOptions.emitKind = CStarEmitKind::Assembly;
      continue;
    }

    if (arg == "--build-exe") {
      args.codegenOptions.emitKind = CStarEmitKind::Executable;
      continue;
    }

    constexpr const char* emitPrefix = "--emit=";
    if (arg.rfind(emitPrefix, 0) == 0) {
      const std::string value = arg.substr(std::strlen(emitPrefix));
      if (!parseEmitKind(value, args.codegenOptions.emitKind)) {
        const auto message = "Unknown emit kind: " + value + "\n";
        LogError(message.c_str());
        showUsage(std::cout);
        return false;
      }
      continue;
    }

    constexpr const char* outputDirPrefix = "--output-dir=";
    if (arg.rfind(outputDirPrefix, 0) == 0) {
      args.codegenOptions.outputDir = arg.substr(std::strlen(outputDirPrefix));
      continue;
    }

    if (arg == "--output-dir") {
      if (i + 1 >= argc) {
        LogError("--output-dir requires a path\n");
        showUsage(std::cout);
        return false;
      }
      args.codegenOptions.outputDir = argv[++i];
      continue;
    }

    if (!arg.empty() && arg[0] == '-') {
      const auto message = "Unknown option: " + arg + "\n";
      LogError(message.c_str());
      showUsage(std::cout);
      return false;
    }

    if (!args.filepath.empty()) {
      LogError("Multiple input files are not supported yet\n");
      showUsage(std::cout);
      return false;
    }

    args.filepath = arg;
  }

  if (args.filepath.empty()) {
    LogError("No input file specified!\n");
    showUsage(std::cout);
    return false;
  }

  return true;
}

}  // namespace cstar::driver
