#ifndef CSTAR_DRIVER_DRIVER_HPP
#define CSTAR_DRIVER_DRIVER_HPP

#include <codegen/codegen.hpp>

#include <iosfwd>
#include <string>

namespace cstar::driver {

struct DriverArgs {
  std::string filepath;
  CStarCodegenOptions codegenOptions;
  bool showedHelp = false;
};

void showUsage(std::ostream& out);
bool parseArgs(int argc, char** argv, DriverArgs& args);

}  // namespace cstar::driver

#endif
