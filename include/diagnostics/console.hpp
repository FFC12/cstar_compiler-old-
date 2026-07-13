#ifndef CSTAR_DIAGNOSTICS_CONSOLE_HPP
#define CSTAR_DIAGNOSTICS_CONSOLE_HPP

#include <cstdlib>
namespace cstar::console {

struct Style {
  static constexpr const char* Reset = "\x1B[0m";
  static constexpr const char* Bold = "\x1B[1m";
  static constexpr const char* Dim = "\x1B[2m";
  static constexpr const char* Red = "\x1B[31m";
  static constexpr const char* Green = "\x1B[32m";
  static constexpr const char* Yellow = "\x1B[33m";
  static constexpr const char* Blue = "\x1B[34m";
  static constexpr const char* Magenta = "\x1B[35m";
  static constexpr const char* Cyan = "\x1B[36m";
  static constexpr const char* White = "\x1B[37m";
  static constexpr const char* BrightWhite = "\x1B[37;1m";
};

inline bool colorsEnabled() {
  if (const char* noColor = std::getenv("NO_COLOR")) {
    return noColor[0] == '\0';
  }
  return true;
}

inline const char* paint(const char* style) {
  return colorsEnabled() ? style : "";
}

}  // namespace cstar::console

#endif
