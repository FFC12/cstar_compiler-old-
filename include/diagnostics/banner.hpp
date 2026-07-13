#ifndef CSTAR_DIAGNOSTICS_BANNER_HPP
#define CSTAR_DIAGNOSTICS_BANNER_HPP

#include <iosfwd>

namespace cstar::diagnostics {

class CompilerBanner {
 public:
  static void print(std::ostream& out);
};

}  // namespace cstar::diagnostics

#endif
