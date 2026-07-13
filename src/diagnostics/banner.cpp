#include <diagnostics/banner.hpp>

#include <diagnostics/console.hpp>
#include <iostream>

namespace cstar::diagnostics {

void CompilerBanner::print(std::ostream& out) {
  using console::paint;
  using console::Style;

  out << paint(Style::Yellow)
      << "\n"
      << "________/\\\\\\\\\\\\\\\\\\________/\\\\\\_______        \n"
      << " _____/\\\\\\////////___/\\\\\\_\\/\\\\\\__/\\\\\\_       \n"
      << "  ___/\\\\\\/___________\\////\\\\\\\\\\\\\\\\\\//__      \n"
      << "   __/\\\\\\________________\\////\\\\\\//_____     \n"
      << "    _\\/\\\\\\____Pre-alpha___/\\\\\\\\\\\\\\\\\\____    \n"
      << "     _\\//\\\\\\_____________/\\\\\\///\\\\\\///\\\\\\_   \n"
      << "      __\\///\\\\\\__________\\///__\\/\\\\\\_\\///__  \n"
      << "       ____\\////\\\\\\\\\\\\\\\\\\_______\\///________ \n"
      << "        _______\\/////////___Author: FFC12____\n"
      << paint(Style::Reset)
      << "\n"
      << paint(Style::Red)
      << "[!] "
      << paint(Style::Reset)
      << paint(Style::Cyan)
      << "This version of the compiler is in early development stage "
      << "(pre-alpha). So I don't recommend it to use in production stage yet.\n"
      << paint(Style::Reset)
      << paint(Style::Yellow)
      << "[?] "
      << paint(Style::Reset)
      << paint(Style::Cyan)
      << "If you encountered an unexpected error let me know by "
      << "fatihsaika@gmail.com\n\n"
      << paint(Style::Reset);
}

}  // namespace cstar::diagnostics
