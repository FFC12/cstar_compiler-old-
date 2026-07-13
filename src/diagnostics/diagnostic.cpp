#include <diagnostics/diagnostic.hpp>

#include <algorithm>
#include <diagnostics/console.hpp>
#include <iomanip>
#include <iostream>
#include <sstream>

namespace cstar::diagnostics {

namespace {

struct DiagnosticCodeInfo {
  DiagnosticCode code;
  std::string_view name;
};

constexpr DiagnosticCodeInfo kDiagnosticCodes[] = {
    {DiagnosticCode::ParserHint, "CST1000"},
    {DiagnosticCode::ParserSyntax, "CST1001"},
    {DiagnosticCode::SemanticWarning, "CST2000"},
    {DiagnosticCode::SemanticError, "CST2001"},
    {DiagnosticCode::DriverUsage, "CST3000"},
    {DiagnosticCode::BackendFailure, "CST4000"},
};

}  // namespace

std::string_view severityName(Severity severity) {
  switch (severity) {
    case Severity::Hint:
      return "hint";
    case Severity::Warning:
      return "warning";
    case Severity::Error:
      return "error";
  }
  return "error";
}

const char* severityStyle(Severity severity) {
  using console::Style;
  switch (severity) {
    case Severity::Hint:
      return Style::BrightWhite;
    case Severity::Warning:
      return Style::Yellow;
    case Severity::Error:
      return Style::Red;
  }
  return Style::Red;
}

std::string_view codeName(DiagnosticCode code) {
  for (const auto& info : kDiagnosticCodes) {
    if (info.code == code) {
      return info.name;
    }
  }
  return "CST0000";
}

DiagnosticRenderer::DiagnosticRenderer(std::ostream& out) : m_Out(out) {}

void DiagnosticRenderer::print(const Diagnostic& diagnostic) const {
  using console::paint;
  using console::Style;

  const auto sourceSize = diagnostic.source.size();
  const auto safeBegin = std::min(diagnostic.span.begin, sourceSize);
  const auto safeEnd = std::min(std::max(diagnostic.span.end, safeBegin + 1),
                                sourceSize == 0 ? size_t{1} : sourceSize);

  size_t lineStart = safeBegin;
  while (lineStart > 0 && diagnostic.source[lineStart - 1] != '\n') {
    --lineStart;
  }

  size_t lineEnd = safeBegin;
  while (lineEnd < sourceSize && diagnostic.source[lineEnd] != '\n' &&
         diagnostic.source[lineEnd] != '\r') {
    ++lineEnd;
  }

  const auto column = safeBegin - lineStart;
  const auto markerEnd = std::max(safeEnd, safeBegin + 1);
  const auto markerWidth = std::max(size_t{1}, markerEnd - safeBegin);
  const auto lineNumber = diagnostic.span.line + 1;
  const auto lineNumberText = std::to_string(lineNumber);

  m_Out << paint(Style::Blue) << diagnostic.file << ':'
        << lineNumber << ':' << (column + 1) << paint(Style::Reset) << ' '
        << paint(severityStyle(diagnostic.severity))
        << severityName(diagnostic.severity) << '[' << codeName(diagnostic.code)
        << ']' << paint(Style::Reset) << ": " << diagnostic.message << "\n\n";

  m_Out << paint(Style::Dim) << std::setw(static_cast<int>(lineNumberText.size()))
        << lineNumber << " | " << paint(Style::Reset)
        << diagnostic.source.substr(lineStart, lineEnd - lineStart) << '\n';

  m_Out << paint(Style::Dim)
        << std::string(lineNumberText.size(), ' ') << " | "
        << paint(Style::Reset) << std::string(column, ' ')
        << paint(severityStyle(diagnostic.severity))
        << std::string(markerWidth, '^') << paint(Style::Reset) << "\n\n";
}

}  // namespace cstar::diagnostics
