#ifndef CSTAR_DIAGNOSTICS_DIAGNOSTIC_HPP
#define CSTAR_DIAGNOSTICS_DIAGNOSTIC_HPP

#include <cstddef>
#include <iosfwd>
#include <string>
#include <string_view>

namespace cstar::diagnostics {

enum class Severity {
  Hint,
  Warning,
  Error,
};

enum class DiagnosticCode {
  ParserHint = 1000,
  ParserSyntax = 1001,
  SemanticWarning = 2000,
  SemanticError = 2001,
  SemanticQualifierMismatch = 2100,
  SemanticInvalidQualifier = 2101,
  SemanticConstAssignment = 2102,
  SemanticConstPtrAssignment = 2103,
  SemanticReadonlyAssignment = 2104,
  SemanticOwnership = 2105,
  DriverUsage = 3000,
  BackendFailure = 4000,
};

struct SourceSpan {
  size_t begin = 0;
  size_t end = 0;
  size_t line = 0;
};

struct Diagnostic {
  DiagnosticCode code = DiagnosticCode::SemanticError;
  Severity severity = Severity::Error;
  std::string message;
  std::string file;
  std::string_view source;
  SourceSpan span;
};

std::string_view severityName(Severity severity);
const char* severityStyle(Severity severity);
std::string_view codeName(DiagnosticCode code);

class DiagnosticRenderer {
 public:
  explicit DiagnosticRenderer(std::ostream& out);

  void print(const Diagnostic& diagnostic) const;

 private:
  std::ostream& m_Out;
};

}  // namespace cstar::diagnostics

#endif
