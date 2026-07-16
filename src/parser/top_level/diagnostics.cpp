#include <parser/parser_private.hpp>

using namespace cstar::parser_private;

void CStarParser::emitDiagnostic(cstar::diagnostics::Severity severity,
                                 cstar::diagnostics::DiagnosticCode code,
                                 std::string mesg, size_t begin, size_t end,
                                 size_t line, bool exitAfter) {
  cstar::diagnostics::DiagnosticRenderer renderer(std::cout);
  renderer.print({
      code,
      severity,
      std::move(mesg),
      this->m_Lexer.getFilepath().get(),
      this->m_Lexer.getBufferView(),
      {begin, end, line},
  });

  if (exitAfter) {
    exit(1);
  }
}

void CStarParser::ParserHint(std::string mesg, TokenInfo tokenInfo) {
  auto posInfo = tokenInfo.getTokenPositionInfo();
  emitDiagnostic(cstar::diagnostics::Severity::Hint,
                 cstar::diagnostics::DiagnosticCode::ParserHint,
                 std::move(mesg), posInfo.begin, posInfo.end, posInfo.line,
                 false);
}

void CStarParser::ParserWarning(std::string mesg, size_t newBegin,
                                size_t newEnd, size_t newLine) {
  emitDiagnostic(cstar::diagnostics::Severity::Warning,
                 cstar::diagnostics::DiagnosticCode::SemanticWarning,
                 std::move(mesg), newBegin, newEnd, newLine, false);
}

void CStarParser::ParserHint(std::string mesg, TokenInfo tokenInfo,
                             size_t new_begin) {
  auto posInfo = tokenInfo.getTokenPositionInfo();
  emitDiagnostic(cstar::diagnostics::Severity::Hint,
                 cstar::diagnostics::DiagnosticCode::ParserHint,
                 std::move(mesg), new_begin, posInfo.end, posInfo.line, false);
}

void CStarParser::ParserError(const std::string& mesg, TokenInfo tokenInfo) {
  auto posInfo = tokenInfo.getTokenPositionInfo();
  emitDiagnostic(cstar::diagnostics::Severity::Error,
                 cstar::diagnostics::DiagnosticCode::ParserSyntax, mesg,
                 posInfo.begin, posInfo.end, posInfo.line, true);
}

void CStarParser::ParserError(std::string mesg, size_t begin, size_t end,
                              size_t line_) {
  ParserError(std::move(mesg), begin, end, line_,
              cstar::diagnostics::DiagnosticCode::SemanticError);
}

void CStarParser::ParserError(std::string mesg, size_t begin, size_t end,
                              size_t line_,
                              cstar::diagnostics::DiagnosticCode code) {
  emitDiagnostic(cstar::diagnostics::Severity::Error,
                 code, std::move(mesg), begin, end, line_, false);
}

void CStarParser::ParserError(std::string mesg, TokenInfo tokenInfo,
                              size_t new_begin) {
  auto posInfo = tokenInfo.getTokenPositionInfo();
  emitDiagnostic(cstar::diagnostics::Severity::Error,
                 cstar::diagnostics::DiagnosticCode::ParserSyntax,
                 std::move(mesg), new_begin, posInfo.end, posInfo.line, true);
}

std::string_view::iterator CStarParser::viewLine(size_t line, size_t& rlbegin,
                                                 size_t& rlend,
                                                 size_t& offset) {
  auto buffer_view = this->m_Lexer.getBufferView();
  size_t lbegin = 0;
  // first line
  if (line == 0) {
    bool oneLineFlag = true;
    for (size_t i = 0; i < buffer_view.size(); i++) {
      if (buffer_view[i] == '\n') {
        oneLineFlag = false;

        lbegin = 0;
        offset = i;
        break;
      }
    }

    // that means we have only one line in the source file.
    // and it's not ending up with '\n'
    if (oneLineFlag) {
      lbegin = 0;
      offset = buffer_view.size();
    }
  } else {
    auto line_walker = 0;
    bool begin_mark = false;

    for (size_t i = 0; i < buffer_view.size(); ++i) {
      if (buffer_view[i] == '\n') {
        line_walker += 1;
      }

      if (line_walker == line && !begin_mark) {
        lbegin = i + 1;
        begin_mark = true;
      } else if (line_walker - 1 == line) {
        rlbegin -= lbegin;
        rlend -= lbegin;
        offset = i - lbegin;
        break;
      } else {
        continue;
      }
    }
  }

  return buffer_view.begin() + lbegin;
}
