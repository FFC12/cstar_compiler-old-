# Changelog

## 0.4.0

- Reworked symbolic operator scopes to neutral `meta.operator.*.cstar` scopes so themes do not paint every operator as an error-like red token.
- Removed the semantic token provider to avoid theme-specific function underlines/decorations.
- Kept analyzer diagnostics/completion active while leaving highlighting to TextMate grammar.
- Added TextMate patterns for variable declarations and assignment LHS names.
- Improved precedence/operator-heavy highlighting for ternary, shifts, comparisons, nullable pointers, and allocation syntax.

## 0.3.0

- Added bundled C* static analyzer server.
- Added automatic analyzer startup for `.cstar` files.
- Added diagnostics bridge through `textDocument/publishDiagnostics`.
- Added LSP completion bridge for keywords, types, symbols, members, module aliases, and snippets.
- Added `cstar.enableAnalyzer`, analyzer path, node path, and trace settings.
- Added `C*: Restart Static Analyzer` command.

## 0.2.0

- Added production package metadata and extension icon.
- Added `.cstar` file icon.
- Added C* snippets for common core constructs.
- Expanded grammar for struct, trait, protocol, enum, tagged, macro, attribute, lifecycle, ownership, native import, and metaprogramming proposal surfaces.
- Improved language configuration with indentation and word pattern rules.

## 0.1.0

- Initial `.cstar` TextMate grammar and language configuration.
