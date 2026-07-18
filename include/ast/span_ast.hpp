#ifndef SPAN_AST_HPP
#define SPAN_AST_HPP

#include <ast/ast.hpp>

class SpanAST : public IAST {
  friend Visitor;
  ASTNode m_Base;
  ASTNode m_Start;
  ASTNode m_End;
  bool m_HasRange;
  bool m_IsUnsafe;

 public:
  SpanAST(ASTNode base, ASTNode start, ASTNode end, bool hasRange,
          bool isUnsafe, SemanticLoc& semanticLoc)
      : IAST(semanticLoc),
        m_Base(std::move(base)),
        m_Start(std::move(start)),
        m_End(std::move(end)),
        m_HasRange(hasRange),
        m_IsUnsafe(isUnsafe) {
    this->m_ASTKind = ASTKind::Expr;
    this->m_ExprKind = ExprKind::SpanExpr;
  }

  void debugNode() override {
    std::cout << (m_IsUnsafe ? "unsafe_span(" : "span ");
    if (m_Base != nullptr) {
      m_Base->debugNode();
    }
    if (m_HasRange) {
      std::cout << "[";
      if (m_Start != nullptr) m_Start->debugNode();
      std::cout << "..";
      if (m_End != nullptr) m_End->debugNode();
      std::cout << "]";
    }
    if (m_IsUnsafe) std::cout << ")";
  }

  SymbolInfo acceptBefore(Visitor& visitor) override {
    return visitor.preVisit(*this);
  }

  ValuePtr accept(Visitor& visitor) override { return visitor.visit(*this); }
};

#endif
