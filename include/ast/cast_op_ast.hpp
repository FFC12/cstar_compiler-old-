#ifndef CAST_OP_HPP
#define CAST_OP_HPP
#include <ast/ast.hpp>

enum CastOpKind { C_UNSAFE_CAST, C_CAST, C_AS, C_DYNAMIC_REF_AS, C_DYNAMIC_MOVE_AS };

class CastOpAST : public IAST {
  friend Visitor;
  ASTNode m_Node, m_TypeNode;
  CastOpKind m_CastOpKind;
  bool m_HasTypeAttrib;

 public:
  CastOpAST() = delete;
  CastOpAST(ASTNode node, ASTNode typeNode, CastOpKind castOpKind,
            bool hasTypeAttrib, SemanticLoc& semanticLoc)
      : IAST(semanticLoc),
        m_Node(std::move(node)),
        m_TypeNode(std::move(typeNode)),
        m_CastOpKind(castOpKind),
        m_HasTypeAttrib(hasTypeAttrib) {
    this->m_ASTKind = ASTKind::Expr;
    this->m_ExprKind = ExprKind::CastExpr;
  }

  void debugNode() override {
    switch (m_CastOpKind) {
      case C_UNSAFE_CAST:
        std::cout << "unsafe_cast";
        break;
      case C_CAST:
        std::cout << "cast";
        break;
      case C_AS:
        std::cout << "as";
        break;
      case C_DYNAMIC_REF_AS:
        std::cout << "dynamic ref as";
        break;
      case C_DYNAMIC_MOVE_AS:
        std::cout << "dynamic move as";
        break;
    }

    if (this->m_HasTypeAttrib) {
      std::cout << "<";
      this->m_TypeNode->debugNode();
      std::cout << ">";
    }

    std::cout << "(";
    this->m_Node->debugNode();
    std::cout << ")";
  }

  SymbolInfo acceptBefore(Visitor& visitor) override {
    return visitor.preVisit(*this);
  }

  ValuePtr accept(Visitor& visitor) override { return visitor.visit(*this); }
};

class CastNode : public CastOpAST {
 public:
  CastNode() = delete;
  CastNode(ASTNode node, ASTNode typeNode, CastOpKind castOpKind,
           bool hasTypeAttrib, SemanticLoc& semanticLoc)
      : CastOpAST(std::move(node), std::move(typeNode), castOpKind,
                  hasTypeAttrib, semanticLoc) {}
};

class UnsafeCastNode : public CastOpAST {
 public:
  UnsafeCastNode() = delete;
  UnsafeCastNode(ASTNode node, ASTNode typeNode, CastOpKind castOpKind,
                 bool hasTypeAttrib, SemanticLoc& semanticLoc)
      : CastOpAST(std::move(node), std::move(typeNode), castOpKind,
                  hasTypeAttrib, semanticLoc) {}
};

#endif
