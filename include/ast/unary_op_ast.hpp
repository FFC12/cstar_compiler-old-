#ifndef UNARY_OP_AST_HPP
#define UNARY_OP_AST_HPP
#include <ast/ast.hpp>

enum UnaryOpKind {
  U_SIZEOF,
  U_TYPEOF,
  U_MOVE,
  U_PREFIX,
  U_POSTFIX,
  U_POSITIVE,
  U_NEGATIVE,
  U_NOT,
  U_XOR,
  U_DEREF,
  U_REF
};

class UnaryOpAST : public IAST {
  ASTNode m_Node;
  UnaryOpKind m_UnaryOpKind;

 public:
  UnaryOpAST() = default;
  UnaryOpAST(ASTNode node, UnaryOpKind unaryOpKind)
      : m_Node(std::move(node)), m_UnaryOpKind(unaryOpKind) {
    this->m_ASTKind = ASTKind::Expr;
    this->m_ExprKind = ExprKind::UnaryOp;
  }

  void debugNode() override {
    switch (m_UnaryOpKind) {
      case U_SIZEOF:
        std::cout << "sizeof";
        break;
      case U_TYPEOF:
        std::cout << "typeof";
        break;
      case U_MOVE:
        std::cout << "move";
        break;
    }

    std::cout << "(";
    this->m_Node->debugNode();
    std::cout << ")";
  }
};

#endif