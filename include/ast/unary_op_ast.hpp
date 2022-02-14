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
  U_DEREF,
  U_REF,
  U_BINNEG,
};

// This is for sign of prefix
// or postfix (to understand that
// is it ++ or --)
enum UnaryNotationSign { S_POS, S_NEG };

class UnaryOpAST : public IAST {
  ASTNode m_Node;
  UnaryOpKind m_UnaryOpKind;
  UnaryNotationSign m_UnaryNotationSign;

 public:
  UnaryOpAST() = default;
  UnaryOpAST(ASTNode node, UnaryOpKind unaryOpKind,
             UnaryNotationSign unaryNotationSign, SemanticLoc& semanticLoc)
      : IAST(semanticLoc),
        m_Node(std::move(node)),
        m_UnaryOpKind(unaryOpKind),
        m_UnaryNotationSign(unaryNotationSign) {
    this->m_ASTKind = ASTKind::Expr;
    this->m_ExprKind = ExprKind::UnaryOp;
  }

  void debugNode() override {
    bool needPar = true;

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
      case U_PREFIX:
      case U_POSTFIX:
        if (this->m_UnaryNotationSign == S_POS) {
          std::cout << "++";
        } else if (this->m_UnaryNotationSign == S_NEG) {
          std::cout << "--";
        } else {
          std::cerr << "Unreacheable!";
        }
        needPar = false;
        break;
      case U_POSITIVE:
      case U_NEGATIVE:
        std::cout << "-";
        needPar = false;
        break;
      case U_NOT:
        std::cout << "!";
        needPar = false;
        break;
      case U_BINNEG:
        std::cout << "~";
        break;
      case U_DEREF:
        std::cout << "deref";
        break;
      case U_REF:
        std::cout << "ref";
        break;
    }

    if (needPar) std::cout << "(";
    this->m_Node->debugNode();
    if (needPar) std::cout << ")";
  }
};

#endif