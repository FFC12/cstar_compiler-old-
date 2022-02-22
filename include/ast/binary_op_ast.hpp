#ifndef BINARY_OP_AST_HPP
#define BINARY_OP_AST_HPP
#include <ast/ast.hpp>
#include <iostream>

enum BinOpKind {
  B_ADD,
  B_SUB,
  B_MUL,
  B_DIV,
  B_MOD,
  B_AND,
  B_LAND,
  B_OR,
  B_LOR,
  B_XOR,
  B_GT,
  B_GTEQ,
  B_LT,
  B_LTEQ,
  B_SHL,
  B_SHR,
  B_EQ,    // "=="
  B_TER,   // Ternary cond op
  B_ARRS,  // Array subscript
  B_COMM,  // ','
  B_DOT,   // a.b
  B_ARW,   // a->b
  B_CCOL,  // ::
  B_MARRS  // [:]
};

class BinaryOpAST : public IAST {
  friend Visitor;
  ASTNode m_LHS, m_RHS, m_Extra;
  BinOpKind m_BinOpKind;
  std::string m_Op;

 public:
  BinaryOpAST() = default;
  BinaryOpAST(ASTNode lhs, ASTNode rhs, ASTNode extra, BinOpKind binOpKind,
              std::string& op, SemanticLoc& semanticLoc)
      : IAST(semanticLoc),
        m_Op(op),
        m_LHS(std::move(lhs)),
        m_RHS(std::move(rhs)),
        m_Extra(std::move(extra)),
        m_BinOpKind(binOpKind) {
    this->m_ASTKind = ASTKind::Expr;
    this->m_ExprKind = ExprKind::BinOp;
  }

  void debugNode() override {
    if (m_LHS != nullptr) {
      this->m_LHS->debugNode();
      if (m_RHS != nullptr) std::cout << this->m_Op;
    }

    if (m_RHS != nullptr) this->m_RHS->debugNode();

    if (m_Extra != nullptr && m_BinOpKind == B_TER) {
      std::cout << ":";
      this->m_Extra->debugNode();
    }

    if (m_BinOpKind == B_ARRS) {
      if (m_RHS == nullptr) std::cout << "[";
      std::cout << "]";
    }
  }

  SymbolInfo acceptBefore(Visitor& visitor) override {
    return visitor.preVisit(*this);
  }

  ValuePtr accept(Visitor& visitor) override { return visitor.visit(*this); }
};

/*
class TernaryOpAST : public IAST {
 protected:
  ASTNode m_Cond, m_b0, m_b1;
  BinOpKind m_BinOpKind;
  char m_Op;

 public:
  TernaryOpAST() = default;
  TernaryOpAST(ASTNode cond, ASTNode b0, ASTNode b1, BinOpKind binOpKind,
               char op)
      : m_Op(op),
        m_Cond(std::move(cond)),
        m_b0(std::move(b0)),
        m_b1(std::move(b1)),
        m_BinOpKind(binOpKind) {
    this->m_ASTKind = ASTKind::Expr;
    this->m_ExprKind = ExprKind::TernaryOp;
  }

  void debugNode() override {
    this->m_Cond->debugNode();
    std::cout << this->m_Op;
    this->m_b0->debugNode();
    std::cout << ":";
    this->m_b1->debugNode();
  }
};

class SubscriptOpAST : public BinaryOpAST {
 protected:
  BinOpKind m_BinOpKind;
  char m_Op;

 public:
  SubscriptOpAST() = default;
  SubscriptOpAST(ASTNode lhs, ASTNode rhs, BinOpKind binOpKind, char op)
      : BinaryOpAST(std::move(lhs), std::move(rhs), binOpKind, op),
        m_BinOpKind(binOpKind) {
    //    this->m_SemLoc = SemanticLoc(0,0,0);
    this->m_ASTKind = ASTKind::Expr;
    this->m_ExprKind = ExprKind::BinOp;
  }

  void debugNode() override {
    this->m_LHS->debugNode();
    std::cout << this->m_Op;
    this->m_RHS->debugNode();
  }
};

class AdditionNode : public BinaryOpAST {
 public:
  AdditionNode(ASTNode lhs, ASTNode rhs, BinOpKind binOpKind, char op)
      : BinaryOpAST(std::move(lhs), std::move(rhs), binOpKind, op) {}
};

class SubstractionNode : public BinaryOpAST {
 public:
  SubstractionNode(ASTNode lhs, ASTNode rhs, BinOpKind binOpKind, char op)
      : BinaryOpAST(std::move(lhs), std::move(rhs), binOpKind, op) {}
};

class MultiplicationNode : public BinaryOpAST {
 public:
  MultiplicationNode(ASTNode lhs, ASTNode rhs, BinOpKind binOpKind, char op)
      : BinaryOpAST(std::move(lhs), std::move(rhs), binOpKind, op) {}
};

class DivisionNode : public BinaryOpAST {
 public:
  DivisionNode(ASTNode lhs, ASTNode rhs, BinOpKind binOpKind, char op)
      : BinaryOpAST(std::move(lhs), std::move(rhs), binOpKind, op) {}
};

class ModuloNode : public BinaryOpAST {
 public:
  ModuloNode(ASTNode lhs, ASTNode rhs, BinOpKind binOpKind, char op)
      : BinaryOpAST(std::move(lhs), std::move(rhs), binOpKind, op) {}
};
*/
// &, &&, |, ||, ~, >, <, >>, <<, ==, ','
#endif