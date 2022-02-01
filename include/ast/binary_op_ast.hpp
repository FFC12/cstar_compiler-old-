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
  B_LT,
  B_SHL,
  B_SHR,
  B_EQ, // "=="
  B_COMM  // ','
};

class BinaryOpAST : public IAST {
 protected:
  ASTNode m_LHS, m_RHS;
  BinOpKind m_BinOpKind;
  char m_Op;

 public:
  BinaryOpAST() = default;
  BinaryOpAST(ASTNode lhs, ASTNode rhs, BinOpKind binOpKind, char op)
      : m_Op(op),
        m_LHS(std::move(lhs)),
        m_RHS(std::move(rhs)),
        m_BinOpKind(binOpKind) {
    this->m_ASTKind = ASTKind::Expr;
    this->m_ExprKind = ExprKind::BinOp;
  }

  void debugNode() override {
    this->m_LHS->debugNode();
    std::cout << this->m_Op;
    this->m_RHS->debugNode();
  }
};

class CommaNode : public BinaryOpAST {
 public:
  CommaNode(ASTNode lhs, ASTNode rhs, BinOpKind binOpKind, char op)
      : BinaryOpAST(std::move(lhs), std::move(rhs), binOpKind, op) {}
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

// &, &&, |, ||, ~, >, <, >>, <<, ==, ','
#endif