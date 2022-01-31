#ifndef CAST_OP_HPP
#define CAST_OP_HPP
#include <ast/ast.hpp>

enum CastOpKind { C_UNSAFE_CAST, C_CAST, C_AS };

class CastOpAST : public IAST {
  ASTNode m_Node, m_TypeNode;
  CastOpKind m_CastOpKind;
  bool m_HasTypeAttrib;

 public:
  CastOpAST() = default;
  CastOpAST(ASTNode node, ASTNode typeNode, CastOpKind castOpKind,
            bool hasTypeAttrib)
      : m_Node(std::move(node)),
        m_TypeNode(std::move(typeNode)),
        m_CastOpKind(castOpKind),
        m_HasTypeAttrib(hasTypeAttrib) 
        {
          
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
    }

    if(this->m_HasTypeAttrib) {
      std::cout << "<";
      this->m_TypeNode->debugNode();
      std::cout << ">";
    }

    std::cout << "(";
    this->m_Node->debugNode();
    std::cout << ")";
  }
};

class CastNode : public CastOpAST {
 public:
  CastNode() = default;
  CastNode(ASTNode node, ASTNode typeNode, CastOpKind castOpKind,
           bool hasTypeAttrib)
      : CastOpAST(std::move(node), std::move(typeNode), castOpKind,
                  hasTypeAttrib) {}
};

class UnsafeCastNode : public CastOpAST {
 public:
  UnsafeCastNode() = default;
  UnsafeCastNode(ASTNode node, ASTNode typeNode, CastOpKind castOpKind,
                 bool hasTypeAttrib)
      : CastOpAST(std::move(node), std::move(typeNode), castOpKind,
                  hasTypeAttrib) {}
};

#endif