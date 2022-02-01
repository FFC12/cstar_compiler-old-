#ifndef FUNC_CALL_AST_HPP
#define FUNC_CALL_AST_HPP
#include <ast/ast.hpp>
#include <string>

class FuncCallAST : public IAST {
  ASTNode m_FuncSymbol;
  ASTNode m_TypeAttrib;
  ASTNode m_Args;

 public:
  FuncCallAST(ASTNode funcName, ASTNode typeAttrib, ASTNode args)
      : m_FuncSymbol(std::move(funcName)),
        m_TypeAttrib(std::move(typeAttrib)),
        m_Args(std::move(args)) {
    this->m_ASTKind = ASTKind::Expr;
    this->m_ExprKind = ExprKind::FuncCallExpr;
  }

  void debugNode() override {
    m_FuncSymbol->debugNode();
    if(m_TypeAttrib) {
      std::cout << "<";
      m_TypeAttrib->debugNode();
      std::cout << ">";
    }
    std::cout << "(";
    m_Args->debugNode();
    std::cout << ")";
  }
};

#endif