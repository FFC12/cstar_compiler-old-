#ifndef ASSIGNMENT_AST_HPP
#define ASSIGNMENT_AST_HPP
#include <ast/ast.hpp>
#include <memory>

class AssignmentAST : public IAST {
  ASTNode m_LHS; // VarAST or IdentAST
  ASTNode m_RHS;
public:
  AssignmentAST(ASTNode lhs, ASTNode rhs) 
    : m_LHS(std::move(lhs)), m_RHS(std::move(rhs))
  {}  
};

#endif
