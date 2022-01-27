#ifndef SCALAR_AST_HPP
#define SCALAR_AST_HPP
#include <ast/ast.hpp>
#include <stdint.h>
#include <string>

// It's just a scalar or constant primitive value like 10, 1.04f
class ScalarAST : public IAST {
  std::string m_Value;
  bool m_IsFloat;
  bool m_IsIntegral;

public:
  ScalarAST() = delete;

  ScalarAST(std::string value, bool isIntegral, bool isFloat) {
    this->m_Value = value;
    this->m_IsFloat = isFloat;
    this->m_IsIntegral = isIntegral;
  }

  std::string getValue() const { return m_Value; }

  bool isFloat() const { return m_IsFloat; }
};

#endif