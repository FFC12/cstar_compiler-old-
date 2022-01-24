#ifndef VAR_AST_HPP
#define VAR_AST_HPP
#include <string>
#include <memory>
#include <ast/ast.hpp>
#include <parser/type_specifiers.hpp>
#include <parser/visibility_specifiers.hpp>

class VarAST : public IAST {
    TypeSpecifier m_TypeSpec;
    VisibilitySpecifier m_VisibilitySpec;
    std::string m_Name;
    std::unique_ptr<IAST> m_RHS;

public:
    VarAST(std::string name, std::unique_ptr<IAST> RHS,TypeSpecifier type_spec, VisibilitySpecifier visibility_spec) 
        : m_Name(std::move(name)), 
        m_RHS(std::move(RHS)), 
        m_TypeSpec(std::move(type_spec)), 
        m_VisibilitySpec(std::move(visibility_spec))
    {} 
};


#endif