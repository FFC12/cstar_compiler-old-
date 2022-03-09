#include <codegen/codegen.hpp>

std::unique_ptr<llvm::IRBuilder<>> Visitor::Builder;
std::unique_ptr<llvm::Module> Visitor::Module;

void CStarCodegen::codegen() {
  for (auto& ast : m_AST) {
    if (ast->getASTKind() == ASTKind::Decl) {
      if (ast->getDeclKind() == DeclKind::FuncDecl) {
        Visitor visitor(this->m_DefinedTypes, this->m_GlobalSymbols,
                        this->m_LocalSymbols);

        auto *function = reinterpret_cast<llvm::Function*>(ast->accept(visitor));
        llvm::verifyFunction(*function);

      } else if (ast->getDeclKind() == DeclKind::VarDecl ||
                 ast->getDeclKind() == DeclKind::ImportVarDecl ||
                 ast->getDeclKind() == DeclKind::GlobVarDecl ||
                 ast->getDeclKind() == DeclKind::ExportVarDecl) {
        Visitor visitor(this->m_DefinedTypes, this->m_GlobalSymbols,
                        this->m_LocalSymbols);

        auto variable = ast->accept(visitor);
      }
    }
  }
}