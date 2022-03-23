#include <codegen/codegen.hpp>

std::unique_ptr<llvm::IRBuilder<>> Visitor::Builder;
std::unique_ptr<llvm::Module> Visitor::Module;

void CStarCodegen::codegen() {
  Visitor visitor(this->m_DefinedTypes);
  for (auto& ast : m_AST) {
    if (ast->getASTKind() == ASTKind::Decl) {
      if (ast->getDeclKind() == DeclKind::FuncDecl) {
        auto* function =
            reinterpret_cast<llvm::Function*>(ast->accept(visitor));
        llvm::verifyFunction(*function);

      } else if (ast->getDeclKind() == DeclKind::VarDecl ||
                 ast->getDeclKind() == DeclKind::ImportVarDecl ||
                 ast->getDeclKind() == DeclKind::GlobVarDecl ||
                 ast->getDeclKind() == DeclKind::ExportVarDecl) {
        auto variable = ast->accept(visitor);
      }
    }
  }
  visitor.finalizeCodegen();
}