#ifndef LOOP_STMT_HPP
#define LOOP_STMT_HPP
#include <ast/ast.hpp>
#include <vector>

class LoopStmtAST : public IAST {
  std::vector<ASTNode> m_Scope;
  ASTNode m_Min, m_Max;
  ASTNode m_IterSymbol;
  ASTNode m_Cond;
  bool m_RangeLoop = false;
  bool m_HasNumericRange = false;
  bool m_Indexable = false;

 public:
  LoopStmtAST(ASTNode cond, ASTNode iterSym, ASTNode min, ASTNode max,
              std::vector<ASTNode> scope, bool rangeLoop, bool hasNumericRange,
              bool indexable, SemanticLoc& semLoc)
      : IAST(semLoc),
        m_Cond(std::move(cond)),
        m_IterSymbol(std::move(iterSym)),
        m_Min(std::move(min)),
        m_Max(std::move(max)),
        m_Scope(std::move(scope)),
        m_RangeLoop(rangeLoop),
        m_HasNumericRange(hasNumericRange),
        m_Indexable(indexable)
  {
    this->m_ASTKind = ASTKind::Stmt;
    this->m_StmtKind = StmtKind::LoopStmt;
  }
};

#endif