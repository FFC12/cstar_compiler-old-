#ifndef LOOP_STMT_HPP
#define LOOP_STMT_HPP
#include <ast/ast.hpp>
#include <vector>

class LoopStmtAST : public IAST {
  std::vector<ASTNode> m_Scope;
  ASTNode m_Min, m_Max, m_IndexSymbol, m_DataSymbol;
  ASTNode m_IterSymbol;
  ASTNode m_Cond;
  bool m_RangeLoop = false;
  bool m_HasNumericRange = false;
  bool m_Indexable = false;

 public:
  LoopStmtAST(ASTNode cond, ASTNode indexSym, ASTNode dataSym, ASTNode iterSym,
              ASTNode min, ASTNode max, std::vector<ASTNode> scope,
              bool rangeLoop, bool hasNumericRange, bool indexable,
              SemanticLoc& semLoc)
      : IAST(semLoc),
        m_Cond(std::move(cond)),
        m_IndexSymbol(std::move(indexSym)),
        m_DataSymbol(std::move(dataSym)),
        m_IterSymbol(std::move(iterSym)),
        m_Min(std::move(min)),
        m_Max(std::move(max)),
        m_Scope(std::move(scope)),
        m_RangeLoop(rangeLoop),
        m_HasNumericRange(hasNumericRange),
        m_Indexable(indexable) {
    this->m_ASTKind = ASTKind::Stmt;
    this->m_StmtKind = StmtKind::LoopStmt;
  }

  void debugNode() override {
    std::cout << "loop(";
    if (!m_RangeLoop && !m_HasNumericRange && !m_Indexable) {
      if (m_Cond) m_Cond->debugNode();
      std::cout << "){\n";
    }

    if (m_RangeLoop) {
      if (m_Indexable) {
        if (m_IndexSymbol) {
          m_IndexSymbol->debugNode();
          std::cout << ",";
        }
      }

      if (m_DataSymbol) {
        m_DataSymbol->debugNode();
      }
      std::cout << " in ";

      if(m_HasNumericRange) {
        std::cout << "[";
        this->m_Min->debugNode();
        std::cout << ",";
        this->m_Max->debugNode();
        std::cout << "]";
      } else {
        this->m_IterSymbol->debugNode();
      }

      std::cout << "){\n";
    }

    for(auto &s: m_Scope) {
      s->debugNode();
      if(s->getASTKind() != ASTKind::Stmt)
        std::cout << ";\n";
    }

    std::cout << "\n}\n";
  }
};

#endif