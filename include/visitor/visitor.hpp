#ifndef VISITOR_HPP
#define VISITOR_HPP
#include <llvm/IR/Value.h>

#include <variant>
#include <visitor/symbols.hpp>

class IAST;
class CastOpAST;
class BinaryOpAST;
class AssignmentAST;
class FuncCallAST;
class FuncAST;
class IfStmtAST;
class LoopStmtAST;
class ParamAST;
class RetAST;
class ScalarOrLiteralAST;
class SymbolAST;
class TypeAST;
class UnaryOpAST;
class VarAST;

using ValuePtr = llvm::Value*;

class Visitor {
  friend VarAST;

  //(0,1,2,3,..)
  std::map<std::string,size_t> m_AmbiguousSymbols;

  std::vector<SymbolInfo> m_SymbolInfos;
  size_t m_ScopeLevel = 0;
  size_t m_ScopeId = 0;
  bool m_InsideScope = false;

  void enterScope(bool globScope) {
    if (!globScope) m_ScopeId += 1;
    m_ScopeLevel += (globScope ? 0 : 1);
    m_InsideScope = true;
  }

  void exitScope(bool globScope) {
    assert(m_InsideScope && "Missed the call enterScope");
    m_ScopeLevel -= (globScope ? 0 : 1);
  }

  void scopeHandler(std::unique_ptr<IAST>& node, SymbolScope symbolScope,
                    size_t scopeLevel, size_t scopeId);

 public:
  Visitor() = default;

  ValuePtr visit(VarAST& varAst);
  ValuePtr visit(AssignmentAST& assignmentAst);
  ValuePtr visit(BinaryOpAST& binaryOpAst);
  ValuePtr visit(CastOpAST& castOpAst);
  ValuePtr visit(FuncAST& funcAst);
  ValuePtr visit(FuncCallAST& funcCallAst);
  ValuePtr visit(IfStmtAST& ifStmtAst);
  ValuePtr visit(LoopStmtAST& loopStmtAst);
  ValuePtr visit(ParamAST& paramAst);
  ValuePtr visit(RetAST& retAst);
  ValuePtr visit(UnaryOpAST& unaryOpAst);
  ValuePtr visit(TypeAST& typeAst);
  ValuePtr visit(ScalarOrLiteralAST& scalarAst);
  ValuePtr visit(SymbolAST& symbolAst);

  SymbolInfo previsit(VarAST& varAst);
  SymbolInfo previsit(AssignmentAST& assignmentAst);
  SymbolInfo previsit(BinaryOpAST& binaryOpAst);
  SymbolInfo previsit(CastOpAST& castOpAst);
  SymbolInfo previsit(FuncAST& funcAst);
  SymbolInfo previsit(FuncCallAST& funcCallAst);
  SymbolInfo previsit(IfStmtAST& ifStmtAst);
  SymbolInfo previsit(LoopStmtAST& loopStmtAst);
  SymbolInfo previsit(ParamAST& paramAst);
  SymbolInfo previsit(RetAST& retAst);
  SymbolInfo previsit(UnaryOpAST& unaryOpAst);
  SymbolInfo previsit(TypeAST& typeAst);
  SymbolInfo previsit(ScalarOrLiteralAST& scalarAst);
  SymbolInfo previsit(SymbolAST& symbolAst);

  std::vector<SymbolInfo> getSymbolInfoList() { return this->m_SymbolInfos; }
};

#endif