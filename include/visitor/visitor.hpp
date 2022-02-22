#ifndef VISITOR_HPP
#define VISITOR_HPP
#include <llvm/IR/Value.h>

#include <utility>
#include <visitor/symbols.hpp>

struct SemanticErrorMessage {
  std::string message;
  SymbolInfo symbolInfo;

  SemanticErrorMessage(std::string mesg, SymbolInfo symInf)
      : message(std::move(mesg)), symbolInfo(std::move(symInf)) {}
};

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
using SymbolInfoList = std::multimap<std::string, SymbolInfo>;
using LocalSymbolInfoList = std::map<std::string, SymbolInfoList>;
using GlobalSymbolInfoList = SymbolInfoList;

class Visitor {
  friend VarAST;

  // These are from pass0;
  GlobalSymbolInfoList m_GlobalSymbolTable;
  LocalSymbolInfoList m_LocalSymbolTable;

  std::vector<SemanticErrorMessage> m_TypeErrorMessages;
  std::vector<SymbolInfo> m_SymbolInfos;
  size_t m_ScopeLevel = 0;
  size_t m_ScopeId = 0;
  bool m_InsideScope = false;
  bool m_TypeChecking = false;

  const std::map<std::string, size_t>& m_TypeTable;

  // This is a state for expressions.
  TypeSpecifier m_ExpectedType = TypeSpecifier::SPEC_VOID;
  bool m_DefinedTypeFlag = false;
  std::string m_DefinedTypeName;
  std::string m_LastSymbolName;
  size_t m_LastSymbolIndirectionLevel = 0;
  bool m_LastSymbolIsRef;
  bool m_LastSymbolIsUniqPtr;
  bool m_LastSymbolRO;
  bool m_LastSymbolConstPtr;
  bool m_LastSymbolConstRef;
  bool m_LastSymbolConst;
  std::map<std::string, std::vector<SymbolInfo>>
      m_IncompleteSymbolsOrConstantsOfRHS;

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
  explicit Visitor(const std::map<std::string, size_t>& typeTable)
      : m_TypeTable(typeTable) {}

  Visitor(const std::map<std::string, size_t>& typeTable,
          GlobalSymbolInfoList globalSymbolInfoList,
          LocalSymbolInfoList localSymbolInfoList, bool typeCheck)
      : m_TypeTable(typeTable),
        m_TypeChecking(typeCheck),
        m_GlobalSymbolTable(std::move(globalSymbolInfoList)),
        m_LocalSymbolTable(std::move(localSymbolInfoList)) {}

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

  SymbolInfo preVisit(VarAST& varAst);
  SymbolInfo preVisit(AssignmentAST& assignmentAst);
  SymbolInfo preVisit(BinaryOpAST& binaryOpAst);
  SymbolInfo preVisit(CastOpAST& castOpAst);
  SymbolInfo preVisit(FuncAST& funcAst);
  SymbolInfo preVisit(FuncCallAST& funcCallAst);
  SymbolInfo preVisit(IfStmtAST& ifStmtAst);
  SymbolInfo preVisit(LoopStmtAST& loopStmtAst);
  SymbolInfo preVisit(ParamAST& paramAst);
  SymbolInfo preVisit(RetAST& retAst);
  SymbolInfo preVisit(UnaryOpAST& unaryOpAst);
  SymbolInfo preVisit(TypeAST& typeAst);
  SymbolInfo preVisit(ScalarOrLiteralAST& scalarAst);
  SymbolInfo preVisit(SymbolAST& symbolAst);

  std::vector<SymbolInfo> getSymbolInfoList() { return this->m_SymbolInfos; }
  std::vector<SemanticErrorMessage> getUnknownTypeErrorMessages() {
    return this->m_TypeErrorMessages;
  }
};

#endif