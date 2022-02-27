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

struct SemanticWarningMessage {
  std::string message;
  SymbolInfo symbolInfo;
  SemanticWarningMessage(std::string mesg, SymbolInfo symInf)
      : message(std::move(mesg)), symbolInfo(std::move(symInf)) {}
};

struct SymbolInfoEntry {
  std::string symbolName;
  SymbolInfo symbolInfo;

  explicit SymbolInfoEntry(std::string symName, SymbolInfo symInfo)
      : symbolName(std::move(symName)), symbolInfo(std::move(symInfo)) {}

  bool operator==(const SymbolInfo& symbol) const {
    bool flag = false;
    if (symbolInfo.symbolName == symbol.symbolName) {
      if (symbolInfo.scopeLevel <= symbol.scopeLevel) {
        if (symbolInfo.scopeLevel == symbol.scopeLevel &&
            symbolInfo.scopeId != symbol.scopeId) {
          goto not_equal;
        }
        flag = true;
        goto not_equal;
      }
    }
  not_equal:
    return flag;
  }
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
using SymbolInfoList = std::vector<SymbolInfoEntry>;
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
  std::vector<SemanticWarningMessage> m_TypeWarningMessages;
  TypeSpecifier m_ExpectedType = TypeSpecifier::SPEC_VOID;
  TypeSpecifier m_MatchedSymbolType = TypeSpecifier::SPEC_VOID;
  bool m_DefinedTypeFlag = false;
  std::string m_DefinedTypeName;
  SymbolInfoList m_LastScopeSymbols;
  SymbolInfo m_LastSymbolInfo;

  // Those are for if we're checking
  // some types which's pointing a address (ptr-type)
  // and if it's a part of binary operation
  bool m_LastBinOp = false;
  bool m_LastBinOpHasAtLeastOnePtr = false;
  bool m_LastReferenced = false;
  size_t m_BinOpTermCount = 0;

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

  void typeCheckerScopeHandler(std::unique_ptr<IAST>& node);
  bool symbolValidation(std::string& symbolName, SymbolInfo& symbolInfo,
                        SymbolInfo& matchedSymbolInfo);

 public:
  static size_t SymbolId;
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
  std::vector<SemanticWarningMessage> getTypeWarningMessages() {
    return this->m_TypeWarningMessages;
  }
  void accumulateIncompatiblePtrErrMesg(const SymbolInfo& symbolInfo,
                                        const std::string& s);
};

#endif