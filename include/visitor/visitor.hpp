#ifndef VISITOR_HPP
#define VISITOR_HPP
#include <llvm/IR/IRBuilder.h>
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

struct BinOpOrVal {
  bool isBinOp;
  bool isSymbol;
  uintptr_t address;
  std::string value;

  BinOpOrVal(bool isBin, bool isSym, uintptr_t addr, std::string val)
      : isBinOp(isBin), isSymbol(isSym), address(addr), value(val) {}
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
class FixAST;
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
  SymbolInfo m_LastFuncRetTypeInfo;

  // Those are for if we're checking
  // some types which's pointing a address (ptr-type)
  // and if it's a part of binary operation
  bool m_LastBinOp = false;
  bool m_LastBinOpHasAtLeastOnePtr = false;

  bool m_LastReferenced = false;
  bool m_LastParamSymbol = false;
  bool m_LastAssignment = false;
  bool m_LastDereferenced = false;
  size_t m_DereferenceLevel = 1;
  bool m_LastLoopDataSymbol = false;
  bool m_LastLoopIndexSymbol = false;
  bool m_LastCondExpr = false;
  bool m_LastRetExpr = false;
  bool m_LastFixExpr = false;
  std::vector<size_t> m_LastArrayDims;
  size_t m_BinOpTermCount = 0;

  // codegen
  bool m_LastVarDecl = false;
  bool m_LastSigned = false;
  bool m_LastGlobVar = false;
  bool m_LastNegConstant = false;
  bool m_LastInitializerList = false;
  std::string m_LastFuncName;
  llvm::Type* m_LastType = nullptr;
  std::map<std::string, llvm::AllocaInst*> m_LocalVarsOnScope;
  std::map<std::string, llvm::GlobalVariable*> m_GlobalVars;
  //--



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
  static std::unique_ptr<llvm::IRBuilder<>> Builder;
  static std::unique_ptr<llvm::Module> Module;
  static std::unique_ptr<llvm::DataLayout> m_DataLayout;

  explicit Visitor(const std::map<std::string, size_t>& typeTable)
      : m_TypeTable(typeTable) {}

  Visitor(const std::map<std::string, size_t>& typeTable,
          GlobalSymbolInfoList globalSymbolInfoList,
          LocalSymbolInfoList localSymbolInfoList, bool typeCheck)
      : m_TypeTable(typeTable),
        m_TypeChecking(typeCheck),
        m_GlobalSymbolTable(std::move(globalSymbolInfoList)),
        m_LocalSymbolTable(std::move(localSymbolInfoList)) {}

  Visitor(const std::map<std::string, size_t>& typeTable,
          GlobalSymbolInfoList globalSymbolInfoList,
          LocalSymbolInfoList localSymbolInfoList)
      : m_TypeTable(typeTable),
        m_GlobalSymbolTable(std::move(globalSymbolInfoList)),
        m_LocalSymbolTable(std::move(localSymbolInfoList)) {
  }

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
  ValuePtr visit(FixAST& fixAst);

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
  SymbolInfo preVisit(FixAST& fixAst);

  std::vector<SymbolInfo> getSymbolInfoList() { return this->m_SymbolInfos; }
  std::vector<SemanticErrorMessage> getUnknownTypeErrorMessages() {
    return this->m_TypeErrorMessages;
  }
  std::vector<SemanticWarningMessage> getTypeWarningMessages() {
    return this->m_TypeWarningMessages;
  }
  void accumulateIncompatiblePtrErrMesg(const SymbolInfo& symbolInfo,
                                        const std::string& s);
  SymbolInfo getSymbolInfo(const std::string& symbolName);
  bool validateArray(
      IAST& binaryExpr, size_t level,size_t& index);

  void getElementsOfArray(IAST& binaryExpr, std::vector<BinOpOrVal>& vector);
  ValuePtr createBinaryOp(BinaryOpAST& binaryOpAst);
  llvm::BranchInst* createBranch(IAST& ifCond, llvm::BasicBlock* thenBB,
                    llvm::BasicBlock* elseBB, llvm::BasicBlock* mergeBB,
                    bool elif);
};

#endif