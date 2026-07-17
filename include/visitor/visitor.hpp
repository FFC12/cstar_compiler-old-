#ifndef VISITOR_HPP
#define VISITOR_HPP
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Value.h>

#include <diagnostics/diagnostic.hpp>
#include <set>
#include <stack>
#include <utility>
#include <visitor/symbols.hpp>

struct SemanticErrorMessage {
  std::string message;
  SymbolInfo symbolInfo;
  cstar::diagnostics::DiagnosticCode code =
      cstar::diagnostics::DiagnosticCode::SemanticError;

  SemanticErrorMessage(std::string mesg, SymbolInfo symInf)
      : message(std::move(mesg)), symbolInfo(std::move(symInf)) {}
  SemanticErrorMessage(std::string mesg, SymbolInfo symInf,
                       cstar::diagnostics::DiagnosticCode diagnosticCode)
      : message(std::move(mesg)),
        symbolInfo(std::move(symInf)),
        code(diagnosticCode) {}
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
class AttributeAST;
class CastOpAST;
class BinaryOpAST;
class AssignmentAST;
class FuncCallAST;
class FuncAST;
class IfStmtAST;
class LoopStmtAST;
class MacroAST;
class DirectiveAST;
class OptionStmtAST;
class NewAST;
class BreakStmtAST;
class ContinueStmtAST;
class DropStmtAST;
class EnumAST;
class ParamAST;
class RetAST;
class ScalarOrLiteralAST;
class SymbolAST;
class StructAST;
class TraitAST;
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


  std::vector<SemanticErrorMessage> m_TypeErrorMessages;
  std::vector<SymbolInfo> m_SymbolInfos;
  size_t m_ScopeLevel = 0;
  size_t m_ScopeId = 0;
  size_t m_SymbolId = 0;
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
  bool m_CurrentFunctionIsStatic = false;
  std::string m_CurrentStructMethodOwner;

  // Those are for if we're checking
  // some types which's pointing a address (ptr-type)
  // and if it's a part of binary operation
  bool m_LastBinOp = false;
  bool m_LastBinOpHasAtLeastOnePtr = false;

  bool m_LastReferenced = false;
  bool m_LastSubscriptable = false;
  bool m_LastParamSymbol = false;
  bool m_LastAssignment = false;
  bool m_LastDereferenced = false;
  size_t m_DereferenceLevel = 1;
  bool m_LastLoop = false;
  bool m_LastLoopDataSymbol = false;
  bool m_LastLoopIndexSymbol = false;
  bool m_LastLoopIter = false;
  bool m_LastCondExpr = false;
  bool m_LastRetExpr = false;
  bool m_LastFixExpr = false;
  std::set<std::string> m_NonNullFlowSymbols;
  std::set<std::string> m_MovedUniqueSymbols;
  std::set<std::string> m_DroppedSemanticSymbols;
  std::vector<size_t> m_LastArrayDims;
  size_t m_BinOpTermCount = 0;

  // codegen
  bool m_LastVarDecl = false;
  bool m_LastSigned = false;
  bool m_LastGlobVar = false;
  bool m_LastNegConstant = false;
  bool m_LastArrayIndex = false;
  bool m_LastInitializerList = false;
  std::string m_LastFuncName;
  std::string m_LastVarName;
  std::vector<llvm::Value*> m_Indices{};
  std::vector<std::pair<std::string,bool>> m_IndicesAsStr{};
  llvm::Type* m_LastType = nullptr;
  std::string m_LastDefinedTypeName;
  std::map<std::string, llvm::AllocaInst*> m_LocalVarsOnScope;
  std::map<std::string, llvm::Type*> m_ReferenceParamValueTypes;
  std::map<std::string, llvm::Type*> m_ArrayParamValueTypes;
  std::map<std::string, llvm::GlobalVariable*> m_GlobalVars;
  std::map<std::string, llvm::Value*> m_SharedPointerRefCounts;
  std::vector<SymbolInfo> m_ScopeDestructors;
  std::vector<SymbolInfo> m_ScopeSharedPointerReleases;
  std::set<std::string> m_CodegenDroppedSymbols;
  struct HeapAllocationInfo {
    std::string typeName;
    std::string allocatorSymbol;
    std::string allocatorTypeName;
    bool isShared = false;
  };
  std::map<std::string, HeapAllocationInfo> m_HeapAllocations;
  std::vector<llvm::BasicBlock*> m_LoopBreakTargets;
  std::vector<llvm::BasicBlock*> m_LoopContinueTargets;
  std::vector<llvm::StringRef> m_GlobaInitVarFunc;
  static llvm::BasicBlock* MainFuncBB;
  static llvm::GlobalVariable* LastGlobVarRef;
  //--

  struct FunctionParamLayout {
    std::vector<llvm::Type*> irTypes;
    std::vector<llvm::Type*> valueTypes;
    std::vector<std::string> names;
    std::vector<bool> isReference;
    std::vector<bool> isArray;

    bool hasParams() const { return !irTypes.empty(); }
  };

  FunctionParamLayout buildFunctionParamLayout(FuncAST& funcAst,
                                               bool nativeAbi = false);

  std::stack<size_t> m_SymbolIds;

  void enterScope(bool globScope) {
    if (!globScope) {
      m_ScopeId += 1;
    }
    m_ScopeLevel += (globScope ? 0 : 1);
    m_SymbolIds.push(m_SymbolId);
    m_SymbolId = 0;
    m_InsideScope = true;
  }

  void exitScope(bool globScope) {
    assert(m_InsideScope && "Missed the call enterScope");
    m_ScopeLevel -= (globScope ? 0 : 1);
    m_SymbolId = m_SymbolIds.top();
    m_SymbolIds.pop();
  }
  bool symbolValidation(std::string& symbolName, SymbolInfo& symbolInfo,
                        SymbolInfo& matchedSymbol, bool noError = false);

  void scopeHandler(std::unique_ptr<IAST>& node, SymbolScope symbolScope,
                    size_t scopeLevel, size_t scopeId);

  void typeCheckerScopeHandler(std::unique_ptr<IAST>& node);
  SymbolAST* symbolFromMoveSource(IAST* node) const;
  bool tryGetConstantIntegerLiteral(IAST* node, int64_t& value) const;
  IAST* methodCallReceiver(IAST* node) const;
  bool constructorInitializer(VarAST& varAst, std::string& constructorName);
  std::string resolveFunctionCallName(IAST* node, SymbolInfo& symbolInfo,
                                      bool emitDiagnostics);
  void registerScopeDestructor(const SymbolInfo& symbolInfo);
  void registerScopeSharedPointerRelease(const SymbolInfo& symbolInfo);
  ValuePtr emitDropForSymbol(const std::string& symbolName,
                             bool markDropped);
  void emitScopeExitDestructors();

 public:
  // These are from pass0;
  static GlobalSymbolInfoList GlobalSymbolTable;
  static LocalSymbolInfoList LocalSymbolTable;
  static FunctionSignatureTable FunctionTable;
  static std::map<std::string, StructInfo> StructTable;
  static std::map<std::string, TraitInfo> TraitTable;
  static std::map<std::string, EnumInfo> EnumTable;
  static std::map<std::string, llvm::StructType*> LLVMStructTypes;
  static std::set<std::string> ModuleAliases;
  static size_t SymbolId, ScopeId;
  static std::unique_ptr<llvm::IRBuilder<>> Builder;
  static std::unique_ptr<llvm::Module> Module;
  static std::unique_ptr<llvm::DataLayout> m_DataLayout;

  explicit Visitor(const std::map<std::string, size_t>& typeTable)
      : m_TypeTable(typeTable) {}

  Visitor(const std::map<std::string, size_t>& typeTable, bool typeCheck)
      : m_TypeTable(typeTable), m_TypeChecking(typeCheck) {}

  ValuePtr visit(VarAST& varAst);
  ValuePtr visit(AttributeAST& attributeAst);
  ValuePtr visit(MacroAST& macroAst);
  ValuePtr visit(DirectiveAST& directiveAst);
  ValuePtr visit(AssignmentAST& assignmentAst);
  ValuePtr visit(BinaryOpAST& binaryOpAst);
  ValuePtr visit(CastOpAST& castOpAst);
  ValuePtr visit(FuncAST& funcAst);
  ValuePtr visit(FuncCallAST& funcCallAst);
  ValuePtr visit(IfStmtAST& ifStmtAst);
  ValuePtr visit(LoopStmtAST& loopStmtAst);
  ValuePtr visit(OptionStmtAST& optionStmtAst);
  ValuePtr visit(NewAST& newAst);
  ValuePtr visit(BreakStmtAST& breakStmtAst);
  ValuePtr visit(ContinueStmtAST& continueStmtAst);
  ValuePtr visit(DropStmtAST& dropStmtAst);
  ValuePtr visit(ParamAST& paramAst);
  ValuePtr visit(RetAST& retAst);
  ValuePtr visit(UnaryOpAST& unaryOpAst);
  ValuePtr visit(TypeAST& typeAst);
  ValuePtr visit(ScalarOrLiteralAST& scalarAst);
  ValuePtr visit(SymbolAST& symbolAst);
  ValuePtr visit(StructAST& structAst);
  ValuePtr visit(TraitAST& traitAst);
  ValuePtr visit(EnumAST& enumAst);
  ValuePtr visit(FixAST& fixAst);
  llvm::Function* declareFunction(FuncAST& funcAst);

  SymbolInfo preVisit(VarAST& varAst);
  SymbolInfo preVisit(AttributeAST& attributeAst);
  SymbolInfo preVisit(MacroAST& macroAst);
  SymbolInfo preVisit(DirectiveAST& directiveAst);
  SymbolInfo preVisit(AssignmentAST& assignmentAst);
  SymbolInfo preVisit(BinaryOpAST& binaryOpAst);
  SymbolInfo preVisit(CastOpAST& castOpAst);
  SymbolInfo preVisit(FuncAST& funcAst);
  SymbolInfo preVisit(FuncCallAST& funcCallAst);
  SymbolInfo preVisit(IfStmtAST& ifStmtAst);
  SymbolInfo preVisit(LoopStmtAST& loopStmtAst);
  SymbolInfo preVisit(OptionStmtAST& optionStmtAst);
  SymbolInfo preVisit(NewAST& newAst);
  SymbolInfo preVisit(BreakStmtAST& breakStmtAst);
  SymbolInfo preVisit(ContinueStmtAST& continueStmtAst);
  SymbolInfo preVisit(DropStmtAST& dropStmtAst);
  SymbolInfo preVisit(ParamAST& paramAst);
  SymbolInfo preVisit(RetAST& retAst);
  SymbolInfo preVisit(UnaryOpAST& unaryOpAst);
  SymbolInfo preVisit(TypeAST& typeAst);
  SymbolInfo preVisit(ScalarOrLiteralAST& scalarAst);
  SymbolInfo preVisit(SymbolAST& symbolAst);
  SymbolInfo preVisit(StructAST& structAst);
  SymbolInfo preVisit(TraitAST& traitAst);
  SymbolInfo preVisit(EnumAST& enumAst);
  SymbolInfo preVisit(FixAST& fixAst);

  void finalizeCodegen();

  std::vector<SymbolInfo> getSymbolInfoList() { return this->m_SymbolInfos; }
  std::vector<SemanticErrorMessage> getUnknownTypeErrorMessages() {
    return this->m_TypeErrorMessages;
  }
  std::vector<SemanticWarningMessage> getTypeWarningMessages() {
    return this->m_TypeWarningMessages;
  }
  void accumulateIncompatiblePtrErrMesg(const SymbolInfo& symbolInfo,
                                        const std::string& s = "");
  SymbolInfo getSymbolInfo(const std::string& symbolName);
  bool validateArray(IAST& binaryExpr, size_t level, size_t& index);

  void getElementsOfArray(IAST& binaryExpr, std::vector<BinOpOrVal>& vector);
  ValuePtr createBinaryOp(BinaryOpAST& binaryOpAst);
  llvm::BranchInst* createBranch(IAST& ifCond, llvm::BasicBlock* thenBB,
                                 llvm::BasicBlock* elseBB,
                                 llvm::BasicBlock* mergeBB, bool elif);
  void scopeHandler(std::unique_ptr<IAST>& node, SymbolScope symbolScope);
  size_t getIndexesOfArray(IAST& binaryExpr);
  size_t getIndexesOfArray(BinaryOpAST& binaryExpr);
  size_t m_LastArrayIndexCount;
};

#endif
