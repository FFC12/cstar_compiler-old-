#ifndef SYMBOLS_HPP
#define SYMBOLS_HPP
#include <map>
#include <string>
#include <vector>
#include <cstdint>
#include <parser/type_qualifier.hpp>
#include <parser/type_specifiers.hpp>

enum SymbolScope { Func, LoopSt, IfSt };

struct TypeCheckerInfo {
  bool isCompatiblePtr = true;
  bool isCompatibleSubs = true;
  bool isCompatibleSubsForBinOp = true;
  bool isCompatibleType = true;
  bool isCompatibleCast = true;
  bool isCompatibleVal =  true;
};

struct StructFieldInfo {
  std::string name;
  TypeSpecifier type = TypeSpecifier::SPEC_VOID;
  std::string definedTypeName;
  size_t indirectionLevel = 0;
  bool isUnique = false;
  bool isRef = false;
  bool isNullable = false;
  bool isPublic = false;
};

struct StructInfo {
  std::string name;
  std::vector<StructFieldInfo> fields;
  std::map<std::string, size_t> fieldIndexes;
  std::vector<std::string> traits;
  bool isFromIncludedModule = false;
};

struct TraitRequirementInfo {
  std::string name;
};

struct TraitInfo {
  std::string name;
  std::vector<TraitRequirementInfo> requirements;
  std::string languageItem;
};

struct EnumMemberInfo {
  std::string name;
  uint64_t value = 0;
};

struct EnumInfo {
  std::string name;
  TypeSpecifier underlyingType = TypeSpecifier::SPEC_I32;
  bool isFlags = false;
  std::vector<EnumMemberInfo> members;
  std::map<std::string, size_t> memberIndexes;
};

struct ProtocolTransitionInfo {
  std::string fromState;
  std::string toState;
  std::string methodName;
};

struct ProtocolForbiddenCallInfo {
  std::string methodName;
  std::string forbiddenState;
};

struct ProtocolInfo {
  std::string name;
  std::string targetTypeName;
  std::vector<std::string> states;
  std::string defaultState;
  std::vector<ProtocolTransitionInfo> transitions;
  std::vector<ProtocolForbiddenCallInfo> forbiddenCalls;
  std::vector<ProtocolTransitionInfo> scopeExitTransitions;
  bool isDynamic = false;
};

// This is using for pipelining informations
// also it contains type informations as well.
struct SymbolInfo {
  size_t begin = 0, end = 0, line = 0;
  size_t indirectionLevel = 0;
  size_t scopeLevel = 0;
  size_t scopeId = 0;
  size_t symbolId = 0;
  bool isGlob = false;
  bool isPrimitive = false;
  bool isRef = false;
  bool isReadOnly = false;
  bool isConstPtr = false;
  bool isConstRef = false;
  bool isConstVal = false;
  bool isNeededEval = false;
  bool isSubscriptable = false;
  bool isNeededTypeCheck = false;
  bool isUnique = false;
  bool isParam = false;
  bool isCastable = false;
  bool isNoMove = false;
  bool isPublic = false;
  bool isStatic = false;
  bool isExported = false;
  bool isImported = false;
  bool isNullable = false;
  bool isDynamicTraitObject = false;

  std::string value;
  std::vector<size_t> arrayDimensions;
  // Per-level qualifier metadata. Index 0 is the final target value, and
  // index N is the pointer object at indirection level N. Existing prefix
  // qualifier syntax populates this conservatively until per-level syntax
  // exists in the parser.
  std::vector<TypeQualifier> qualifierLevels;
  std::pair<std::string, std::string>
      definedTypenamePair;  // left one is for symbol0 and right one is for
                            // symbol1
  std::map<size_t, std::string> ptrAliases;
  std::string symbolName;
  std::string definedTypeName;
  std::map<std::string, std::string> protocolStates;
  std::string assocFuncName;  // if it's global, then no assoc. func.
  TypeSpecifier type = TypeSpecifier::SPEC_VOID;
  SymbolScope symbolScope = SymbolScope::IfSt;
  TypeCheckerInfo typeCheckerInfo;

//  SymbolInfo() : typeCheckerInfo() {}
};

struct FunctionSignature {
  SymbolInfo returnType;
  std::vector<SymbolInfo> params;
  bool isVariadic = false;
  bool canThrow = false;
  std::string errorTypeName;
};

using FunctionSignatureTable = std::map<std::string, FunctionSignature>;

#endif  // SYMBOLS_HPP
