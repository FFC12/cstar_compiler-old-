#ifndef SYMBOLS_HPP
#define SYMBOLS_HPP
#include <map>
#include <string>
#include <parser/type_specifiers.hpp>

enum SymbolScope { Func, LoopSt, IfSt };

struct TypeCheckerInfo {
  bool isCompatiblePtr = true;
  bool isCompatibleType = true;
  bool isCompatibleCast = true;
  bool isCompatibleVal =  true;
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

  std::string value;
  std::pair<std::string, std::string>
      definedTypenamePair;  // left one is for symbol0 and right one is for
                            // symbol1
  std::string symbolName;
  std::string assocFuncName;  // if it's global, then no assoc. func.
  TypeSpecifier type = TypeSpecifier::SPEC_VOID;
  SymbolScope symbolScope = SymbolScope::IfSt;
  TypeCheckerInfo typeCheckerInfo;

//  SymbolInfo() : typeCheckerInfo() {}
};

#endif  // SYMBOLS_HPP