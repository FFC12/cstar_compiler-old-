#ifndef SYMBOLS_HPP
#define SYMBOLS_HPP
#include <map>
#include <parser/type_specifiers.hpp>
#include <string>

enum SymbolScope {
  Func,
  LoopSt,
  IfSt
};


// This is using for pipelining informations
// also it contains type informations as well.
struct SymbolInfo {
  size_t begin, end, line;
  size_t indirectionLevel;
  size_t scopeLevel;
  size_t scopeId;
  bool isGlob;
  bool isIntegral;
  bool isFloat;
  bool isConstVal;
  bool isConstPtr;
  bool isConstRef;
  bool isNeededEval;
  bool isNeededTypeCheck;
  std::string value;
  std::string symbolName;
  std::string assocFuncName;  // if it's global, then no assoc. func.
  TypeSpecifier type;
  SymbolScope symbolScope;
};

#endif  // SYMBOLS_HPP