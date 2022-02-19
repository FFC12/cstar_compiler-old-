#ifndef SYMBOLS_HPP
#define SYMBOLS_HPP
#include <map>
#include <string>
#include <parser/type_specifiers.hpp>

enum SymbolScope { Func, LoopSt, IfSt };

// This is using for pipelining informations
// also it contains type informations as well.
struct SymbolInfo {
  size_t begin, end, line;
  size_t indirectionLevel;
  size_t scopeLevel;
  size_t scopeId;
  bool isGlob;
  bool isPrimitive;
  bool isConstVal;
  bool isConstPtr;
  bool isConstRef;
  bool isNeededEval;
  bool isSubscriptable;
  bool isNeededTypeCheck;
  std::string value;
  std::pair<std::string, std::string>
      definedTypenamePair;  // left one is for symbol0 and right one is for
                            // symbol1
  std::string symbolName;
  std::string assocFuncName;  // if it's global, then no assoc. func.
  TypeSpecifier type;
  SymbolScope symbolScope;
};

#endif  // SYMBOLS_HPP