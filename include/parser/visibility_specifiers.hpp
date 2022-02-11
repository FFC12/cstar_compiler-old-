#ifndef VISIBILITY_SPECIFIERS_HPP
#define VISIBILITY_SPECIFIERS_HPP
/*
  in the C/C++ programs the using of extern is for
  that the symbol is in somehere apart from current source file.
  and also default global variables and functions are automatically
  set as extern. Except in C++ the symbol name may change.

*/


enum VisibilitySpecifier { VIS_EXPORT, VIS_IMPORT, VIS_STATIC, VIS_LOCAL, VIS_DEFAULT};

#endif
