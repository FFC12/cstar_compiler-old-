#ifndef AST_HPP
#define AST_HPP

class IAST {
public:

  //we'll change the return-type by LLVM types 
  //or our self codegenerator type 
//  virtual void codegen();
};
  
  
#include <memory>
using ASTNode =  std::unique_ptr<IAST>; 

#endif
