#ifndef TYPE_SPECIFIERS_HPP
#define TYPE_SPECIFIERS_HPP

enum TypeSpecifier {
  SPEC_I8,
  SPEC_I16,
  SPEC_I32,
  SPEC_I64,
  SPEC_INT,
  SPEC_U8,
  SPEC_U16,
  SPEC_U32,
  SPEC_U64,
  SPEC_U128,
  SPEC_UINT,
  SPEC_ISIZE,
  SPEC_USIZE,
  SPEC_F32,
  SPEC_F64,
  SPEC_FLOAT,
  SPEC_CHAR,
  SPEC_UCHAR,
  SPEC_BOOL,
  SPEC_VEC2,
  SPEC_VEC3,
  SPEC_VEC4,
  //MATXxX - MAT
  SPEC_NIL,
  
  //User defined types
  SPEC_DEFINED, //This will be checked in next phase (type checker)
};

enum FunctionReturnType {
  Primitives,
  None,
  Any
};

#endif
