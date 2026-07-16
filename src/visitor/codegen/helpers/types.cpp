#include <visitor/codegen/codegen_private.hpp>

ValuePtr Visitor::visit(AttributeAST &attributeAst) {
  (void)attributeAst;
  return nullptr;
}

ValuePtr Visitor::visit(MacroAST &macroAst) {
  (void)macroAst;
  return nullptr;
}

ValuePtr Visitor::visit(DirectiveAST &directiveAst) {
  (void)directiveAst;
  return nullptr;
}

SymbolAST *Visitor::symbolFromMoveSource(IAST *node) const {
  if (node == nullptr) {
    return nullptr;
  }

  if (node->m_ExprKind == ExprKind::SymbolExpr) {
    return static_cast<SymbolAST *>(node);
  }

  if (node->m_ExprKind == ExprKind::UnaryOp) {
    auto *unary = static_cast<UnaryOpAST *>(node);
    if (unary->m_UnaryOpKind == U_MOVE &&
        unary->m_Node->m_ExprKind == ExprKind::SymbolExpr) {
      return static_cast<SymbolAST *>(unary->m_Node.get());
    }
  }

  return nullptr;
}

llvm::PointerType *GetI8PtrTy() {
  return llvm::PointerType::get(Visitor::Builder->getContext(), 0);
}

llvm::Type *GetType(TypeSpecifier typeSpecifier, size_t indirectLevel,
                           bool isRef);

llvm::StructType *GetSharedPointerTy() {
  auto &ctx = Visitor::Builder->getContext();
  return llvm::StructType::get(ctx, {GetI8PtrTy(), GetI8PtrTy()});
}

std::string DefinedTypeNameFromTypeAst(TypeAST *typeAst) {
  if (typeAst == nullptr || typeAst->typeSpec() != TypeSpecifier::SPEC_DEFINED ||
      typeAst->symbol() == nullptr ||
      typeAst->symbol()->getExprKind() != ExprKind::SymbolExpr) {
    return {};
  }

  auto *symbol = static_cast<SymbolAST *>(typeAst->symbol().get());
  return symbol->name();
}

std::string DefinedTypeNameFromVarAst(VarAST &varAst) {
  if (varAst.typeSpec() != TypeSpecifier::SPEC_DEFINED ||
      varAst.typeName() == nullptr ||
      varAst.typeName()->getExprKind() != ExprKind::SymbolExpr) {
    return {};
  }

  auto *symbol = static_cast<SymbolAST *>(varAst.typeName().get());
  return symbol->name();
}

std::string ValueOperatorMethodName(BinOpKind kind) {
  switch (kind) {
    case B_ADD:
      return "operator+";
    case B_SUB:
      return "operator-";
    case B_MUL:
      return "operator*";
    case B_DIV:
      return "operator/";
    case B_MOD:
      return "operator%";
    case B_EQ:
      return "operator==";
    case B_NEQ:
      return "operator!=";
    case B_LT:
      return "operator<";
    case B_LTEQ:
      return "operator<=";
    case B_GT:
      return "operator>";
    case B_GTEQ:
      return "operator>=";
    default:
      return {};
  }
}

llvm::Type *GetStorageType(TypeSpecifier typeSpecifier,
                                  size_t indirectLevel, bool isUnique,
                                  bool isRef,
                                  const std::string &definedTypeName);

llvm::Type *GetStructFieldLLVMType(const StructFieldInfo &field);

llvm::StructType *GetDefinedStructTy(const std::string &name) {
  auto existing = Visitor::LLVMStructTypes.find(name);
  if (existing != Visitor::LLVMStructTypes.end()) {
    return existing->second;
  }

  auto structIt = Visitor::StructTable.find(name);
  if (structIt == Visitor::StructTable.end()) {
    return nullptr;
  }

  auto *structType =
      llvm::StructType::create(Visitor::Builder->getContext(), name);
  Visitor::LLVMStructTypes[name] = structType;

  std::vector<llvm::Type *> fieldTypes;
  fieldTypes.reserve(structIt->second.fields.size());
  for (const auto &field : structIt->second.fields) {
    fieldTypes.push_back(GetStructFieldLLVMType(field));
  }
  structType->setBody(fieldTypes, false);
  return structType;
}

TypeSpecifier GetDefinedStorageType(const std::string &definedTypeName) {
  auto enumIt = Visitor::EnumTable.find(definedTypeName);
  if (enumIt != Visitor::EnumTable.end()) {
    return enumIt->second.underlyingType;
  }

  return TypeSpecifier::SPEC_DEFINED;
}

TypeSpecifier GetEffectiveStorageType(TypeSpecifier typeSpecifier,
                                             const std::string &definedTypeName) {
  if (typeSpecifier == TypeSpecifier::SPEC_DEFINED) {
    return GetDefinedStorageType(definedTypeName);
  }

  return typeSpecifier;
}

const EnumMemberInfo *FindEnumMember(const std::string &enumName,
                                            const std::string &memberName) {
  auto enumIt = Visitor::EnumTable.find(enumName);
  if (enumIt == Visitor::EnumTable.end()) {
    return nullptr;
  }

  auto memberIt = enumIt->second.memberIndexes.find(memberName);
  if (memberIt == enumIt->second.memberIndexes.end()) {
    return nullptr;
  }

  return &enumIt->second.members[memberIt->second];
}

llvm::Value *ExtractSharedPointerData(llvm::Value *handle);
llvm::Value *ExtractSharedPointerCount(llvm::Value *handle);

bool IsSharedPointerTy(llvm::Type *type) {
  return type == GetSharedPointerTy();
}

bool IsSharedPointerSymbol(const SymbolInfo &symbolInfo) {
  if (symbolInfo.type == TypeSpecifier::SPEC_CHAR ||
      symbolInfo.type == TypeSpecifier::SPEC_UCHAR ||
      symbolInfo.type == TypeSpecifier::SPEC_VOID) {
    return false;
  }
  return symbolInfo.indirectionLevel > 0 && !symbolInfo.isUnique &&
         !symbolInfo.isRef;
}

llvm::Type *GetStorageType(TypeSpecifier typeSpecifier,
                                  size_t indirectLevel, bool isUnique,
                                  bool isRef = false) {
  if ((typeSpecifier == TypeSpecifier::SPEC_CHAR ||
       typeSpecifier == TypeSpecifier::SPEC_UCHAR ||
       typeSpecifier == TypeSpecifier::SPEC_VOID) &&
      indirectLevel > 0) {
    return GetType(typeSpecifier, indirectLevel, isRef);
  }

  if (indirectLevel > 0 && !isUnique && !isRef) {
    return GetSharedPointerTy();
  }

  return GetType(typeSpecifier, indirectLevel, isRef);
}

llvm::Type *GetStorageType(TypeSpecifier typeSpecifier,
                                  size_t indirectLevel, bool isUnique,
                                  bool isRef,
                                  const std::string &definedTypeName) {
  if (typeSpecifier == TypeSpecifier::SPEC_DEFINED && indirectLevel == 0) {
    auto enumIt = Visitor::EnumTable.find(definedTypeName);
    if (enumIt != Visitor::EnumTable.end()) {
      return GetType(enumIt->second.underlyingType, 0, false);
    }

    return GetDefinedStructTy(definedTypeName);
  }

  return GetStorageType(typeSpecifier, indirectLevel, isUnique, isRef);
}

llvm::Type *GetStructFieldLLVMType(const StructFieldInfo &field) {
  return GetStorageType(field.type, field.indirectionLevel, field.isUnique,
                        field.isRef, field.definedTypeName);
}

llvm::Type *GetSymbolLLVMType(const SymbolInfo &symbolInfo) {
  return GetStorageType(symbolInfo.type, symbolInfo.indirectionLevel,
                        symbolInfo.isUnique, symbolInfo.isRef,
                        symbolInfo.definedTypeName);
}

llvm::ArrayType *GetArrayType(llvm::Type *elementType,
                                     const std::vector<ASTNode> &dimensions) {
  size_t length = 1;
  for (const auto &dim : dimensions) {
    auto *scalar = dynamic_cast<ScalarOrLiteralAST *>(dim.get());
    if (scalar == nullptr || scalar->isFloat()) {
      return llvm::ArrayType::get(elementType, length);
    }

    size_t parsedChars = 0;
    uint64_t dimension = 0;
    try {
      dimension = std::stoull(scalar->getValue(), &parsedChars);
    } catch (...) {
      return llvm::ArrayType::get(elementType, length);
    }

    if (parsedChars != scalar->getValue().size() || dimension == 0 ||
        dimension > std::numeric_limits<size_t>::max() ||
        length > std::numeric_limits<size_t>::max() /
                     static_cast<size_t>(dimension)) {
      return llvm::ArrayType::get(elementType, length);
    }

    length *= static_cast<size_t>(dimension);
  }

  return llvm::ArrayType::get(elementType, length);
}
