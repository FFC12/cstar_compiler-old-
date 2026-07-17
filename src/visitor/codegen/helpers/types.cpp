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

llvm::StructType *GetDynamicTraitObjectTy() {
  auto &ctx = Visitor::Builder->getContext();
  return llvm::StructType::get(ctx, {GetI8PtrTy(), GetI8PtrTy()});
}

llvm::StructType *GetDynamicTraitVTableTy(const std::string &traitName) {
  const auto key = "$dynamic.vtable." + traitName;
  auto existing = Visitor::LLVMStructTypes.find(key);
  if (existing != Visitor::LLVMStructTypes.end()) {
    return existing->second;
  }

  auto traitIt = Visitor::TraitTable.find(traitName);
  if (traitIt == Visitor::TraitTable.end()) {
    return nullptr;
  }

  auto *ptrTy = GetI8PtrTy();
  std::vector<llvm::Type *> fields(traitIt->second.requirements.size(), ptrTy);
  auto *vtableTy = llvm::StructType::create(Visitor::Builder->getContext(),
                                            fields, key);
  Visitor::LLVMStructTypes[key] = vtableTy;
  return vtableTy;
}

int DynamicTraitMethodIndex(const std::string &traitName,
                            const std::string &methodName) {
  auto traitIt = Visitor::TraitTable.find(traitName);
  if (traitIt == Visitor::TraitTable.end()) {
    return -1;
  }

  const auto &requirements = traitIt->second.requirements;
  for (size_t i = 0; i < requirements.size(); ++i) {
    if (requirements[i].name == methodName) {
      return static_cast<int>(i);
    }
  }
  return -1;
}

std::string DynamicTraitDispatchName(const std::string &traitName,
                                     const std::string &methodName) {
  return "$dynamic." + traitName + "." + methodName;
}

bool ParseDynamicTraitDispatchName(const std::string &dispatchName,
                                   std::string &traitName,
                                   std::string &methodName) {
  const std::string prefix = "$dynamic.";
  if (dispatchName.rfind(prefix, 0) != 0) {
    return false;
  }

  const auto rest = dispatchName.substr(prefix.size());
  const auto split = rest.rfind('.');
  if (split == std::string::npos || split == 0 || split + 1 >= rest.size()) {
    return false;
  }

  traitName = rest.substr(0, split);
  methodName = rest.substr(split + 1);
  return true;
}

const FunctionSignature *FindDynamicTraitMethodSignature(
    const std::string &traitName, const std::string &methodName,
    std::string *concreteMethodName) {
  for (const auto &structEntry : Visitor::StructTable) {
    const auto &traits = structEntry.second.traits;
    if (std::find(traits.begin(), traits.end(), traitName) == traits.end()) {
      continue;
    }

    const auto candidate = structEntry.first + "." + methodName;
    auto signatureIt = Visitor::FunctionTable.find(candidate);
    if (signatureIt != Visitor::FunctionTable.end()) {
      if (concreteMethodName != nullptr) {
        *concreteMethodName = candidate;
      }
      return &signatureIt->second;
    }
  }

  return nullptr;
}

llvm::GlobalVariable *GetOrCreateDynamicTraitVTable(
    const std::string &traitName, const std::string &concreteTypeName) {
  const auto globalName = "$dynamic.vtable." + concreteTypeName + "." +
                          traitName;
  if (auto *existing = Visitor::Module->getGlobalVariable(globalName)) {
    return existing;
  }

  auto *vtableTy = GetDynamicTraitVTableTy(traitName);
  auto traitIt = Visitor::TraitTable.find(traitName);
  if (vtableTy == nullptr || traitIt == Visitor::TraitTable.end()) {
    return nullptr;
  }

  std::vector<llvm::Constant *> entries;
  entries.reserve(traitIt->second.requirements.size());
  for (const auto &requirement : traitIt->second.requirements) {
    auto *function = Visitor::Module->getFunction(concreteTypeName + "." +
                                                  requirement.name);
    if (function == nullptr) {
      return nullptr;
    }
    entries.push_back(function);
  }

  auto *initializer = llvm::ConstantStruct::get(vtableTy, entries);
  auto *global = new llvm::GlobalVariable(
      *Visitor::Module, vtableTy, true, llvm::GlobalValue::PrivateLinkage,
      initializer, globalName);
  global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
  return global;
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
  if (symbolInfo.isDynamicTraitObject) {
    return GetDynamicTraitObjectTy();
  }

  return GetStorageType(symbolInfo.type, symbolInfo.indirectionLevel,
                        symbolInfo.isUnique, symbolInfo.isRef,
                        symbolInfo.definedTypeName);
}

llvm::Type *GetStorageType(TypeSpecifier typeSpecifier,
                           size_t indirectLevel, bool isUnique, bool isRef,
                           const SymbolInfo &symbolInfo) {
  if (symbolInfo.isDynamicTraitObject) {
    return GetDynamicTraitObjectTy();
  }

  return GetStorageType(typeSpecifier, indirectLevel, isUnique, isRef,
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
