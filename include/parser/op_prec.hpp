#ifndef OP_PREC_HPP
#define OP_PREC_HPP
#include <lexer/lexer.hpp>
#include <unordered_map>
#include <vector>

enum OpType {
  OP_UNARY,
  OP_BINARY,
  OP_CAST,
  OP_VEC,  // Vector
  OP_MAT   // Matrix
};

#include <iostream>

class PrecedenceInfo {
  size_t m_Precedence;
  bool m_IsLeftToRight;

 public:
  PrecedenceInfo() {}

  PrecedenceInfo(size_t prec, bool isLeftToRight)
      : m_Precedence(prec), m_IsLeftToRight(isLeftToRight) {}

  bool operator>(PrecedenceInfo &p) {
    return this->m_Precedence > p.m_Precedence;
  }

  bool operator<(PrecedenceInfo &p) {
    return this->m_Precedence < p.m_Precedence;
  }

  bool operator==(PrecedenceInfo &p) {
    return this->m_Precedence == p.m_Precedence;
  }

  bool isLtr() const { this->m_IsLeftToRight; }

  // Debuggin purpose
  int getPrec() { return this->m_Precedence; }
};

class PrecedenceEntry {
  OpType m_OpType;
  TokenKind m_Token;
  size_t m_Id;
  size_t m_Stride;
  bool m_IsFirst;
  bool m_IsLast;
  bool m_HasTypeAttrib;
  PrecedenceInfo m_PrecedenceInfo;

 public:
  PrecedenceEntry(const TokenKind &kind, const OpType &opType,
                  const PrecedenceInfo &precInfo, size_t stride, size_t id,
                  bool isFirst, bool isLast, bool hasTypeAttrib)
      : m_Token(kind),
        m_OpType(opType),
        m_PrecedenceInfo(precInfo),
        m_Stride(stride),
        m_Id(id),
        m_IsFirst(isFirst),
        m_IsLast(isLast),
        m_HasTypeAttrib(hasTypeAttrib) {}

  bool operator>(PrecedenceEntry &e) {
    return this->m_PrecedenceInfo > e.m_PrecedenceInfo;
  }

  // std::sort calls this one
  bool operator<(PrecedenceEntry &e) {
    bool sameOperatorFlag = false;
    bool isLtrOp = this->m_PrecedenceInfo.isLtr();

    if (this->m_PrecedenceInfo == e.m_PrecedenceInfo) {
      if (this->m_PrecedenceInfo.isLtr() == e.m_PrecedenceInfo.isLtr()) {
        sameOperatorFlag = true;
      }
    }

    if (sameOperatorFlag) {
      return isLtrOp ? false : true;
    } else {
      return this->m_PrecedenceInfo < e.m_PrecedenceInfo;
    }
  }

  bool operator==(PrecedenceEntry &e) {
    return this->m_PrecedenceInfo == e.m_PrecedenceInfo;
  }

  TokenKind entryTokenKind() const noexcept { return this->m_Token; }
  OpType entryOpType() const noexcept { return this->m_OpType; }
  bool entryHasTypeAttrib() const noexcept { return this->m_HasTypeAttrib; }
  size_t entryId() const noexcept { return this->m_Id; }
  size_t entryStride() const noexcept { return this->m_Stride; }
  void decreaseStride() noexcept { this->m_Stride -= 1; }

  // Debuggin purpose
  size_t getPrec() { return this->m_PrecedenceInfo.getPrec(); }

  void print() {
    std::cout << "Op Type: " << std::to_string(m_OpType) << std::endl;
    std::cout << "Op Token Kind: " << std::to_string(m_Token) << std::endl;
    std::cout << "Has Type Attrib: " << std::to_string(m_HasTypeAttrib) << std::endl;
    std::cout << "Stride: " << std::to_string(m_Stride) << std::endl;
    std::cout << "Entry Id: " << m_Id << std::endl;
    std::cout << "Precedence: " << m_PrecedenceInfo.getPrec() << std::endl;
    std::cout << "Is Left to Right: " << m_PrecedenceInfo.isLtr() << std::endl;
  }

};

using PrecedenceInfoTable = std::unordered_map<TokenKind, PrecedenceInfo>;
using PrecedenceTable = std::unordered_map<OpType, PrecedenceInfoTable>;

// Expression Precedence Bucket for the calculation of values
// with associated by operator itself
using OpPrecBucket = std::vector<PrecedenceEntry>;

#endif
