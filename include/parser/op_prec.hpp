#ifndef OP_PREC_HPP
#define OP_PREC_HPP
#include <unordered_map>
#include <vector>
#include <lexer/lexer.hpp>  

enum OpType {
	OP_UNARY,
	OP_BINARY,
	OP_CAST,
	OP_VEC, //Vector
	OP_MAT  //Matrix
};

class PrecedenceInfo {
	int m_Precedence;
	bool m_IsLeftToRight;

public:
	PrecedenceInfo() {}

	PrecedenceInfo(int prec, bool isLeftToRight) 
			: m_Precedence(prec), m_IsLeftToRight(isLeftToRight)
	{}

	bool operator >(PrecedenceInfo& p) {
		return this->m_Precedence > p.m_Precedence;
	}

	bool operator <(PrecedenceInfo& p) {
		return this->m_Precedence < p.m_Precedence;
	}

	bool operator ==(PrecedenceInfo& p) {
		return this->m_Precedence == p.m_Precedence;
	}

  bool isLtr() const {
    this->m_IsLeftToRight;
  }
};

class PrecedenceEntry {
  TokenKind m_Token;
  size_t m_Id;
  bool m_IsFirst;
  bool m_IsLast;
  PrecedenceInfo m_PrecedenceInfo;				
	
public:
  PrecedenceEntry(const TokenKind& kind, const PrecedenceInfo& precInfo, size_t id,bool isFirst, bool isLast) 
    : m_Token(kind), m_PrecedenceInfo(precInfo), m_Id(id),  m_IsFirst(isFirst), m_IsLast(isLast)
  {
  }

  bool operator >(PrecedenceEntry& e) {
    return this->m_PrecedenceInfo > e.m_PrecedenceInfo;
  }

  //std::sort calls this one
  bool operator <(PrecedenceEntry& e) {
    bool sameOperatorFlag = false;
    bool isLtrOp = this->m_PrecedenceInfo.isLtr();

    if(this->m_PrecedenceInfo == e.m_PrecedenceInfo) {
      if(this->m_PrecedenceInfo.isLtr() == e.m_PrecedenceInfo.isLtr()){
        sameOperatorFlag = true;
      }
    }

    if(sameOperatorFlag) {
      return isLtrOp ? false : true;
    } else {
      return this->m_PrecedenceInfo < e.m_PrecedenceInfo;
    }
  }

  bool operator ==(PrecedenceEntry& e) {
    return this->m_PrecedenceInfo == e.m_PrecedenceInfo;
  }
}; 

class PrecedenceInfoTable: public std::unordered_map<TokenKind, PrecedenceInfo> {};

class PrecedenceTable : public std::unordered_map<OpType, PrecedenceInfoTable> {};

// Expression Precedence Bucket for the calculation of values 
// with associated by operator itself 
class OpPrecBucket : public std::vector<PrecedenceEntry> {};


#endif
