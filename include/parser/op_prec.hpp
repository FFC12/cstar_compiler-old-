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
};

class PrecedenceEntry {
  bool m_IsOp;
  bool m_IsNode;
  TokenKind m_Token;
  size_t m_Id;
  bool m_IsFirst;
  bool m_IsLast;
	PrecedenceInfo m_PrecedenceInfo;				
	
public:
	PrecedenceEntry(const TokenKind& kind, const PrecedenceInfo& precInfo, size_t id,
			bool isOp, bool isNode, bool isFirst, bool isLast) 
		: m_Token(kind), m_PrecedenceInfo(precInfo), m_Id(id), 
			m_IsOp(isOp), m_IsNode(isNode), m_IsFirst(isFirst), m_IsLast(isLast)
	{
	}
};

class PrecedenceInfoTable: public std::unordered_map<TokenKind, PrecedenceInfo> {};

class PrecedenceTable : public std::unordered_map<OpType, PrecedenceInfoTable> {};

// Expression Precedence Bucket for the calculation of each operator and values 
// with associated by operator itself
class ExprPrecBucket : public std::vector<PrecedenceEntry> {}; 


#endif
