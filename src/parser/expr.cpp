#include <parser/parser.hpp>

/*
 	int p = x + 2 / 3;
	int* r = &p;
	uint k = unsafe_cast<uint>(p) + 25 + (*r * 2) + (true ? 1 : 2) + funcCall(0,1,2,3) + arr_[2] + arr[2][3]; 
*/

ASTNode CStarParser::expression() {
	//advance the EQUAL here.
	this->advance();

	//and perform	parsing the expression by recursive-descent way.
	return this->advanceConstantOrLiteral();
}

ASTNode CStarParser::advanceConstantOrLiteral() {
	//this is obviously a constant
	if(is(TokenKind::SCALARD) || is(TokenKind::SCALARI) || is(TokenKind::LITERAL)) { 
		bool isIntegral = is(TokenKind::LITERAL) ? false : true;
		bool isFloat = is(TokenKind::SCALARD) ? true : false;

		auto value = this->currentTokenStr(); 
		return std::make_unique<ScalarAST>(value,isIntegral,isFloat);
	}

	return nullptr;
}

ASTNode CStarParser::advanceRef() {

}

ASTNode CStarParser::advanceIndirect() {

}

ASTNode CStarParser::advanceBinOp() {
	
}

ASTNode CStarParser::advanceUnaryOp() {

}

ASTNode CStarParser::advanceFunctionCall() {

}

ASTNode CStarParser::advanceArraySubscript() {

}
