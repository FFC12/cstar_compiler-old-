#include <parser/parser_private.hpp>

using namespace cstar::parser_private;

ASTNode CStarParser::reduceExpression(std::deque<ASTNode>& exprBucket,
                                      OpPrecBucket& opPrecBucket) {
  auto popAtom = [&](const PrecedenceEntry& pe, size_t pos) -> ASTNode {
    if (pos >= exprBucket.size())
      ParserError("Unexpected token", pe.entryTokenInfo());

    auto atom = std::move(exprBucket[pos]);
    exprBucket.erase(exprBucket.begin() + pos);
    return std::move(atom);
  };

  auto insertNode = [&](size_t pos, ASTNode node) {
    if (exprBucket.empty())
      exprBucket.push_back(std::move(node));
    else
      exprBucket.insert(exprBucket.begin() + pos, std::move(node));
  };

  int i = 0;
  for (auto& op : opPrecBucket) {
    auto tokenPos = op.entryTokenInfo().getTokenPositionInfo();
    auto semLoc = SemanticLoc(tokenPos.begin, tokenPos.end, tokenPos.line);

    if (op.entryOpType() == OpType::OP_UNARY) {
      UnaryOpKind unaryOpKind = UnaryOpKind::U_SIZEOF;
      UnaryNotationSign unaryNotationSign = UnaryNotationSign::S_POS;

      switch (op.entryTokenKind()) {
        case TokenKind::TILDE:
          unaryOpKind = UnaryOpKind::U_BINNEG;
          break;
        case TokenKind::SIZEOF:
          unaryOpKind = UnaryOpKind::U_SIZEOF;
          break;
        case TokenKind::TYPEOF:
          unaryOpKind = UnaryOpKind::U_TYPEOF;
          break;
        case TokenKind::MOVE:
          unaryOpKind = UnaryOpKind::U_MOVE;
          break;
        case TokenKind::PLUSPLUS:
          unaryNotationSign = UnaryNotationSign::S_POS;
          goto jump_fix;
        case TokenKind::MINUSMINUS:
          unaryNotationSign = UnaryNotationSign::S_NEG;
        jump_fix:
          if (op.entryPrecInfo().isLtr())
            unaryOpKind = UnaryOpKind::U_POSTFIX;
          else
            unaryOpKind = UnaryOpKind::U_PREFIX;
          break;
        case TokenKind::PLUS:
          unaryOpKind = UnaryOpKind::U_POSITIVE;
          break;
        case TokenKind::MINUS:
          unaryOpKind = UnaryOpKind::U_NEGATIVE;
          break;
        case TokenKind::NOT:
          unaryOpKind = UnaryOpKind::U_NOT;
          break;
        case TokenKind::DEREF:
        case TokenKind::STAR:
          unaryOpKind = UnaryOpKind::U_DEREF;
          break;
        case TokenKind::REF:
        case TokenKind::AND:
          unaryOpKind = UnaryOpKind::U_REF;
          break;
        default: {
          std::cout << "Iteration : " << i << std::endl;
          std::cout << "Op Count: " << opPrecBucket.size() << std::endl;
          for (auto& op_d : opPrecBucket) {
            op_d.print();
            std::cout << " ---------------- \n";
          }
          assert(false && "Unreacheable!");
        }
      }
      size_t pos = op.entryStride();

      auto expr = std::move(popAtom(op, pos));
      auto node = std::make_unique<UnaryOpAST>(std::move(expr), unaryOpKind,
                                               unaryNotationSign, semLoc);

      insertNode(pos, std::move(node));
    } else if (op.entryOpType() == OpType::OP_BINARY) {
      BinOpKind binOpKind = BinOpKind::B_ADD;

      // Debuggin purpose
      std::string opCharacter = this->m_Lexer.tokenAsStr(op.entryTokenKind());
      // std::cout << opCharacter << std::endl;

      bool funcCall = false;
      bool ternaryOp = false;

      // select the op kind
      switch (op.entryTokenKind()) {
        case LPAREN:
          funcCall = true;
          break;
        case COMMA:
          binOpKind = BinOpKind::B_COMM;
          break;
        case PLUS:
          binOpKind = BinOpKind::B_ADD;
          break;
        case MINUS:
          binOpKind = BinOpKind::B_SUB;
          break;
        case STAR:
          binOpKind = BinOpKind::B_MUL;
          break;
        case DIV:
          binOpKind = BinOpKind::B_DIV;
          break;
        case MOD:
          binOpKind = BinOpKind::B_MOD;
          break;
        case AND:
          binOpKind = BinOpKind::B_AND;
          break;
        case LAND:
          binOpKind = BinOpKind::B_LAND;
          break;
        case OR:
          binOpKind = BinOpKind::B_OR;
          break;
        case LOR:
          binOpKind = BinOpKind::B_LOR;
          break;
        case XOR:
          binOpKind = BinOpKind::B_XOR;
          break;
        case GT:
          binOpKind = BinOpKind::B_GT;
          break;
        case GTEQ:
          binOpKind = BinOpKind::B_GTEQ;
          break;
        case LT:
          binOpKind = BinOpKind::B_LT;
          break;
        case LTEQ:
          binOpKind = BinOpKind::B_LTEQ;
          break;
        case LSHIFT:
          binOpKind = BinOpKind::B_SHL;
          break;
        case RSHIFT:
          binOpKind = BinOpKind::B_SHR;
          break;
        case EQUALEQUAL:
          binOpKind = BinOpKind::B_EQ;
          break;
        case NOTEQUAL:
          binOpKind = BinOpKind::B_NEQ;
          break;
        case DOT:
          binOpKind = BinOpKind::B_DOT;
          break;
        case ARROW:
          binOpKind = BinOpKind::B_ARW;
          break;
        case COLONCOLON:
          binOpKind = BinOpKind::B_CCOL;
          break;
          // TODO: Add other binary ops if needed
        case QMARK:
          binOpKind = BinOpKind::B_TER;
          ternaryOp = true;
          break;
        case LSQPAR:
          binOpKind = BinOpKind::B_ARRS;
          break;
        case COLON:
          binOpKind = BinOpKind::B_MARRS;
          break;
        default: {
          std::cout << "Iteration : " << i << std::endl;
          std::cout << "Op Count: " << opPrecBucket.size() << std::endl;
          for (auto& op_d : opPrecBucket) {
            op_d.print();
            std::cout << " ---------------- \n";
          }
          assert(false && "Unreacheable");
        }
      }
      size_t pos = op.entryStride();
      // pos - 1 since it's binary op
      // Popping the element at the same loc [0,1,2] -> pop(1) -> [0,2] ->
      // pop(1) -> [0]
      if (!funcCall && !ternaryOp) {
        auto lhs = std::move(popAtom(op, pos - 1));
        auto rhs = std::move(popAtom(op, pos - 1));

        for (auto& walkOp : opPrecBucket) {
          if (walkOp.entryStride() > pos) walkOp.decreaseStride();
        }

        auto node = std::make_unique<BinaryOpAST>(
            std::move(lhs), std::move(rhs), nullptr, binOpKind, opCharacter,
            semLoc);

        insertNode(pos - 1, std::move(node));
      } else if (!funcCall && ternaryOp) {
        auto cond = std::move(popAtom(op, pos - 2));
        auto b0 = std::move(popAtom(op, pos - 2));
        auto b1 = std::move(popAtom(op, pos - 2));

        for (auto& walkOp : opPrecBucket) {
          if (walkOp.entryStride() > pos) {
            walkOp.decreaseStride();
            walkOp.decreaseStride();
          }
        }

        auto node = std::make_unique<BinaryOpAST>(
            std::move(cond), std::move(b0), std::move(b1), binOpKind,
            opCharacter, semLoc);
        insertNode(pos - 2, std::move(node));
      } else {
        // Maybe it has type attrib
        if (op.entryHasTypeAttrib()) {
          auto lhs = std::move(popAtom(op, pos - 2));
          auto type = std::move(popAtom(op, pos - 2));
          auto rhs = std::move(popAtom(op, pos - 2));

          for (auto& walkOp : opPrecBucket) {
            if (walkOp.entryStride() > pos) {
              walkOp.decreaseStride();
              walkOp.decreaseStride();
            }
          }
          // lhs -> must be symbol name -> func name
          // if(!is(static_cast<>lhs.get()))
          auto node = std::make_unique<FuncCallAST>(
              std::move(lhs), std::move(type), std::move(rhs), semLoc);
          insertNode(pos - 2, std::move(node));
        } else {
          auto lhs = std::move(popAtom(op, pos - 1));
          auto rhs = std::move(popAtom(op, pos - 1));

          for (auto& walkOp : opPrecBucket) {
            if (walkOp.entryStride() > pos) {
              walkOp.decreaseStride();
            }
          }
          // lhs -> must be symbol name -> func name
          // if(!is(static_cast<>lhs.get()))
          auto node = std::make_unique<FuncCallAST>(std::move(lhs), nullptr,
                                                    std::move(rhs), semLoc);
          insertNode(pos - 1, std::move(node));
        }
      }

    } else if (op.entryOpType() == OpType::OP_CAST) {
      CastOpKind castOpKind = CastOpKind::C_CAST;

      switch (op.entryTokenKind()) {
        case UNSAFE_CAST:
          castOpKind = CastOpKind::C_UNSAFE_CAST;
          break;
        case CAST:
          castOpKind = CastOpKind::C_CAST;
          break;
        case AS:
          castOpKind = CastOpKind::C_AS;
          break;
        default:
          // unreacheable
          assert(false && "Unreacheable");
      }
      // sizeof type + 10 + unsafe_cast<int>(10 - 30 + 40);
      // atom => [type, 10, int, 10, 30, 40 ]
      // op => [ (sizeof), (+,1), (+,2), (unsafe_cast,2)

      if (castOpKind == CastOpKind::C_AS) {
        size_t pos = op.entryStride();
        auto type = std::move(popAtom(op, pos));
        auto expr = std::move(popAtom(op, pos - 1));

        auto node = std::make_unique<CastNode>(std::move(expr), std::move(type),
                                               castOpKind, true, semLoc);
        insertNode(pos - 1, std::move(node));
      } else if (op.entryHasTypeAttrib()) {
        size_t pos = op.entryStride();
        auto type = std::move(popAtom(op, pos));
        auto expr = std::move(popAtom(op, pos));

        auto node = std::make_unique<CastNode>(std::move(expr), std::move(type),
                                               castOpKind, true, semLoc);
        insertNode(pos, std::move(node));
      } else {
        size_t pos = op.entryStride();
        auto expr = std::move(popAtom(op, pos));

        auto node = std::make_unique<CastNode>(std::move(expr), nullptr,
                                               castOpKind, false, semLoc);
        insertNode(pos, std::move(node));
      }
    } else {
      std::cout << "Iteration : " << i << std::endl;
      std::cout << "Op Count: " << opPrecBucket.size() << std::endl;
      for (auto& op_d : opPrecBucket) {
        op_d.print();
        std::cout << " ---------------- \n";
      }
      assert(false && "Unreacheable!");
    }
    i++;
  }

  if (exprBucket.empty() || exprBucket.size() > 1) {
    if (!exprBucket.empty()) {
      auto tokenInfo = opPrecBucket[opPrecBucket.size() - 1].entryTokenInfo();
      ParserError("Unexpected token '" +
                      std::string(tokenToStr(tokenInfo.getTokenKind())) + "'",
                  tokenInfo);
    }
    /*else {

      auto expr = exprBucket[exprBucket.size() - 1].get();
      auto semLoc = expr->getSemLoc();
      TokenInfo tokenInfo(
          TokenKind::UNKNOWN,
          PositionInfo(semLoc.begin, semLoc.end, semLoc.line));
      ParserError(
          "Unexpected token. Types cannot be involved to arithmetic "
          "operations.",
          tokenInfo);
    }*/
  }

  return std::move(exprBucket[0]);
}
