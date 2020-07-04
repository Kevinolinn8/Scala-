import org.scalatest.FlatSpec
import Parsing._

// unit tests for the Tree functions
class ParsingTest extends FlatSpec {
  "Lexer" should "properly evaluate simple strings" in {
    assert(Parsing.lexer("1.234truefalse cool \"one two\"123".toList) === List(NumToken("1.234"), KeywordToken("true"), KeywordToken("false"), StrToken("one two"), NumToken("123")))
    assert(Parsing.lexer("1.234 + 2 + 6 +0+1".toList) === List(NumToken("1.234"), OpToken("+"), NumToken("2"), OpToken("+"), NumToken("6"), OpToken("+"), NumToken("0"), OpToken("+"), NumToken("1")))
    assert(Parsing.lexer("1".toList) === List(NumToken("1")))
    assert(Parsing.lexer("\"string\"".toList) === List(StrToken("string")))
    assert(Parsing.lexer("\"string\"\"two\"".toList) === List(StrToken("string"), StrToken("two")))
    assert(Parsing.lexer("".toList) === List())
  }


  "Lexer" should "properly evaluate simple strings" in {
    assert(Parsing.lexer("1".toList) === List(NumToken("1")))
  }

  "Parser" should "properly evaluate simple strings" in {
    assert(Parsing.parser(Parsing.lexer("1 + 2 + 3 * 5 + 6".toList)) === (Some(BopExpr(ConstIntExpr(1),PlusBop,BopExpr(ConstIntExpr(2),PlusBop,BopExpr(BopExpr(ConstIntExpr(3),TimesBop,ConstIntExpr(5)),PlusBop,ConstIntExpr(6))))),List()))
  }

    "Parser" should "properly evaluate simple strings" in {
      assert(Parsing.parser(Parsing.lexer("1  1  1  1  1".toList)) === (None, [1, 1, 1, 1])
    }

    "Parser" should "properly evaluate simple strings" in {
    assert(Parsing.parser(Parsing.lexer("1 + 2 + 3 * 5 + 6".toList)) === (Some(BopExpr(ConstIntExpr(1),PlusBop,BopExpr(ConstIntExpr(2),PlusBop,BopExpr(BopExpr(ConstIntExpr(3),TimesBop,ConstIntExpr(5)),PlusBop,ConstIntExpr(6))))),List()))
  }

    "Parser" should "properly evaluate simple strings" in {
    assert(Parsing.parser(Parsing.lexer("1 + 2 + 3 * 5 + 6".toList)) === (Some(BopExpr(ConstIntExpr(1),PlusBop,BopExpr(ConstIntExpr(2),PlusBop,BopExpr(BopExpr(ConstIntExpr(3),TimesBop,ConstIntExpr(5)),PlusBop,ConstIntExpr(6))))),List()))
  }

    "Parser" should "properly evaluate simple strings" in {
    assert(Parsing.parser(Parsing.lexer("1 + 2 + 3 * 5 + 6".toList)) === (Some(BopExpr(ConstIntExpr(1),PlusBop,BopExpr(ConstIntExpr(2),PlusBop,BopExpr(BopExpr(ConstIntExpr(3),TimesBop,ConstIntExpr(5)),PlusBop,ConstIntExpr(6))))),List()))
  }
}
