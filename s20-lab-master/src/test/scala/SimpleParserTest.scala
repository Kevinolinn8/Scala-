import org.scalatest.FlatSpec
import scala.collection.mutable.Stack
import scala.util.parsing.input._
import Ast._
import Evaluator2._

class SimpleParserTest extends FlatSpec {
  "Simple Parser Combinators" should "properly compute sums/products" in {
    assert(SimpleParser.parse("0.0") === ve(0.0f))
    assert(SimpleParser.parse("true") === ve(true))
    assert(SimpleParser.parse("1.0*2.0+3.0") === BopExpr(BopExpr(ve(1.0f), TimesBop, ve(2.0f)), PlusBop, ve(3.0f)))

    assert(SimpleParser.parse("true || false") === BopExpr(ve(true), OrBop, ve(false)))
    assert(SimpleParser.parse("false || true && true && false") ===  BopExpr(ValueExpr(BoolVal(false)),OrBop,BopExpr(ValueExpr(BoolVal(true)),AndBop,BopExpr(ValueExpr(BoolVal(true)),AndBop,ValueExpr(BoolVal(false))))))

  }

  it should "properly compute difference" in {
    assert(SimpleParser.parse("1.0*2.0-3.0") === BopExpr(BopExpr(ve(1.0f), TimesBop, ve(2.0f)), MinusBop, ve(3.0f)))

    assert(SimpleParser.parse("6.0-2.0/2.0") === BopExpr(ve(6.0f), MinusBop, BopExpr(ve(2.0f), DivBop, ve(2.0f))))
    assert(SimpleParser.parse("10-5/9/2.0") === BopExpr(ValueExpr(NumVal(10.0f)),MinusBop,BopExpr(ValueExpr(NumVal(5.0f)),DivBop,BopExpr(ValueExpr(NumVal(9.0f)),DivBop,ValueExpr(NumVal(2.0f))))))
    assert(SimpleParser.parse("true/15.0+5*2&&false") === BopExpr(BopExpr(ValueExpr(BoolVal(true)),DivBop,ValueExpr(NumVal(15.0f))),PlusBop,BopExpr(ValueExpr(NumVal(5.0f)),TimesBop,BopExpr(ValueExpr(NumVal(2.0f)),AndBop,ValueExpr(BoolVal(false))))))
  }
}
