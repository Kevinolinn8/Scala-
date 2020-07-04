import org.scalatest.FlatSpec
import Ast._
import Evaluator3._

// unit tests for the Tree functions
class Evaluator3Test extends FlatSpec {
  "Eval" should "perform 3 + (4 + 5) --> 12" in {
    assert(Evaluator3.eval((List(),Map()), BopExpr(ve(3f),PlusBop,BopExpr(ve(4f),PlusBop,ve(5f)))) === v(12f))
  }


  "Eval" should "perform 3 * (5 + 5) --> 30" in {
    assert(Evaluator3.eval((List(),Map()), BopExpr(ve(3f),TimesBop,BopExpr(ve(5f),PlusBop,ve(5f)))) === v(30f))
  }

    "Eval" should "perform 3 * (5 * 1) --> 15" in {
    assert(Evaluator3.eval((List(),Map()), BopExpr(ve(3f),TimesBop,BopExpr(ve(5f),TimesBop,ve(1f)))) === v(15f))
  }

    "Eval" should "perform 10 - (4 + 5) --> 1" in {
    assert(Evaluator3.eval((List(),Map()), BopExpr(ve(10f),MinusBop,BopExpr(ve(4f),PlusBop,ve(5f)))) === v(1f))
  }

    "Eval" should "perform 5 / (6 - 1) --> 1" in {
    assert(Evaluator3.eval((List(),Map()), BopExpr(ve(5f),DivBop,BopExpr(ve(6f),MinusBop,ve(1f)))) === v(1f))
  }
 
   "Eval" should "determine if statement is true or false" in {
    assert(Evaluator3.eval((List(),Map()), IfExpr(ve(true), ve(false), ve(true))) === v(false))
  }

   "Eval" should "determine if NotUop" in {
    assert(Evaluator3.eval((List(),Map()), UopExpr(NotUop, ve(true))) === v(false))
  }

   "Eval" should "determine if NegUop" in {
    assert(Evaluator3.eval((List(),Map()), UopExpr(NegUop, ve(3))) === v(-3))
  }

  it should "handle simple constant" in {
    assert(Evaluator3.eval((List(Map("x"->0)),Map(0->(Immutable,v(123f)))), BopExpr(VarExpr("x"),PlusBop,BopExpr(ve(4f),PlusBop,ve(5f)))) === v(123f+4f+5f))
  }

  it should "handle let" in {
    assert(Evaluator3.eval((List(),Map()), LetExpr(Immutable, "x",ve(123f) ,BopExpr(VarExpr("x"),PlusBop,BopExpr(ve(4f),PlusBop,ve(5f))))) === v(123f+4f+5f))
  }

}
