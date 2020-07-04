import org.scalatest.FlatSpec
import Ast._
import Evaluator2._

// unit tests for the Tree functions
class Evaluator2Test extends FlatSpec {
  "Eval" should "perform 3 + (4 + 5) --> 12" in {
    assert(Evaluator2.eval(BopExpr(ve(3f),PlusBop,BopExpr(ve(4f),PlusBop,ve(5f)))) === Some(v(12f)))
  }
  it should "return True for True AND Not False" in {
    assert(Evaluator2.eval(BopExpr(ve(true),AndBop,UopExpr(NotUop,ve(false)))) === Some(v(true)))
  }
  it should "perform (4 / 2) - 1 --> 1" in {
    assert(Evaluator2.eval(BopExpr(BopExpr(ve(4),DivBop,ve(2)),MinusBop, ve(1))) === Some(v(1)))
  }
  it should "return True for (4 * 2 === 8) OR False" in {
    assert(Evaluator2.eval(BopExpr(BopExpr(BopExpr(ve(4),TimesBop,ve(2)),EqBop,ve(8)),OrBop,ve(false))) === Some(v(true)))
  }
  it should "return True for (5 > 5) !== True" in {
    assert(Evaluator2.eval(BopExpr(BopExpr(ve(5),GtBop,ve(5)),NeqBop,ve(true))) === Some(v(true)))
  }
  it should "return True for Not (False and False)" in {
    assert(Evaluator2.eval(UopExpr(NotUop,BopExpr(ve(false),AndBop,ve(false)))) === Some(v(true)))
  }


  "Typecheck" should "typecheck the expression 3 + (4 + 5) as Number" in {
    assert(Evaluator2.typecheck(BopExpr(ve(3f),PlusBop,BopExpr(ve(4f),PlusBop,ve(5f)))) === Some(NumType))
  }

  it should "typecheck the expression 3 < 5 as BoolType" in {
    assert(Evaluator2.typecheck(BopExpr(ve(3f),LtBop,ve(5f))) === Some(BoolType))
  }

  it should "typecheck the expression 3 === 5 as BoolType" in {
    assert(Evaluator2.typecheck(BopExpr(ve(3f),EqBop,ve(5f))) === Some(BoolType))
  }

  it should "typecheck the expression 3 <= 5 as BoolType" in {
    assert(Evaluator2.typecheck(BopExpr(ve(3f),LteBop,ve(5f))) === Some(BoolType))
  }

  it should "typecheck the expression 10 - (4 + 5) as Number" in {
    assert(Evaluator2.typecheck(BopExpr(ve(10f),MinusBop,BopExpr(ve(4f),PlusBop,ve(5f)))) === Some(NumType))
  }

  it should "typecheck the expression 6 * 5 as NumType" in {
    assert(Evaluator2.typecheck(BopExpr(ve(6f),TimesBop,ve(5f))) === Some(NumType))
  }

  it should "typecheck the expression 10 == 10 as BoolType" in {
    assert(Evaluator2.typecheck(BopExpr(ve(10f),EqBop,ve(10f))) === Some(BoolType))
  }

  it should "typecheck the expression 3 != 5 as BoolType" in {
    assert(Evaluator2.typecheck(BopExpr(ve(3f),NeqBop,ve(5f))) === Some(BoolType))
  }

  it should "typecheck the expression 5 <= 5 as BoolType" in {
    assert(Evaluator2.typecheck(BopExpr(ve(5f),LteBop,ve(5f))) === Some(BoolType))
  }

  it should "typecheck the expression 19 > 3 as BoolType" in {
    assert(Evaluator2.typecheck(BopExpr(ve(19f),GtBop,ve(3f))) === Some(BoolType))
  }
  
  it should "typecheck the expression 4 <= 4 as BoolType" in {
    assert(Evaluator2.typecheck(BopExpr(ve(4f),GteBop,ve(4f))) === Some(BoolType))
  } 

  it should "fail to typecheck the expression 3 + (true + 5)" in {
    assert(Evaluator2.typecheck(BopExpr(ve(3),PlusBop,BopExpr(ve(true),PlusBop,ve(5)))) === None)
  }

  it should "fail to typecheck the expression 3 * (5 / False)" in {
    assert(Evaluator2.typecheck(BopExpr(ve(3),TimesBop,BopExpr(ve(5),PlusBop,ve(false)))) === None)
  }
   
}
