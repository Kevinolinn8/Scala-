import org.scalatest.FlatSpec
import Ast._
import Evaluator4._

// unit tests for the Tree functions
class Evaluator4Test extends FlatSpec {
  "Eval" should "perform 3 + (4 + 5) --> 12" in {
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve(3f),PlusBop,BopExpr(ve(4f),PlusBop,ve(5f)))) === v(12f))
  }

  it should "handle simple constant" in {
    assert(Evaluator4.eval((List(Map("x"->0)),Map(0->(Immutable,NumVal(123f)))), BopExpr(VarExpr("x"),PlusBop,BopExpr(ve(4f),PlusBop,ve(5f)))) === v(123f+4f+5f))

    assert(Evaluator4.eval((List(Map("p"->0)),Map(0->(Immutable,StringVal("500")))), BopExpr(VarExpr("p"),PlusBop,ve("200"))) === v("500"+200f))
  }

  it should "handle simple recursive function" in {
    val n = 6;
    //   defining function dec     input list of function dec (one input x)             IfExpr defines the function body                                                             None, doesn't care what return type is
    val e1 = CallExpr(LambdaExpr(Some("dec"),List(("x",None)),IfExpr(BopExpr(VarExpr("x"),GtBop,ve(0f)),BopExpr(VarExpr("x"),PlusBop,CallExpr(VarExpr("dec"),List(BopExpr(VarExpr("x"),MinusBop,ve(1f))))),ve(0f)),None),List(ve(n.toFloat)))
    assert(Evaluator4.eval((List(),Map()), e1) === v((0 to n).foldLeft(0f)(_.toFloat+_.toFloat)))
     }

  it should "handle string equal/not equal function" in {
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve(5),EqBop,ve("6"))) === v(false))
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve("250"),EqBop,ve("6"))) === v(false))
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve(10),EqBop,ve("10"))) === v(true))
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve("250"),EqBop,ve(90))) === v(false))
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve(5),EqBop,ve(5))) === v(true))

    assert(Evaluator4.eval((List(),Map()), BopExpr(ve(5000),NeqBop,ve(65))) === v(true))
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve(6),NeqBop,ve("6"))) === v(false))
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve("14"),NeqBop,ve(12))) === v(true))
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve("6"),NeqBop,ve("6"))) === v(false))
  }

  it should "handle string plus function" in {
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve(5),PlusBop,ve("6"))) === v(11.0f))
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve("2451"),PlusBop,ve(25))) === v(2476f))
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve(5),PlusBop,ve(546))) === v(551.0f))
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve("53"),PlusBop,ve("6"))) === v(59.0f))
  }

  it should "handle string less than function" in {
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve(5),LtBop,ve("10"))) === v(true))
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve("5"),LtBop,ve(10))) === v(true))
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve("5"),LtBop,ve("10"))) === v(true))
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve(10),LtBop,ve(5))) === v(false))

    assert(Evaluator4.eval((List(),Map()), BopExpr(ve(5),LteBop,ve("hey"))) === v(false))
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve("5"),LteBop,ve(10))) === v(true))
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve("5"),LteBop,ve("10"))) === v(true))
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve(5),LteBop,ve(5))) === v(true))
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve("same string"),LteBop,ve("Same string"))) === v(false))
  }

  it should "handle string greater than function" in {
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve(5),GtBop,ve("10"))) === v(false))
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve("5"),GtBop,ve(10))) === v(false))
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve("5"),GtBop,ve("10"))) === v(false))
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve(10),GtBop,ve(5))) === v(true))

    assert(Evaluator4.eval((List(),Map()), BopExpr(ve(5),GteBop,ve("10"))) === v(false))
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve("5"),GteBop,ve(10))) === v(false))
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve("5"),GteBop,ve("10"))) === v(false))
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve(5),GteBop,ve(5))) === v(true))
    assert(Evaluator4.eval((List(),Map()), BopExpr(ve("cool"),GteBop,ve("hot"))) === v(false))
  }
  it should "handle Lambda expressions using CallExpr" in {
    val i = 5;
    val j = 10;
    val k = 1;
    val l = 180;
    val m = false
    val e1 = CallExpr(LambdaExpr(Some("squared"),List(("x",None)),BopExpr(VarExpr("x"),TimesBop,VarExpr("x")),None),List(ve(i.toFloat)))
    val e2 = CallExpr(LambdaExpr(Some("squared"),List(("n",None)),BopExpr(VarExpr("n"),TimesBop,VarExpr("n")),None),List(ve(j.toFloat)))
    val e3 = CallExpr(LambdaExpr(Some("radToDeg"),List(("rad",None)),BopExpr(BopExpr(VarExpr("rad"),TimesBop,ve(180f)),DivBop,ve(3.14f)),None),List(ve(k.toFloat)))
    val e4 = CallExpr(LambdaExpr(Some("degToRad"),List(("deg",None)),BopExpr(BopExpr(VarExpr("deg"),TimesBop,ve(3.14f)),DivBop,ve(180f)),None),List(ve(l.toFloat)))
    val e5 = CallExpr(LambdaExpr(Some("NOTbool"),List(("boolVal",None)),UopExpr(NotUop,VarExpr("boolVal")),None),List(ve(m)))
    assert(Evaluator4.eval((List(),Map()), e1) === v((i * i).toFloat))
    assert(Evaluator4.eval((List(),Map()), e2) === v(100f))
    assert(Evaluator4.eval((List(),Map()), e3) === v(180/3.14))
    assert(Evaluator4.eval((List(),Map()), e4) === v(3.14f))
    assert(Evaluator4.eval((List(),Map()), e5) == v(true))

  }

// Maybe use Evaluator3Test for examples
  // it should "perform cat + dog"{

  // }
}
