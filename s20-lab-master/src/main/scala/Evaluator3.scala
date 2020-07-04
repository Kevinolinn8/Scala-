
import scala.io.Source

import Ast._


object Evaluator3 {
  // eval(e) should return a *constant* expression
  // (if Node.js produces a constant expression (value) for an example JavaScript program,
  // your evaluator should produce that same value).
  def eval(env : Environment, e : Expr) : Value = {
    e match {
      // Completed example cases
      case ValueExpr(UndefVal) => UndefVal
      case BopExpr(e1,AndBop,e2) => BoolVal(toBool(eval(env, e1)) && toBool(eval(env, e2)))
      case BopExpr(e1,OrBop,e2) => BoolVal(toBool(eval(env, e1)) || toBool(eval(env, e2)))
      case BopExpr(e1,PlusBop,e2) => NumVal(toNum(eval(env, e1)) + toNum(eval(env, e2)))
      case BopExpr(e1,MinusBop,e2) => NumVal(toNum(eval(env, e1)) - toNum(eval(env, e2)))
      case BopExpr(e1,TimesBop,e2) => NumVal(toNum(eval(env, e1)) * toNum(eval(env, e2)))
      case BopExpr(e1,DivBop,e2) => NumVal(toNum(eval(env, e1)) / toNum(eval(env, e2)))
      case BopExpr(e1,EqBop,e2) => BoolVal(toBool(eval(env, e1)) == toBool(eval(env, e2)))
      case BopExpr(e1,NeqBop,e2) => BoolVal(toBool(eval(env, e1)) != toBool(eval(env, e2)))
      case BopExpr(e1,LtBop,e2) => BoolVal(toBool(eval(env, e1)) < toBool(eval(env, e2)))
      case BopExpr(e1,LteBop,e2) => BoolVal(toBool(eval(env, e1)) <= toBool(eval(env, e2)))
      case BopExpr(e1,GtBop,e2) => BoolVal(toBool(eval(env, e1)) > toBool(eval(env, e2)))
      case BopExpr(e1,GteBop,e2) => BoolVal(toBool(eval(env, e1)) >= toBool(eval(env, e2)))
      case UopExpr(NotUop,e1) => BoolVal(!(toBool(eval(env, e1))))
      case UopExpr(NegUop,e1) => NumVal(-(toNum(eval(env, e1))))
      case ValueExpr(NumVal(d)) => NumVal(d)
      case ValueExpr(BoolVal(d)) => BoolVal(d)
      case IfExpr(c,e1,e2) => {
        eval(env, c) match {
          case BoolVal(true) => {
            eval(env,e1)
          }
          case BoolVal(false) => {
            eval(env,e2)
          }
        }
      }
      // See: Ast.readEnvironment, Ast.pushEnvironment
      case VarExpr(x) => Ast.readEnvironment(env, x)._2
      case LetExpr(a,x,e1,e2) => {

        val new_environment = Ast.pushEnvironment(env, x, (a, eval(env, e1)))
        eval(new_environment, e2)
      }
      case ValueExpr(d) => NumVal(toNum(d))


      case PrintExpr(e) => {
        println(toStr(eval(env, e)))
        UndefVal
      }

      // TODO: replace wildcard with other cases: BoolVal, NumVal, ||,
      // +, -, *, /, <=, <, >=, >, ==, !=,
     
      //need to do 
      // - (unary), !, Vars, If,
      // Let.
      case _ => throw UnimplementedError(e)



      // See: Ast.readEnvironment, Ast.pushEnvironment

      // Unimplemented for this project
      case ValueExpr(StringVal(_)) => throw UnimplementedError(e)
      case ValueExpr(ClosureVal(_,_)) => throw UnimplementedError(e)
      case ValueExpr(ReferenceVal(_)) => throw UnimplementedError(e)
      case LetExpr(Mutable, v, e1, e2) => throw UnimplementedError(e)
      case LambdaExpr(name : Option[String], _, e1 : Expr, t2) => throw UnimplementedError(e)
      case CallExpr(e1 : Expr, _) => throw UnimplementedError(e)
      case ObjectExpr(_) => throw UnimplementedError(e)
      case AssignExpr(_,_,_) => throw UnimplementedError(e)
      case FieldExpr(_,_) => throw UnimplementedError(e)
      case ReturnExpr(_) => throw UnimplementedError(e)
    }
  }
}
