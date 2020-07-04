import scala.io.Source
import Ast._

object Evaluator4 {
  // eval(e) should return a *constant* expression (or throw a DynamicTypeError)
  // (as in Lab 2, use Node.js to check the correctness of your evaluator).
  def eval(env : Environment, e : Expr) : Value = {
    e match {
      // Completed example cases
      case ValueExpr(UndefVal) => UndefVal
      case BopExpr(e1,AndBop,e2) => BoolVal(toBool(eval(env, e1)) && toBool(eval(env, e2)))
      case BopExpr(e1,OrBop,e2) => BoolVal(toBool(eval(env, e1)) || toBool(eval(env, e2)))
        // defining with strings now...Plus, Equal, NotEquals, Lt, Gt, Lte, Gte
      case BopExpr(e1,PlusBop,e2) => {
        eval(env, e1) match {
          case NumVal(a) => {
            eval(env,e2) match {
            case NumVal(b) => NumVal(a + b) 
            case  StringVal(b) => StringVal(a + b)
            }
          }
          case StringVal(a) => StringVal(a + toStr(eval(env, e2)))
        }
      }
      case BopExpr(e1,MinusBop,e2) => NumVal(toNum(eval(env,e1)) - toNum(eval(env,e2)))
      case BopExpr(e1,TimesBop,e2) => NumVal(toNum(eval(env,e1)) * toNum(eval(env,e2)))
      case BopExpr(e1,DivBop,e2) => NumVal(toNum(eval(env,e1)) / toNum(eval(env,e2)))
      case BopExpr(e1,EqBop,e2) => {
        eval(env, e1) match{
          case NumVal(a) => {
              eval(env, e2) match{
              case  NumVal(b) => BoolVal(a == b)
              case  StringVal(b) => BoolVal(a == b)
              }
            }
            case StringVal(a) => BoolVal(a == eval(env, e2))
        }
      }  
    
      case BopExpr(e1,NeqBop,e2) => {
        eval(env, e1) match{
          case NumVal(a) => {
              eval(env, e2) match{
              case  NumVal(b) => BoolVal(a != b)
              case  StringVal(b) => BoolVal(a != b)
              }
            }
            case StringVal(a) => BoolVal(a != eval(env, e2))
        }
      }  
      case BopExpr(e1,LtBop,e2) => {//BoolVal(toBool(eval(env, e1)) < toBool(eval(env, e2)))
        eval(env, e1) match{
          case NumVal(a) => {
            eval(env, e2) match {
              case NumVal(b) => BoolVal(a < b)
              case StringVal(b) => BoolVal(a < b.toFloat)
            }
          }
          case StringVal(a) => {//BoolVal(toBool(eval(env,e1)) < toBool(eval(env,e2)))
            eval(env,e2) match {
              case NumVal(b) => BoolVal(a.toFloat < b)
              case StringVal(b) => BoolVal(a.toFloat < b.toFloat)
              }
           }
        }
      }

      case BopExpr(e1,LteBop,e2) => {
        eval(env,e1) match {
          case NumVal(a) => {
            eval(env, e2) match {
              case NumVal(b) => BoolVal(a <= b)
              case StringVal(b) => BoolVal(a <= b.toFloat)
            }
          }
          case StringVal(a) => {
            eval(env,e2) match {
              case NumVal(b) => BoolVal(a.toFloat <= b)
              case StringVal(b) => BoolVal(a.toFloat <= b.toFloat)
            }
          }
        }
      }
      
      case BopExpr(e1,GtBop,e2) => {
        eval(env,e1) match {
          case NumVal(a) => {
            eval(env, e2) match {
              case NumVal(b) => BoolVal(a > b)
              case StringVal(b) => BoolVal(a > b.toFloat)
            }
          }
          case StringVal(a) => {
            eval(env, e2) match {
              case NumVal(b) => BoolVal(a.toFloat > b)
              case StringVal(b) => BoolVal(a.toFloat > b.toFloat)
            }
          }
        }
      }

      case BopExpr(e1,GteBop,e2) => {
        eval(env,e1) match {
          case NumVal(a) => {
            eval(env,e2) match {
              case NumVal(b) => BoolVal(a >= b)
              case StringVal(b) => BoolVal(a >= b.toFloat)
            }
          }
          case StringVal(a) => {
            eval(env,e2) match {
              case NumVal(b) => BoolVal(a.toFloat >= b)
              case StringVal(b) => BoolVal(b.toFloat >= b.toFloat)
            }
          }
        }
      }

      
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
// name is name of function I want to define
// xs is list of input for function
// ex is what the actual function is
// rt is the return type, defines return type of whatever the function wants
      case LambdaExpr(name : Option[String], (x : String, t)::Nil, e1 : Expr, rt) => {
        ClosureVal(Map(),LambdaExpr(name,List((x,t)),e1,rt))
      }
      case CallExpr(e1 : Expr, (e2 : Expr)::Nil) => {
        val v1 = eval(env,e1) 
        v1 match {
          case ClosureVal(_, LambdaExpr(name, List((x, t)), ex, rt)) => {
            val new_env1 = pushEnvironment(env, name.get, (Immutable, v1))
            val new_env2 = pushEnvironment(new_env1,x, (Immutable, eval(env,e2)))
            eval(new_env2,ex)
          }
          case _ => throw UnimplementedError(e)
        }
      }     
      case ValueExpr(ClosureVal(_,_)) => throw UnimplementedError(e)
      case ValueExpr(ReferenceVal(_)) => throw UnimplementedError(e)
      case LetExpr(Mutable, v, e1, e2) => throw UnimplementedError(e)
      case LambdaExpr(name : Option[String], Nil, e1 : Expr, t2) => throw UnimplementedError(e)
      case LambdaExpr(name : Option[String], a::b::more, e1 : Expr, t2) => throw UnimplementedError(e)
      case CallExpr(e1 : Expr, Nil) => throw UnimplementedError(e)
      case CallExpr(e1 : Expr, a::b::more) => throw UnimplementedError(e)
      case ObjectExpr(_) => throw UnimplementedError(e)
      case AssignExpr(_,_,_) => throw UnimplementedError(e)
      case FieldExpr(_,_) => throw UnimplementedError(e)
      case ReturnExpr(_) => throw UnimplementedError(e)
    }
  }
}
