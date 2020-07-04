object Parsing {
  // types for expressions
  sealed trait Type
  case object BoolType extends Type
  case object IntType extends Type
  case object FloatType extends Type
  case object StringType extends Type

  // binary operators
  sealed trait Bop
  case object AndBop extends Bop // "&&" operator
  case object OrBop extends Bop  // "||" operator
  case object PlusBop extends Bop  // "+" operator
  case object MinusBop extends Bop // "-" operator
  case object TimesBop extends Bop // "*" operator
  case object DivBop extends Bop   // "/" operator

  // unary operators
  sealed trait Uop
  case object NotUop extends Uop // "!" operator
  case object NegUop extends Uop // "-" operator

  // expressions
  sealed trait Expr
  case class ConstBoolExpr(b : Boolean) extends Expr
  case class ConstIntExpr(i : Int) extends Expr
  case class ConstFloatExpr(f : Float) extends Expr
  case class ConstStringExpr(s : String) extends Expr
  case class BopExpr(e1 : Expr, op : Bop, e2 : Expr) extends Expr
  case class UopExpr(op : Uop, e : Expr) extends Expr

  sealed trait Token
  case class KeywordToken(s : String) extends Token
  case class OpToken(s : String) extends Token
  case class NumToken(s : String) extends Token
  case class StrToken(s : String) extends Token

  def extract(str:List[Char], f:(Int,Char)=>Boolean) : (List[Char],List[Char]) = {
    val (_,_,s1,s2) = str.foldLeft((0,true,List[Char](),List[Char]())){
      case((count,b,s,rest),y) => {
        val b2 = f(count,y)
        (count+1,b && b2,(if(b && b2) (s:+y) else (s)),(if(b && b2) rest else (rest:+y)))
      }
    }
    (s1,s2)
  }

  def lexer(s : List[Char]) : List[Token] = {
    s match {
      case Nil => Nil
      case('t'::'r'::'u'::'e'::more) => KeywordToken("true") +: lexer(more)
      case('f'::'a'::'l'::'s'::'e'::more) => KeywordToken("false") +: lexer(more)
      case('&'::'&'::more) => OpToken("&&") +: lexer(more)
      case('|'::'|'::more) => OpToken("||") +: lexer(more)
      case('+'::more) => OpToken("+") +: lexer(more)
      case('-'::more) => OpToken("-") +: lexer(more)
      case('*'::more) => OpToken("*") +: lexer(more)
      case('/'::more) => OpToken("/") +: lexer(more)
      case('!'::more) => OpToken("!") +: lexer(more)
      case(' '::more) => lexer(more)
      case('\t'::more) => lexer(more)
      case('\r'::more) => lexer(more)
      case('\n'::more) => lexer(more)
      case(d::more) if d.isDigit => {
        val (d2,m) = extract(more, (count:Int,x:Char)=>x=='.' || x.isDigit)
        NumToken((d+:d2).mkString) +: lexer(m)
      }
      case('"'::more) => {
        val (d2,m) = extract(more, (count:Int,x:Char)=>x!='"')
        StrToken((d2).mkString) +: lexer(m.tail)
      }
      case(_::more) => lexer(more)
    }
  }

  /* GRAMMAR
   * -------
   *
   *  expr       ::= times_exp | times_expr plus_op expr
   *  times_expr ::= unary_exp | unary_expr times_op times_expr
   *  unary_expr ::= const | unary_op const
   *  plus_op    ::= "+" | "-" | "||"
   *  times_op   ::= "*" | "/" | "&&"
   *  unary_op   ::= "!" | "-"
   *  const      ::= "true" | "false" | number | string
   */

  def parser(l : List[Token]) : (Option[Expr], List[Token]) = {
    parseExpr(l)
  }

  //expr       ::= times_exp | times_expr plus_op expr
  def parseExpr(l : List[Token]) : (Option[Expr],List[Token]) = {
    parseExpr1(l) match {
      case (Some(e),l2) => (Some(e),l2)
        case _ => parseTimesExpr(l)
    }
  }

  def parseExpr1(l : List[Token]) : (Option[Expr],List[Token]) = {
    parseTimesExpr(l) match {
      case (Some(e1),l2) => parsePlusOp(l2) match {
        case (Some(op),l3) => parseExpr(l3) match {
          case (Some(e2),l4) => (Some(BopExpr(e1,op,e2)),l4)
            case _ => (None,l)
        }
        case _ => (None,l)
      }
      case _ => (None,l)
    }
  }

  def parsePlusOp(l : List[Token]) : (Option[Bop],List[Token]) = {
    l match {
      case OpToken("+")::more => (Some(PlusBop),more)
        case OpToken("-")::more => (Some(MinusBop),more)
          case OpToken("||")::more => (Some(OrBop),more)
            case _ => (None,l)
    }
  }

  def parseTimesOp(l : List[Token]) : (Option[Bop],List[Token]) = {
    l match {
      case OpToken("*")::more => (Some(TimesBop),more)
        case OpToken("/")::more => (Some(DivBop),more)
          case OpToken("&&")::more => (Some(AndBop),more)
            case _ => (None,l)
    }
  }

 //times_expr ::= unary_exp | unary_expr times_op times_expr
  def parseTimesExpr(l : List[Token]) : (Option[Expr],List[Token]) = {
    parseTimesExpr1(l) match {
      case (Some(e),l2) => (Some(e),l2)
        case (None,l3) => parseUnaryExpr(l3)
    }    
  }

  def parseTimesExpr1(l : List[Token]) : (Option[Expr],List[Token]) = {
    parseUnaryExpr(l) match {
      case (Some(e1),l2) => parseTimesOp(l2) match {
        case (Some(op),l3) => parseTimesExpr(l3) match {
          case (Some(e2),l4) => (Some(BopExpr(e1,op,e2)),l4)
            case _ => (None,l)
        }
        case _ => (None,l)
      }
      case _ => (None,l)
    }
  }

  def parseUnaryExpr(l : List[Token]) : (Option[Expr],List[Token]) = {
     parseUnaryExpr1(l) match {
       case (Some(op),l2) => (Some(op),l2)
          case (None,l3) => parseConst(l3)
       }

  }

  def parseUnaryExpr1(l : List[Token]) : (Option[Expr],List[Token]) = {
    parseUnaryOp(l) match {
      case (Some(op),l2) => parseConst(l2) match {
        case (Some(e2),l3) => (Some(UopExpr(op,e2)),l3)
          case _ => (None,l)
      }
      case _ => (None,l)
    }
  }

     
def parseUnaryOp(l : List[Token]) : (Option[Uop],List[Token]) = {
    l match {
      case OpToken("!")::more => (Some(NotUop),more)
        case OpToken("-")::more => (Some(NegUop),more)
            case _ => (None,l) 
    }
  }


  def parseConst(l : List[Token]) : (Option[Expr],List[Token]) = {
    l match {
      case KeywordToken("true")::more => (Some(ConstBoolExpr(true)),more)
        case KeywordToken("false")::more => (Some(ConstBoolExpr(false)),more)
          case NumToken(aNum)::more if aNum.forall(_.isDigit) => (Some(ConstIntExpr(aNum.toInt)),more)
            case NumToken(aNum)::more if !aNum.forall(_.isDigit) => (Some(ConstFloatExpr(aNum.toFloat)),more)
               case StrToken(aStr)::more => (Some(ConstStringExpr(aStr)),more)
                 case _ => (None,l)
    }
 
  }

}