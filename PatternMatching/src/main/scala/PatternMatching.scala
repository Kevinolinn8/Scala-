object PatternMatching {

  // sealed means all possible extending classes (Note, Rest, ...)
  // are defined in this source file -- this lets the compiler
  // enumerate all possibilities for classes which extend Symbol


  /*
   * abstract data type for binary trees
   */

  sealed trait Tree
  case class Node(left:Tree, d:Int, right:Tree) extends Tree
  case class Leaf(d:Int) extends Tree

  // in-order traversal of binary tree
  // Unit type for functions that don't return anything
  def traverse(t : Tree) : Unit = t match {
    case Node(l, d, r) => {
      traverse(l)
      println(d)
      traverse(r)
    }
    case Leaf(d) => println(d)
  }

  // returns the proper order for the In-Order traversal (as a list)
  def traverse2(t : Tree) : List[Int] = t match {
    case Node(l, d, r) => {
      traverse2(l) ++ List(d) ++ traverse2(r)
    }
    case Leaf(d) => List(d)
  }

  // returns the proper order for the Pre-Order traversal (as a list)
  def traverse2Pre(t : Tree) : List[Int] = t match{
      case Node(l, d, r) => {
        List(d) ++ traverse2Pre(l) ++ traverse2Pre(r)
      }
      case Leaf(d) => List(d)
  }

  // returns the proper order for the Post-Order traversal (as a list)
  def traverse2Post(t : Tree) : List[Int] = t match{
      case Node( l, d, r) => {
      traverse2Post(l) ++ traverse2Post(r) ++ List(d)
      }
      case Leaf(d) => List(d)
  }

  /*
   this encodes the following tree:
   4
   / \
   2   5
   / \
   1   3
   */

  val myTree = Node(Node(Node(Leaf(1),2,Leaf(3)),4,Leaf(5)),6,Node(Leaf(7),8,Leaf(9)))
  val v = traverse2(myTree) 
  val vPre1 = traverse2Pre(myTree) 
  val vPost1 = traverse2Post(myTree)

  val myTree2 = Node(Node(Leaf(9),8,Leaf(7)),6,Node(Leaf(5),4,(Node(Leaf(3),2,Leaf(1)))))
  val v2 = traverse2(myTree2) 
  val vPre2 = traverse2Pre(myTree2)  
  val vPost2 = traverse2Post(myTree2) 

  val myTree3 = Node(Node(Leaf(5),6,Leaf(7)),8,Leaf(9))
  val v3 = traverse2(myTree3) 
  val vPre3 = traverse2Pre(myTree3) 
  val vPost3 = traverse2Post(myTree3) 

  val myTree4 = Node(Node(Leaf(9),8,Leaf(7)),6,Node(Leaf(5),4,Leaf(3)))
  val v4 = traverse2(myTree4) 
  val vPre4 = traverse2Pre(myTree4) 
  val vPost4 = traverse2Post(myTree4) 

  val myTree5 = Node(Node(Leaf(5),6,Node(Leaf(8),7,Leaf(9))),10,Leaf(11))
  val v5 = traverse2(myTree5) 
  val vPre5 = traverse2Pre(myTree5) 
  val vPost5 = traverse2Post(myTree5) 


}
