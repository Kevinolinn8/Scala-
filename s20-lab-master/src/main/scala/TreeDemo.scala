class TreeDemo[T](implicit o : T => Ordered[T]) {
  // NOTE - Leaf(x) is now represented as Node(Empty,x,Empty)
  sealed trait BinaryTree
  case object Empty extends BinaryTree
  case class Node(left:BinaryTree, d:T, right:BinaryTree) extends BinaryTree
  //case class Leaf(d:T) extends BinaryTree

  // construct a "leaf" node
  def Leaf(d : T) : BinaryTree = Node(Empty,d,Empty)

  // in-order traversal of binary tree
  // Unit type for functions that don't return anything
  // returns the proper order for the traversal (as a list)
  def traverse(t : BinaryTree) : List[T] = t match {
    case Empty => Nil
    case Node(l, d, r) => traverse(l) ++ List(d) ++ traverse(r)
  }

  // remove all nodes equal to x from tree t
  def remove(t : BinaryTree, x : T) : BinaryTree = {
    replace(t, x, Empty)
  }

  def replace(t : BinaryTree, x : T, s : BinaryTree) : BinaryTree = {
    t match {
      case Empty => Empty
      case Node(l, d, r) => {
           if (d==x) {s}
           else if (Node(l,d,r)==Leaf(d)) {Leaf(d)}
           else {
            Node(replace(l,x,s),d,replace(r,x,s))
           }

      }
    }
  }

  // get the maximum of two elements
  def max(x : T, y : Option[T]) : T = {
    y match {
      case None => x
      case Some(z) => if(z > x) z else x
    }
  }

  // get the minimum of two elements
  def min(x : T, y : Option[T]) : T = {
    y match {
      case None => x
      case Some(z) => if(z < x) z else x
    }
  }

  def getMax(t : BinaryTree) : Option[T] = {
    var maxList = traverse(t)
    if (maxList.isEmpty) None else Some(maxList.last)
      
  }

  def getMin(t : BinaryTree) : Option[T] = {
    var minList = traverse(t)
    if (minList.isEmpty) None else Some(minList.head)
  }

  // TODO: check if t is a binary search tree
  def isBST(t : BinaryTree) : Boolean = {
      val bstList = traverse(t)
      val result = bstList.sorted
      if(bstList == result) {true}
      else{false}

  }


  // TODO: insert element x into binary search tree t
  def insertBST(t : BinaryTree, x : T) : BinaryTree = {
    t match {
      case Empty => Leaf(x)
      case Node(l,d,r) => {
        if (x <= d){
          Node(insertBST(l,x),d,r)

          //replace(Leaf(insertBST(l, x)),d,t)
        }
        else{
          Node(l,d,insertBST(r,x))
          //replace(t,d,Leaf(x))
        }
        
        
      }
    }
  }


  def searchBST(t : BinaryTree, x : T) : Boolean = {
    var myList = traverse(t)
    if (myList.contains(x)) {true}
    else {false}
  }
}