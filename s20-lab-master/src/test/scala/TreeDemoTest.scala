import org.scalatest.FlatSpec

// unit tests for the Tree functions 
class TreeDemoTest extends FlatSpec {
    val td = new TreeDemo[Int]
    import td._
    /*
    this encodes the following tree:
      4
     / \
    2   5
    / \
   1   3
    */

    val myTree = Node(Node(Leaf(1),2,Leaf(3)),4,Leaf(5))
    val myTree2 = Node(Node(Leaf(1),2,Leaf(3)),4,Node(Leaf(5),6,(Node(Leaf(7),8,Leaf(9)))))
    val myTree3 = Node(Node(Leaf(5),6,Leaf(7)),8,Leaf(9))
    val myTree4 = Node(Node(Leaf(3),4,Leaf(5)),6,Node(Leaf(7),8,Leaf(9)))
    val myTree5 = Node(Node(Leaf(5),6,Node(Leaf(7),8,Leaf(9))),10,Leaf(11))

    
    
    "Traverse" should "do in-order traversal for a small tree containing integers" in {
        assert(td.traverse(myTree) === List(1,2,3,4,5))
        assert(td.traverse(myTree2) === List(1,2,3,4,5,6,7,8,9))
        assert(td.traverse(myTree3) === List(5,6,7,8,9))
        assert(td.traverse(myTree4) === List(3,4,5,6,7,8,9))
        assert(td.traverse(myTree5) === List(5,6,7,8,9,10,11))
    }

    it should "do in-order traversal for a small tree containing strings" in {
        /*
        this encodes the following tree:
          "four"
         /      \
        "two"   "five"
        /   \
     "one"  "three"
        */

        val td2 = new TreeDemo[String]
        import td2._
        val myStrTree = Node(Node(Leaf("one"),"two",Leaf("three")),"four",Leaf("five"))
        assert(td2.traverse(myStrTree) === List("one","two","three","four","five"))
    }

    "Remove" should "properly remove the left subtree in the integer example" in {
        assert(td.remove(myTree, 2) === Node(Empty,4,Leaf(5)))
        assert(td.remove(myTree2, 2) === Node(Empty,4,Node(Leaf(5),6,(Node(Leaf(7),8,Leaf(9))))))
        assert(td.remove(myTree3, 6) === Node(Empty,8,Leaf(9)))
        assert(td.remove(myTree4, 4) === Node(Empty,6,Node(Leaf(7),8,Leaf(9))))
        assert(td.remove(myTree5, 6) === Node(Empty,10,Leaf(11)))
    }

    "Replace" should "properly replace the left subtree in the integer example" in {
        assert(td.replace(myTree, 2, myTree) === Node(myTree,4,Leaf(5)))
        assert(td.replace(myTree2, 2, myTree2) === Node(myTree2,4,Node(Leaf(5),6,(Node(Leaf(7),8,Leaf(9))))))
        assert(td.replace(myTree3, 6, myTree3) === Node(myTree3,8,Leaf(9)))
        assert(td.replace(myTree4, 4, myTree4) === Node(myTree4,6,Node(Leaf(7),8,Leaf(9))))
        assert(td.replace(myTree5, 6, myTree5) === Node(myTree5,10,Leaf(11)))
    }

    "Is BST" should "give the correct answer for integer example" in {
        assert(td.isBST(myTree) === true)
        assert(td.isBST(myTree2) === true)
        assert(td.isBST(myTree3) === true)
        assert(td.isBST(myTree4) === true)
        assert(td.isBST(myTree5) === true)
    }

    it should "give the correct answer for simple non-BST" in {
        assert(td.isBST(td.replace(myTree, 3, Leaf(7))) === false)
        assert(td.isBST(td.replace(myTree2, 3, Leaf(7))) === false)
        assert(td.isBST(td.replace(myTree3, 9, Leaf(7))) === false)
        assert(td.isBST(td.replace(myTree4, 3, Leaf(7))) === false)
        assert(td.isBST(td.replace(myTree5, 5, Leaf(7))) === false)
    }


    "max" should "gives the correct maximum integer from the binary tree" in {
        assert(td.getMax(myTree).get == 5)
        assert(td.getMax(myTree2).get == 9)
        assert(td.getMax(myTree3).get == 9)
        assert(td.getMax(myTree4).get == 9)
        assert(td.getMax(myTree5).get == 11)
    }

     "min" should "gives the correct minimum integer from the binary tree" in {
        assert(td.getMin(myTree).get == 1)
        assert(td.getMin(myTree2).get == 1)
        assert(td.getMin(myTree3).get == 5)
        assert(td.getMin(myTree4).get == 3)
        assert(td.getMin(myTree5).get == 5)
    }



    "BST Insert" should "give the correct answer for integer example" in {
        assert(td.insertBST(myTree,6) === td.replace(myTree,5,Node(Empty,5,Leaf(6))))
        assert(td.insertBST(myTree2,12) === td.replace(myTree2,9,Node(Empty,9,Leaf(12))))
        assert(td.insertBST(myTree3,1) === td.replace(myTree3,5,Node(Leaf(1),5,Empty)))
        assert(td.insertBST(myTree4,10) === td.replace(myTree4,9,Node(Empty,9,Leaf(10))))
        assert(td.insertBST(myTree5,4) === td.replace(myTree5,5,Node(Leaf(4),5,Empty)))
        assert(td.insertBST(myTree,-10) === td.replace(myTree,1,Node(Leaf(-10),1,Empty)))
    }



    "BST Search" should "give the correct answer for non-existant element" in {
        assert(td.searchBST(myTree,6) === false)
        assert(td.searchBST(myTree2,15) === false)
        assert(td.searchBST(myTree3,16) === false)
        assert(td.searchBST(myTree4,17) === false)
        assert(td.searchBST(myTree5,18) === false)
    }

    it should "give the correct answer for integer example" in {
        assert(td.searchBST(myTree,2) === true)
        assert(td.searchBST(myTree2,1) === true)
        assert(td.searchBST(myTree3,5) === true)
        assert(td.searchBST(myTree4,3) === true)
        assert(td.searchBST(myTree5,11) === true)
    }
}