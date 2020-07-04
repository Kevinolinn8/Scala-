import org.scalatest.FlatSpec

// unit tests for pattern matching example
class PatternMatchingTest extends FlatSpec {

  it should "return the proper tree-traversal order (In-Order)" in {
    assert(PatternMatching.traverse2(PatternMatching.myTree) === List(1,2,3,4,5,6,7,8,9))
    assert(PatternMatching.traverse2(PatternMatching.myTree2) === List(9,8,7,6,5,4,3,2,1))
    assert(PatternMatching.traverse2(PatternMatching.myTree3) === List(5,6,7,8,9))
    assert(PatternMatching.traverse2(PatternMatching.myTree4) === List(9,8,7,6,5,4,3))
    assert(PatternMatching.traverse2(PatternMatching.myTree5) === List(5,6,8,7,9,10,11))


  }

  it should "return the proper tree-traversal order (Pre-Order)" in {
    assert(PatternMatching.traverse2Pre(PatternMatching.myTree) === List(6,4,2,1,3,5,8,7,9))
    assert(PatternMatching.traverse2Pre(PatternMatching.myTree2) === List(6,8,9,7,4,5,2,3,1))
    assert(PatternMatching.traverse2Pre(PatternMatching.myTree3) === List(8,6,5,7,9))
    assert(PatternMatching.traverse2Pre(PatternMatching.myTree4) === List(6,8,9,7,4,5,3))
    assert(PatternMatching.traverse2Pre(PatternMatching.myTree5) === List(10,6,5,7,8,9,11))



  }

  it should "return the proper tree-traversal order (Post-Order)" in {
    assert(PatternMatching.traverse2Post(PatternMatching.myTree) === List(1,3,2,5,4,7,9,8,6))
    assert(PatternMatching.traverse2Post(PatternMatching.myTree2) === List(9,7,8,5,3,1,2,4,6))
    assert(PatternMatching.traverse2Post(PatternMatching.myTree3) === List(5,7,6,9,8))
    assert(PatternMatching.traverse2Post(PatternMatching.myTree4) === List(9,7,8,5,3,4,6))
    assert(PatternMatching.traverse2Post(PatternMatching.myTree5) === List(5,8,9,7,6,11,10))
  






  }
}
