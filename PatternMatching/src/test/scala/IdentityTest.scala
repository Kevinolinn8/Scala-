import org.scalatest.FlatSpec

// unit tests for the "identity" function
class IdentityTest extends FlatSpec {
  "Identity Function" should "return 1 when given 1 as input" in {
    val myInt = 1

    assert(Identity.identity(myInt) === myInt)
  }

  it should "return [1,2,3] when given [1,2,3] as input" in {
    val myList = List[Int](1,2,3)

    assert(Identity.identity(myList) === myList)
  }
}
