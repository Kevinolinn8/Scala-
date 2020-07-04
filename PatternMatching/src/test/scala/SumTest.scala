import org.scalatest.FlatSpec

// unit tests for pattern matching example
class SumTest extends FlatSpec {
  "Sum" should "return the correct answer for small numbers" in {

    assert(Sum.sum(Sum.one) == 2)
  
    assert(Sum.sum(Sum.two) === 6)
 
    assert(Sum.sum(Sum.three) === 14)
  
    assert(Sum.sum(Sum.four) === 30)

    
  }
}
