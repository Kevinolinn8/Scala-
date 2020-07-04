// sum(1) = 2
// sum(2) = 2 + 4
// sum(3) = 2 + 4 + 8
// sum(4) = 2 + 4 + 8 + 16 = 2*(1 + (2 + 4 + 8))
object Sum {
  
  def power(n: Int):Int = n match {
  	case 1 => 2
  	case _ => 2 * power(n-1)
  }


  def sum(n : Int):Int = n match {
  	case 0 => 0
  	case _ => power(n) + sum(n-1)
    
    
  }



  var one = 1
  var two = 2
  var three = 3
  var four = 4
}
