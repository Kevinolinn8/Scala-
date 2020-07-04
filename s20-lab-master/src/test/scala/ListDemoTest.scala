import org.scalatest.FlatSpec

// unit tests for the List functions 
class ListDemoTest extends FlatSpec {
  val ld = new ListDemo[Int]
  "Selection Sort" should "return a sorted list of five integers" in {
    assert(ld.selectionSort(List(3,2,4,1,5)) === List(1,2,3,4,5))
    assert(ld.selectionSort(List(10,2,4,1,5,0,-3)) === List(-3,0,1,2,4,5,10))
    assert(ld.selectionSort(List(0,50,25,5)) === List(0,5,25,50))
    assert(ld.selectionSort(List(9,9,6,6,3,3,2,2,7,7,8,8,1,1)) === List(1,1,2,2,3,3,6,6,7,7,8,8,9,9))
    assert(ld.selectionSort(List(-3,-9,-2,-6,-6)) === List(-9,-6,-6,-3,-2))
    assert(ld.selectionSort(List(100, 70, 120, 3, -18)) === List(-18,3,70,100,120))
  }

  "Map" should "work properly with a basic function" in {
    assert(ld.map(List(1,2,3,4,5),(x:Int)=>x+1) === List(2,3,4,5,6))
    assert(ld.map(List(1,2,3,4,5),(x:Int)=>x-1) === List(0,1,2,3,4))
    assert(ld.map(List(1,2,3,4,5),(x:Int)=>x+5) === List(6,7,8,9,10))
    assert(ld.map(List(4,5,2,6,1),(x:Int)=>x*1) === List(4,5,2,6,1))
    assert(ld.map(List(7,8,2,6,4,9,1,3),(x:Int)=>x+4) === List(11,12,6,10,8,13,5,7))
    assert(ld.map(List(2,4,6,8,10),(x:Int)=>x/2) === List(1,2,3,4,5))
  }

  it should "work properly with a basic integer-to-string conversion" in {
    assert(ld.map(List(1,2,3,4,5),(x:Int)=>"$"+x) === List("$1","$2","$3","$4","$5"))
    assert(ld.map(List(2,4,6,8,10),(x:Int)=>"Dollars"+x) === List("Dollars2","Dollars4","Dollars6","Dollars8","Dollars10"))
    assert(ld.map(List(0,1,2,3,4),(x:Int)=>"Number "+x) === List("Number 0","Number 1","Number 2","Number 3","Number 4"))
    assert(ld.map(List(10,20,30,40,50),(x:Int)=>"$"+x) === List("$10","$20","$30","$40","$50"))
    assert(ld.map(List(40,56,20,74,62,31,12),(x:Int)=>x+" Thousand") === List("40 Thousand","56 Thousand","20 Thousand","74 Thousand","62 Thousand", "31 Thousand", "12 Thousand"))
    assert(ld.map(List(1,2,3,4,5),(x:Int)=>"$"+x+",000") === List("$1,000","$2,000","$3,000","$4,000","$5,000"))
  }

  "FoldLeft" should "sum the list elements properly" in {
    assert(ld.foldLeft(List(1,2,3),123,(x:Int,y:Int)=>x+y) === 6 + 123)
    assert(ld.foldLeft(List(4,5,6),456,(x:Int,y:Int)=>x+y) === 15 + 456)
    assert(ld.foldLeft(List(9,9,9,9),9999,(x:Int,y:Int)=>x+y) === 36 + 9999)
    assert(ld.foldLeft(List(9,8,7,6,5,4,3,2,1),987654321,(x:Int,y:Int)=>x+y) === 45 + 987654321)
    assert(ld.foldLeft(List(1,1,1,1,1,1,1,1,1),111111111,(x:Int,y:Int)=>x+y) === 9 + 111111111)
    assert(ld.foldLeft(List(7,12,20,14),7122014,(x:Int,y:Int)=>x+y) === 53 + 7122014)
  }

  "FoldRight" should "sum the list elements properly" in {
    assert(ld.foldRight(List(1,2,3),123,(y:Int,x:Int)=>y+x) === 6 + 123)
    assert(ld.foldRight(List(10,10,10),101010,(y:Int,x:Int)=>y+x) === 30 + 101010)
    assert(ld.foldRight(List(5,6,7,8,9),98765,(y:Int,x:Int)=>y+x) === 35 + 98765)
    assert(ld.foldRight(List(1,4,1,5,1,6),141516,(y:Int,x:Int)=>y+x) === 18 + 141516)
    assert(ld.foldRight(List(14,15,16),141516,(y:Int,x:Int)=>y+x) === 45 + 141516)
    assert(ld.foldRight(List(1,45,21,52,0),14521520,(y:Int,x:Int)=>y+x) === 119 + 14521520)
  }

  "Filter" should "properly capture integers greater than a bound" in {
    assert(ld.filter(List(1,4,2,5,3,6),(x:Int)=>(x > 3)) === List(4,5,6))
    assert(ld.filter(List(6,4,9,8,6,4,2,2,6,4,8),(x:Int)=>(x > 8)) === List(9))
    assert(ld.filter(List(2,0,3,9,5,8,3,2,1,9,8,6,7,5,3),(x:Int)=>(x > 1)) === List(2,3,9,5,8,3,2,9,8,6,7,5,3))
    assert(ld.filter(List(-2,6,4,-9),(x:Int)=>(x > -9)) === List(-2,6,4))
    assert(ld.filter(List(1,4,9,5,7,2,9,3,5,9,7,6,2),(x:Int)=>(x > 7)) === List(9,9,9))
    assert(ld.filter(List(4,8,9,2,7,4),(x:Int)=>(x > 3)) === List(4,8,9,7,4))
  }

  "Reverse" should "properly reverse a list of integers" in {
    assert(ld.reverse(List(1,2,3,4)) === List(4,3,2,1))
    assert(ld.reverse(List(-5,-3,9,1,8,7)) === List(7,8,1,9,-3,-5))
    assert(ld.reverse(List(10,20,30,40,50,60,70,80,90)) === List(90,80,70,60,50,40,30,20,10))
    assert(ld.reverse(List(4,10,9,3,2,16,-3)) === List(-3,16,2,3,9,10,4))
    assert(ld.reverse(List(9,6,5,2,1,0)) === List(0,1,2,5,6,9))
    assert(ld.reverse(List(0,4,0,2,8,5,0,5,7,7,6,0,0,3,8,2,7,7,5,9,0,0,6,-1,8,4,9,6,8,9,9,4)) === List(4,9,9,8,6,9,4,8,-1,6,0,0,9,5,7,7,2,8,3,0,0,6,7,7,5,0,5,8,2,0,4,0))
  }
}