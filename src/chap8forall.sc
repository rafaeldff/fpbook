import chapter8._

object chap8forall extends props {
  import Streams._
  
  val smallIntList = Gen.listOfN2(3, Gen.choose(0,2))
                                                  //> smallIntList  : chap8forall.Gen[List[Int]] = Gen(State(<function1>),Stream(S
                                                  //| ome(List(0, 0, 0)), ?))
  val intList = Gen.listOfN2(10, Gen.choose(0,2)) //> intList  : chap8forall.Gen[List[Int]] = Gen(State(<function1>),Stream(Some(L
                                                  //| ist(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)), ?))
  
  println( smallIntList.exhaustive.size )         //> 8
  
  println( intList.exhaustive.size )              //> 1024
  
                                                    
  forAll(smallIntList)(l => l.reverse.reverse == l).run(100, 1000, RNG.simple(9))
                                                  //> res0: chap8forall.Result = Right((Proven,8))
                                                  
  forAll(intList)(l => l.reverse.reverse == l).run(100, 1000, RNG.simple(9))
                                                  //> res1: chap8forall.Result = Right((Unfalsified,1000))
    
    
    
  forAll(smallIntList)(l => l.reverse == l).run(100, 1000, RNG.simple(9))
                                                  //> res2: chap8forall.Result = Left(List(0, 0, 1))
                                                  
  forAll(intList)(l => l.reverse == l).run(100, 1000, RNG.simple(9))
                                                  //> res3: chap8forall.Result = Left(List(0, 0, 0, 0, 0, 0, 0, 0, 0, 1))
 
 
  val anyIntList = Gen.listOf(Gen.choose(0,2))    //> anyIntList  : chap8forall.SGen[List[Int]] = SGen(<function1>)
  
  forAllSGen(anyIntList)(l => l.reverse.reverse == l).run(10, 100, RNG.simple(9))
                                                  //> res4: chap8forall.Result = Right((Unfalsified,563))
  
  forAllSGen(anyIntList)(l => l.reverse == l).run(100, 1000, RNG.simple(9))
                                                  //> res5: chap8forall.Result = Left(List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1))
 
 
  forAllSGen(anyIntList)(l => l.size < 5).run(10, 1000, RNG.simple(9))
                                                  //> res6: chap8forall.Result = Left(List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
  
  forAllSGen(anyIntList)(l => l.size < 5).run(10, 1000, RNG.simple(9))
                                                  //> res7: chap8forall.Result = Left(List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
  
}