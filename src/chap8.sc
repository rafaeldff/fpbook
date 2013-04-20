import chapter8._

object chap8 extends props {
  import Streams._
  def printRand(rand: Rand[Any]): String =
    rand.run(RNG.simple(1010))._1.toString        //> printRand: (rand: chap8.Rand[Any])String

  def printStream(s: Stream[Any]): String =
    s.take(6).mkString(",")                       //> printStream: (s: Stream[Any])String

  def printGen(gen: Gen[Any]) = gen match {
    case Gen(rand, exhaustive) =>
      val randStr = printRand(rand)
      val exStr = printStream(exhaustive)
      println(s"Gen($randStr,[$exStr])")
  }                                               //> printGen: (gen: chap8.Gen[Any])Unit
	
	
  //val propTrue = new Prop { def check = Right(1) }
  //val propFalse = new Prop { def check = Left("err") }
  //
  //propTrue.check
  //propFalse.check
  
  
  //(propTrue && propFalse).check
  //(propTrue && propTrue).check
  
  
  //val intList = Gen.listOf(Gen.choose(0,100))
  val intList = Gen.listOfN2(10, Gen.choose(0,2)) //> intList  : chap8.Gen[List[Int]] = Gen(State(<function1>),Stream(Some(List(0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0)), ?))
                                                  
	val prop1  =	forAll(intList)(l => l.reverse.reverse == l)
                                                  //> prop1  : chap8.Prop = Prop(<function2>)
  println( intList.exhaustive.size )              //> 1024
                                                    
  prop1.run(1000, RNG.simple(9))                  //> res0: chap8.Result = Right((Unfalsified,1000))
  
  
  
	val prop2 = forAll(intList)(l => l.headOption == l.reverse.lastOption)
                                                  //> prop2  : chap8.Prop = Prop(<function2>)
	val failingProp = forAll(intList)(l => l.reverse == l)
                                                  //> failingProp  : chap8.Prop = Prop(<function2>)
 

  
  printGen( Gen.listOfN(3, Gen.choose(0, 10) ))   //> Gen(List(1, 5, 3),[Some(List(0, 1, 2)),Some(List(0, 1, 3)),Some(List(0, 1, 
                                                  //| 4)),Some(List(0, 1, 5)),Some(List(0, 1, 6)),Some(List(0, 1, 7))])
                                                   
  
  traverse(List(Some(1), Some(2)))                //> res1: Option[List[Int]] = Some(List(1, 2))
  traverse(List(Some(1), None))                   //> res2: Option[List[Int]] = None
    
  
  printGen( Gen.uniform )                         //> Gen(0.1809542948291424,[None])
  printGen( Gen.choose(10000d,10001d) )           //> Gen(10000.180954294829,[None])
  printGen( Gen.listOfN(5, Gen.choose(10000d,10001d)) )
                                                  //> Gen(List(10000.180954294829, 10000.55689133264, 10000.396418477592, 10000.1
                                                  //| 42231085869, 10000.728413107678),[None])
  printGen( Gen.pair )                            //> Gen((388596389,1195915030),[None])
  printGen( Gen.pairOf(Gen.boolean, Gen.choose(1,3)) )
                                                  //> Gen((false,2),[Some((true,1)),Some((true,2)),Some((false,1)),Some((false,2)
                                                  //| )])
                                                  
  printGen( Gen.pairOf(Gen.uniform, Gen.boolean) )//> Gen((0.1809542948291424,true),[None])
  printGen( Gen.pairOf(Gen.boolean, Gen.uniform) )//> Gen((false,0.5568913326397964),[None,None])
  printGen( Gen.pairOf(Gen.uniform, Gen.choose(10d,11d)) )
                                                  //> Gen((0.1809542948291424,10.556891332639797),[None])
  printGen( Gen.sameParity(10,100) )              //> Gen((26,60),[Some((10,10)),Some((10,12)),Some((10,12)),Some((10,14)),Some((
                                                  //| 10,14)),Some((10,16))])
  
  printGen( Gen.boolean.listOfN(Gen.choose(5,15)) )
                                                  //> Gen(List(true, true, false, false, true, false),[Some(List(true, true, true
                                                  //| , false, false)),Some(List(true, true, false, true, false)),Some(List(true,
                                                  //|  true, false, false, true)),Some(List(true, false, true, true, false)),Some
                                                  //| (List(true, false, true, false, true)),Some(List(true, false, false, true, 
                                                  //| true))])
                                                  
  printGen( Gen.listOfN(5, Gen.uniform) )         //> Gen(List(0.1809542948291424, 0.5568913326397964, 0.39641847759318466, 0.142
                                                  //| 23108586959127, 0.7284131076784865),[None])
  printGen( Gen.listOfN2(5, Gen.uniform) )        //> Gen(List(0.1809542948291424, 0.5568913326397964, 0.39641847759318466, 0.142
                                                  //| 23108586959127, 0.7284131076784865),[None])
                                                  
  printRand( Gen.mix(Gen.unit(1).sample, Gen.unit(10).sample) )
                                                  //> res3: String = 10
                                                  
  printStream( interleave(Stream(1,2,3), Stream(10,20,30)) )
                                                  //> res4: String = 1,10,2,20,3,30
  
  printStream( interleave(Stream.from(1), Stream.from(100)) )
                                                  //> res5: String = 1,100,2,101,3,102
                                                  
  printStream( interleave(Stream(1,2), Stream.from(100)) )
                                                  //> res6: String = 1,100,2,101,102,103
                                                  
  printGen( Gen.union(Gen.choose(0,50), Gen.choose(100,150)) )
                                                  //> Gen(127,[Some(0),Some(100),Some(1),Some(101),Some(2),Some(102)])
  /*
 
   
      
  */
}