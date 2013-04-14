import chapter8._

object chap8 extends props {
	import Streams._
	
  val propTrue = new Prop { def check = Right(1) }//> propTrue  : chap8.Prop{def check: scala.util.Right[Nothing,Int]} = chap8$$an
                                                  //| onfun$main$1$$anon$1@c69203
  val propFalse = new Prop { def check = Left("err") }
                                                  //> propFalse  : chap8.Prop{def check: scala.util.Left[String,Nothing]} = chap8$
                                                  //| $anonfun$main$1$$anon$2@17911a2d
  
  propTrue.check                                  //> res0: scala.util.Right[Nothing,Int] = Right(1)
  propFalse.check                                 //> res1: scala.util.Left[String,Nothing] = Left(err)
  
  
  (propTrue && propFalse).check                   //> res2: Either[chap8.FailedCase,chap8.SuccessCount] = Left(err)
  (propTrue && propTrue).check                    //> res3: Either[chap8.FailedCase,chap8.SuccessCount] = Right(1)
  
  
  //val intList = Gen.listOf(Gen.choose(0,100))
	//val prop =
	//forAll(intList)(l => l.reverse.reverse == l) &&
	//forAll(intList)(l => l.headOption == l.reverse.lastOption)
	//val failingProp = forAll(intList)(l => l.reverse == l)
 
 	def printGen(gen: Gen[Any]) = gen match {
 		case Gen(rand, exhaustive) =>
 			val randStr = rand.run(RNG.simple(1010))._1.toString
 			val exStr = exhaustive.take(6).mkString(",")
 			println(s"Gen($randStr,[$exStr])")
 	}                                         //> printGen: (gen: chap8.Gen[Any])Unit
	
  
  printGen( Gen.listOfN(3, Gen.choose(0, 10) ))   //> Gen(List(1, 5, 3),[Some(List(0, 1, 2)),Some(List(0, 1, 3)),Some(List(0, 1, 4
                                                  //| )),Some(List(0, 1, 5)),Some(List(0, 1, 6)),Some(List(0, 1, 7))])
                                                   
  
  traverse(List(Some(1), Some(2)))                //> res4: Option[List[Int]] = Some(List(1, 2))
  traverse(List(Some(1), None))                   //> res5: Option[List[Int]] = None
    
  
  printGen( Gen.uniform )                         //> Gen(0.1809542948291424,[None])
  printGen( Gen.choose(10000d,10001d) )           //> Gen(10000.180954294829,[None])
  printGen( Gen.listOfN(5, Gen.choose(10000d,10001d)) )
                                                  //> Gen(List(10000.180954294829, 10000.55689133264, 10000.396418477592, 10000.1
                                                  //| 42231085869, 10000.728413107678),[None])
  printGen( Gen.pair )                            //> Gen((388596389,1195915030),[None])
  printGen( Gen.pairOf(Gen.boolean, Gen.choose(1,3)) )
                                                  //> Gen((false,2),[Some((true,1)),Some((true,2)),Some((false,1)),Some((false,2)
                                                  //| )])
                                                  
  printGen( Gen.pairOf(Gen.uniform, Gen.boolean) )//> Gen((0.1809542948291424,true),[None,None])
  printGen( Gen.pairOf(Gen.boolean, Gen.uniform) )//> Gen((false,0.5568913326397964),[None,None])
  
  
  cross(Stream(Some(1)), Stream(Some(2))).mkString//> res6: String = Some((1,2))
  cross(Stream(Some(1),Some(10)), Stream(Some(2),Some(20))).mkString
                                                  //> res7: String = Some((1,2))Some((1,20))Some((10,2))Some((10,20))
  cross(Stream(Some(1),None), Stream(Some(2),Some(20))).mkString
                                                  //> res8: String = Some((1,2))Some((1,20))NoneNone
  
  /*
 
  
   
  */
}