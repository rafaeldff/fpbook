import chapter8._

object chap8 extends props {
	import Streams._
	
  val propTrue = new Prop { def check = Right(1) }//> propTrue  : chap8.Prop{def check: scala.util.Right[Nothing,Int]} = chap8$$an
                                                  //| onfun$main$1$$anon$1@3210a146
  val propFalse = new Prop { def check = Left("err") }
                                                  //> propFalse  : chap8.Prop{def check: scala.util.Left[String,Nothing]} = chap8$
                                                  //| $anonfun$main$1$$anon$2@42d0850
  
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
 		case (rand, exhaustive) =>
 			val randStr = rand.run(RNG.simple(1010))._1.toString
 			val exStr = exhaustive.take(11).mkString(",")
 			println(s"$randStr [$exStr]")
 	}                                         //> printGen: (gen: (chapter6.StateMonad.State[chap8.RNG,Any], Stream[Option[Any
                                                  //| ]]))Unit
	
  
  printGen( Gen.boolean )                         //> false [Some(true),Some(false)]
 
 	printGen( Gen.choose(100, 200) )          //> 118 [Some(100),Some(101),Some(102),Some(103),Some(104),Some(105),Some(106),S
                                                  //| ome(107),Some(108),Some(109),Some(110)]

 	printGen( Gen.listOfN(5, Gen.boolean) )   //> List(false, true, true, false, false) [Some(List(true, true, true, false, fa
                                                  //| lse)),Some(List(true, true, false, true, false)),Some(List(true, true, false
                                                  //| , false, true)),Some(List(true, false, true, true, false)),Some(List(true, f
                                                  //| alse, true, false, true)),Some(List(true, false, false, true, true)),Some(Li
                                                  //| st(false, true, true, true, false)),Some(List(false, true, true, false, true
                                                  //| )),Some(List(false, true, false, true, true)),Some(List(false, false, true, 
                                                  //| true, true))]
 
  printGen( Gen.listOfN(3, Gen.choose(0, 10) ))   //> List(1, 5, 3) [Some(List(0, 1, 2)),Some(List(0, 1, 3)),Some(List(0, 1, 4)),S
                                                  //| ome(List(0, 1, 5)),Some(List(0, 1, 6)),Some(List(0, 1, 7)),Some(List(0, 1, 8
                                                  //| )),Some(List(0, 1, 9)),Some(List(0, 2, 1)),Some(List(0, 2, 3)),Some(List(0, 
                                                  //| 2, 4))]
                                                   
   
  traverse(List(None))                            //> res4: Option[List[Nothing]] = None
  
  traverse(List(Some(1), Some(2)))                //> res5: Option[List[Int]] = Some(List(1, 2))
  traverse(List(Some(1), None))                   //> res6: Option[List[Int]] = None
  traverse(List(Some(1), None, Some(2)))          //> res7: Option[List[Int]] = None
    
  
  printGen( Gen.uniform )                         //> 0.1809542948291424 [None]
  printGen( Gen.choose(20d,100d) )                //> 34.476343586331396 [None]
                                                  
  printGen( Gen.choose(10000d,10001d) )           //> 10000.180954294829 [None]
  ()
}