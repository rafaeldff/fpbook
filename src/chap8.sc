import chapter8._

object chap8 extends props {
	
  val propTrue = new Prop { def check = Right(1) }//> propTrue  : chap8.Prop{def check: scala.util.Right[Nothing,Int]} = chap8$$an
                                                  //| onfun$main$1$$anon$1@31059033
  val propFalse = new Prop { def check = Left("err") }
                                                  //> propFalse  : chap8.Prop{def check: scala.util.Left[String,Nothing]} = chap8$
                                                  //| $anonfun$main$1$$anon$2@539e922d
 
 
  
  propTrue.check                                  //> res0: scala.util.Right[Nothing,Int] = Right(1)
  propFalse.check                                 //> res1: scala.util.Left[String,Nothing] = Left(err)
  
  
  (propTrue && propFalse).check                   //> res2: Either[chap8.FailedCase,chap8.SuccessCount] = Left(err)
  (propTrue && propTrue).check                    //> res3: Either[chap8.FailedCase,chap8.SuccessCount] = Right(1)
  
  
  //val intList = Gen.listOf(Gen.choose(0,100))
  //
  //
  //
  //
  //
  //
  //
  //
  //
  //
  //
	//val prop =
	//forAll(intList)(l => l.reverse.reverse == l) &&
	//forAll(intList)(l => l.headOption == l.reverse.lastOption)
	//val failingProp = forAll(intList)(l => l.reverse == l)
 
 	def printGen(gen: Gen[Any]) = gen match {
 		case (rand, exhaustive) =>
 			val randStr = rand.run(RNG.simple(1010))._1.toString
 			val exStr = exhaustive.take(10).mkString(",")
 			println(s"$randStr [$exStr]")
 	}                                         //> printGen: (gen: (chapter6.StateMonad.State[chap8.RNG,Any], Stream[Any]))Unit
                                                  //| 
	
 
  
  printGen( Gen.boolean )                         //> false [true,false]
 
 	printGen( Gen.choose(100, 200) )          //> 118 [100,101,102,103,104,105,106,107,108,109]

 	printGen( Gen.listOfN(5, Gen.boolean) )   //> List(false, true, true, false, false) [List(true, true, true, false, false),
                                                  //| List(true, true, false, true, false),List(true, true, false, false, true),Li
                                                  //| st(true, false, true, true, false),List(true, false, true, false, true),List
                                                  //| (true, false, false, true, true),List(false, true, true, true, false),List(f
                                                  //| alse, true, true, false, true),List(false, true, false, true, true),List(fal
                                                  //| se, false, true, true, true)]
 
  printGen( Gen.listOfN(3, Gen.choose(0, 10 )) )  //> List(1, 5, 3) [List(0, 1, 2),List(0, 2, 1),List(1, 0, 2),List(1, 2, 0),List(
                                                  //| 2, 0, 1),List(2, 1, 0)]
  def p[T](s: Stream[Stream[T]]) = s.take(10).map(_.take(10).force).force
                                                  //> p: [T](s: Stream[Stream[T]])scala.collection.immutable.Stream[scala.collect
                                                  //| ion.immutable.Stream[T]]
                                                    
  p( Gen.combinations(1, Stream(1)) )             //> res4: scala.collection.immutable.Stream[scala.collection.immutable.Stream[I
                                                  //| nt]] = Stream(Stream(1))
  p( Gen.combinations(1, Stream(1,2)) )           //> res5: scala.collection.immutable.Stream[scala.collection.immutable.Stream[I
                                                  //| nt]] = Stream(Stream(1), Stream(2))
  
  p( Gen.combinations(2, Stream(1,2)) )           //> res6: scala.collection.immutable.Stream[scala.collection.immutable.Stream[I
                                                  //| nt]] = Stream(Stream(1, 2), Stream(2, 1))
                                                  
  p( Gen.combinations(3, Stream.from(1).take(10)) )
                                                  //> res7: scala.collection.immutable.Stream[scala.collection.immutable.Stream[I
                                                  //| nt]] = Stream(Stream(1, 2, 3), Stream(1, 3, 2), Stream(2, 1, 3), Stream(2, 
                                                  //| 3, 1), Stream(3, 1, 2), Stream(3, 2, 1), Stream(2, 3, 4), Stream(2, 4, 3), 
                                                  //| Stream(3, 2, 4), Stream(3, 4, 2))
                                                    
}