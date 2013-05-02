import chapter8._

object chap8forall extends props {
  import Streams._
  def printRand(rand: Rand[Any]): String =
    rand.run(RNG.simple(1010))._1.toString        //> printRand: (rand: chap8forall.Rand[Any])String

  def printStream(s: Stream[Any]): String =
    s.take(6).mkString(",")                       //> printStream: (s: Stream[Any])String

  def printGen(gen: Gen[Any]) = gen match {
    case Gen(rand, exhaustive) =>
      val randStr = printRand(rand)
      val exStr = printStream(exhaustive)
      println(s"Gen($randStr,[$exStr])")
  }                                               //> printGen: (gen: chap8forall.Gen[Any])Unit

  def printProp(p: Prop) =
    p.run(100, 100, RNG.simple(1010))             //> printProp: (p: chap8forall.Prop)chap8forall.Result

  val smallIntList = Gen.listOfN2(3, Gen.choose(0, 2))
                                                  //> smallIntList  : chap8forall.Gen[List[Int]] = Gen(State(<function1>),Stream(S
                                                  //| ome(List(0, 0, 0)), ?))
  val intList = Gen.listOfN2(10, Gen.choose(0, 2))//> intList  : chap8forall.Gen[List[Int]] = Gen(State(<function1>),Stream(Some(L
                                                  //| ist(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)), ?))

  println(smallIntList.exhaustive.size)           //> 8

  println(intList.exhaustive.size)                //> 1024

  forAll(smallIntList)(l => l.reverse.reverse == l).run(100, 1000, RNG.simple(9))
                                                  //> res0: chap8forall.Result = Right((Proven,8))

  forAll(intList)(l => l.reverse.reverse == l).run(100, 1000, RNG.simple(9))
                                                  //> res1: chap8forall.Result = Right((Unfalsified,1000))

  forAll(smallIntList)(l => l.reverse == l).run(100, 1000, RNG.simple(9))
                                                  //> res2: chap8forall.Result = Left(List(0, 0, 1))

  forAll(intList)(l => l.reverse == l).run(100, 1000, RNG.simple(9))
                                                  //> res3: chap8forall.Result = Left(List(0, 0, 0, 0, 0, 0, 0, 0, 0, 1))

  val anyIntList = Gen.listOf(Gen.choose(0, 2))   //> anyIntList  : chap8forall.SGen[List[Int]] = SGen(<function1>)

  forAllSGen(anyIntList)(l => l.reverse.reverse == l).run(10, 100, RNG.simple(9))
                                                  //> res4: chap8forall.Result = Right((Unfalsified,563))

  forAllSGen(anyIntList)(l => l.reverse == l).run(100, 1000, RNG.simple(9))
                                                  //> res5: chap8forall.Result = Left(List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1))

  forAllSGen(anyIntList)(l => l.size < 5).run(10, 1000, RNG.simple(9))
                                                  //> res6: chap8forall.Result = Left(List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0))

  forAllSGen(anyIntList)(l => l.size < 5).run(10, 1000, RNG.simple(9))
                                                  //> res7: chap8forall.Result = Left(List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0))

  val smallInt = Gen.choose(-10, 10)              //> smallInt  : chap8forall.Gen[Int] = Gen(State(<function1>),Stream(Some(-10),
                                                  //|  ?))
  val maxProp = forAllSGen(Gen.listOf(smallInt)) { l =>
    val max = l.max
    !l.exists(_ > max)
  }                                               //> maxProp  : chap8forall.Prop = Prop(<function3>)
  
  printProp(maxProp)                              //> res8: chap8forall.Result = Left(test case: Stream(Some(List()))
                                                  //| generated an exception: empty.max
                                                  //| stack trace:
                                                  //| scala.collection.TraversableOnce$class.max(TraversableOnce.scala:202)
                                                  //| scala.collection.AbstractTraversable.max(Traversable.scala:105)
                                                  //| chap8forall$$anonfun$main$1$$anonfun$9.apply(chap8forall.scala:48)
                                                  //| chap8forall$$anonfun$main$1$$anonfun$9.apply(chap8forall.scala:47)
                                                  //| chapter8.props$$anonfun$forAll$1$$anonfun$chapter8$props$class$$anonfun$$go
                                                  //| $1$2.apply(props.scala:218)
                                                  //| chapter8.props$$anonfun$forAll$1$$anonfun$chapter8$props$class$$anonfun$$go
                                                  //| $1$2.apply(props.scala:218)
                                                  //| chapter8.props$class.attempt(props.scala:207)
                                                  //| chap8forall$.attempt(chap8forall.scala:3)
                                                  //| chapter8.props$$anonfun$forAll$1.chapter8$props$class$$anonfun$$go$1(props.
                                                  //| scala:217)
                                                  //| chapter8.props$$anonfun$forAll$1.apply(props.scala:227)
                                                  //| chapter8.props$$anonfun$forAll$1.apply(props.scala:210)
                                                  //| chapter8.props$Prop$$anonfun$$amp$am
                                                  //| Output exceeds cutoff limit.

}