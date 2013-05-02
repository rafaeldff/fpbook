import chapter8._

object chap8streams extends props {
  import Streams._

  val input = Gen.choose(0,3).exhaustive.force    //> input  : scala.collection.immutable.Stream[Option[Int]] = Stream(Some(0), So
                                                  //| me(1), Some(2))
  val clamped = clampDown(2, input)               //> clamped  : Stream[Option[Int]] = Stream(Some(0), Some(1), Some(2))
  
 	//combinations(2, clamped).force
  
  val perms = clamped.permutations.take(30)       //> perms  : Iterator[scala.collection.immutable.Stream[Option[Int]]] = non-empt
                                                  //| y iterator
  //val prefixes = perms.map(_.take(n)).toStream.distinct
  //
  //
  //prefixes.take(10).force.map(_.force).mkString("\n")


  combinations(3, clamped).take(30).mkString("\n")//> res0: String = Stream(Some(0), ?)
                                                  //| Stream(Some(1), ?)
                                                  //| Stream(Some(2), ?)
                                                  //| Stream(Some(0), ?)
                                                  //| Stream(Some(1), ?)
                                                  //| Stream(Some(2), ?)
                                                  //| Stream(Some(0), ?)
                                                  //| Stream(Some(1), ?)
                                                  //| Stream(Some(2), ?)
                                                  //| Stream(Some(0), ?)
                                                  //| Stream(Some(1), ?)
                                                  //| Stream(Some(2), ?)
                                                  //| Stream(Some(0), ?)
                                                  //| Stream(Some(1), ?)
                                                  //| Stream(Some(2), ?)
                                                  //| Stream(Some(0), ?)
                                                  //| Stream(Some(1), ?)
                                                  //| Stream(Some(2), ?)
                                                  //| Stream(Some(0), ?)
                                                  //| Stream(Some(1), ?)
                                                  //| Stream(Some(2), ?)
                                                  //| Stream(Some(0), ?)
                                                  //| Stream(Some(1), ?)
                                                  //| Stream(Some(2), ?)
                                                  //| Stream(Some(0), ?)
                                                  //| Stream(Some(1), ?)
                                                  //| Stream(Some(2), ?)

 
  0                                               //> res1: Int(0) = 0
}