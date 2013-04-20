import chapter8._

object chap8streams extends props {
  import Streams._

  val input = Gen.choose(0,3).exhaustive.force
  val clamped = clampDown(2, input)
  
 	//combinations(2, clamped).force
  
  val perms = clamped.permutations.take(30)
  //val prefixes = perms.map(_.take(n)).toStream.distinct
  //
  //
  //prefixes.take(10).force.map(_.force).mkString("\n")


  combinations(3, clamped).take(30).mkString("\n")

 
  0
}