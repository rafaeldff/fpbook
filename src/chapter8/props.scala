package chapter8
import chapter6.{StateMonad, Randoms}

trait props extends Randoms {
  import StateMonad._
  import Randoms._
  
  
  type SuccessCount = Int
  type FailedCase = String

  trait Prop { self =>
    def check: Either[FailedCase, SuccessCount]
    def &&(other: Prop): Prop = new Prop {
      def check = self.check match {
        case Left(failedCase) => Left(failedCase) 
        case Right(successCount) => other.check
      }
      
    }
  }
  
  object Gen {
    def unit[A](a: => A): Gen[A] =
      (StateMonad.unit(a), Stream(a))
    
    private def randomBoolean: Rand[Boolean] =
      nextInt.map {i => i % 2 == 0}
    
    def boolean: Gen[Boolean] = (randomBoolean, Stream(true,false))
    
    private def randomInterval(from:Int, to:Int): Rand[Int] = { 
      val range = to - from
      nextDouble.map {d => ((d * range) + from).toInt }
    }
    
    def choose(from:Int, to:Int): Gen[Int] =  
      (randomInterval(from, to), Stream.from(from).take(to-from))

    private def between[A](n: Int, g: Gen[A]): Rand[List[A]] =
      if (n <= 0)
        StateMonad.unit(Nil)
      else {
        val ra  : Rand[A] = g._1
        val rest: Rand[List[A]] = between(n-1, g)
        StateMonad.map2(ra, rest) {(a, la) => a :: la}
      }
    
      
    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
      val exaustiveN: Stream[A] = 
        Stream.continually(g._2).flatten.take(n)
      val permutations: Iterator[List[A]] = exaustiveN.permutations.map(_.toList)
      (between(n, g), permutations.toStream)
    }
    
    def listOf[A](gen: Gen[A]):Gen[List[A]] = ???
  }
  
  type Gen[A] = (State[RNG, A],  Stream[A])
  
  def forAll[A](ga: Gen[A])(f: A => Boolean): Prop = ???
}