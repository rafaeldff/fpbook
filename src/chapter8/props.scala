package chapter8
import chapter6.{StateMonad, Randoms}

trait props extends Randoms {
  import StateMonad._
  import Randoms._

  object Streams {
    def combinations[A](size: Int, chooseFrom: Stream[A]): Stream[Stream[A]] = {
      chooseFrom.permutations.toStream.map(_.take(size)).distinct
    }

    def clampDown[A](n: Int, s: Stream[A]): Stream[A] = {
      val firstN = s.take(n)
      if (firstN.size == n)
        s
      else
        Stream.continually(s).flatten.take(n)
    }
    
    def bounded[A](a:Stream[A]):Stream[Option[A]] =
      a map (Some(_))
  }
  
  
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
    import Streams._
    
    def unit[A](a: => A): Gen[A] =
      (StateMonad.unit(a), Stream(Some(a)))
    
    private def randomBoolean: Rand[Boolean] =
      nextInt.map {i => i % 2 == 0}
    
    def boolean: Gen[Boolean] = 
      (randomBoolean, bounded( Stream(true,false) ))
    
    private def randomInterval(from:Int, to:Int): Rand[Int] = { 
      val range = to - from
      nextDouble.map {d => ((d * range) + from).toInt }
    }
    
    def choose(from:Int, to:Int): Gen[Int] =  
      (randomInterval(from, to), bounded( Stream.from(from).take(to-from) ))

    private def between[A](n: Int, g: Gen[A]): Rand[List[A]] =
      if (n <= 0)
        StateMonad.unit(Nil)
      else {
        val ra  : Rand[A] = g._1
        val rest: Rand[List[A]] = between(n-1, g)
        StateMonad.map2(ra, rest) {(a, la) => a :: la}
      }
    
    
    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
      val exhaustiveElements: Option[Stream[A]] =
        if (g._2.exists{! _.isDefined})
          None
        else
          Some(g._2.map {_.get})
            
      
      val exhaustive = exhaustiveElements match {
        case None => Stream(None)
        case Some(elements) =>  combinations(n, clampDown(n, elements)).map{x => Some(x.toList)}
      }
      
      (between(n, g), exhaustive)
    }
    
    def listOf[A](gen: Gen[A]):Gen[List[A]] = ???
  }
  
  type Gen[A] = (State[RNG, A],  Stream[Option[A]])
  
  def forAll[A](ga: Gen[A])(f: A => Boolean): Prop = ???
}