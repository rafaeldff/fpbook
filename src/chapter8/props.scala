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
    private def randomInterval(from:Int, to:Int): Rand[Int] = { 
      val range = to - from
      nextDouble.map {d => ((d * range) + from).toInt }
    }
    
    def choose(from:Int, to:Int): Gen[Int] =  
      randomInterval(from, to)
    
    def listOf[A](gen: Gen[A]):Gen[List[A]] = ???
  }
  
  type Gen[A] = State[RNG, A]
  
  def forAll[A](ga: Gen[A])(f: A => Boolean): Prop = ???
}