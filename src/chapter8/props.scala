package chapter8
import chapter6.StateMonad

trait props {
  import StateMonad._
  
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
    def choose(from:Int, to:Int):Gen[Int] = ???
    def listOf[A](gen: Gen[A]):Gen[List[A]] = ???
  }
  
  trait Gen[A]
  
  def forAll[A](ga: Gen[A])(f: A => Boolean): Prop = ???
}