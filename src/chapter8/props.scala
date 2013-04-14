package chapter8
import chapter6.{StateMonad, Randoms}

trait props extends Randoms {
  import StateMonad._
  import Randoms._

  object Streams {
    def combinations[A](size: Int, chooseFrom: Stream[Option[A]]): Stream[List[Option[A]]] = {
      chooseFrom.permutations.toStream.map(_.take(size).toList).distinct
    }

    def clampDown[A](n: Int, s: Stream[A]): Stream[A] = {
      val firstN = s.take(n)
      if (firstN.size == n)
        s
      else
        Stream.continually(s).flatten.take(n)
    }
    
    def traverse[A](loa: List[Option[A]]):Option[List[A]] = 
      loa.foldRight(Some(Nil):Option[List[A]]) {(oa, ola) => (oa, ola) match {
        case (Some(a), Some(la)) => Some(a :: la)
        case _ => None
      }}
    
    def cross[A,B](soa: Stream[Option[A]], sob: Stream[Option[B]] ): Stream[Option[(A,B)]] =
      for (oa <- soa; ob <- sob) yield (oa, ob) match {
        case (Some(a), Some(b)) => Some((a, b))
        case _ => None
      }
      
    
    def bounded[A](a:Stream[A]):Stream[Option[A]] =
      a map (Some(_))
    
    def unbounded[A]: Stream[Option[A]] = Stream(None)
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
      Gen(StateMonad.unit(a), Stream(Some(a)))
    
    private def randomBoolean: Rand[Boolean] =
      nextInt.map {i => i % 2 == 0}
    
    def boolean: Gen[Boolean] = 
      Gen(randomBoolean, bounded( Stream(true,false) ))
    
    private def randomInterval(from:Int, to:Int): Rand[Int] = { 
      val range = to - from
      nextDouble.map {d => ((d * range) + from).toInt }
    }
    
    def choose(from:Int, to:Int): Gen[Int] =  
      Gen(randomInterval(from, to), bounded( Stream.from(from).take(to-from) ))

    private def between[A](n: Int, g: Gen[A]): Rand[List[A]] =
      if (n <= 0)
        StateMonad.unit(Nil)
      else {
        val ra  : Rand[A] = g.sample
        val rest: Rand[List[A]] = between(n-1, g)
        StateMonad.map2(ra, rest) {(a, la) => a :: la}
      }
    
    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
      val input: Stream[Option[A]] = g.exhaustive
      val exhaustive = combinations(n, clampDown(n, input)).map(traverse _)
      
      Gen(between(n, g), exhaustive)
    }
    
    def int: Gen[Int] =
      Gen(nextInt, unbounded)
    
    def pair: Gen[(Int,Int)] = 
      pairOf(int, int)
      
    def pairOf[A,B](ga: Gen[A], gb:Gen[B]): Gen[(A,B)] =
      for (a <- ga; b <- gb) yield (a,b)
    
    def uniform: Gen[Double] = 
      Gen(nextDouble, unbounded)
      
    def choose(i: Double, j:Double): Gen[Double] =
      for (d <- uniform) yield i + d*(j-i)
    
    def listOf[A](gen: Gen[A]):Gen[List[A]] = ???
  }
  
  case class Gen[+A](sample: State[RNG, A],  exhaustive:Stream[Option[A]]) {
    def map[B](f: A => B): Gen[B] = {
      val sampleB = sample.map(f)
      val exhaustiveB = exhaustive.map {
        case None => None
        case Some(a) => Some(f(a))
      }
      Gen(sampleB, exhaustiveB)
    }
    
    def flatMap[B](f: A => Gen[B]): Gen[B] = {
      val sampleB = sample.flatMap (f andThen (_.sample))
      val exhaustiveB = exhaustive.flatMap{
        case None => Stream(None) 
        case Some(a) => f(a).exhaustive
      }
      Gen(sampleB, exhaustiveB)
    }
  }

  def forAll[A](ga: Gen[A])(f: A => Boolean): Prop = ???
}