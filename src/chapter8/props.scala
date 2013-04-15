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
    
    def interleave[A](s1: Stream[A], s2: Stream[A]): Stream[A] =
      s1 match {
        case Stream.Empty => s2
        case x #:: xs => x #:: interleave(s2, xs)
      }
    
    def bounded[A](a:Stream[A]):Stream[Option[A]] =
      a map (Some(_))
    
    def unbounded[A]: Stream[Option[A]] = Stream(None)
  }
  
  trait Status
  case object Proven extends Status
  case object Unfalsified extends Status
  
  type SuccessCount = Int
  type FailedCase = String

  type TestCases = Int
  type Result = Either[FailedCase, (Status,SuccessCount)]
  case class Prop(run: (TestCases,RNG) => Result) { self =>
    def &&(other: Prop): Prop = ???/*new Prop {
      def check = self.check match {
        case Left(failedCase) => Left(failedCase) 
        case Right(successCount) => other.check
      }
    }*/
    
  }
  
  object Gen {
    import Streams._
    
    def unit[A](a: => A): Gen[A] =
      Gen(StateMonad.unit(a), Stream(Some(a)))
      
    def map2[A,B,C](ga: Gen[A], gb: Gen[B])(f: (A,B)=>C): Gen[C] = 
      for (a <- ga; b <- gb) yield f(a,b)
    
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
      map2(ga, gb){(a,b) => (a,b)}
    
    def uniform: Gen[Double] = 
      Gen(nextDouble, unbounded)
      
    def choose(i: Double, j:Double): Gen[Double] =
      for (d <- uniform) yield i + d*(j-i)
      
    def sameParity(from:Int, to:Int): Gen[(Int,Int)] = 
      pairOf(choose(from,to), choose(from,to)).map{ case (n,m) =>
        if (n % 2 == m % 2)
          (n,m)
        else
          (n, m+1)
      }
    
    def listOfN2[A](n: Int, g: Gen[A]): Gen[List[A]] = {
      if (n <=0) 
        unit(Nil:List[A])
      else
        map2(g, listOfN2(n-1, g)) {_ :: _}
    }
    
    def mix[A](r: Rand[A], s:Rand[A]):Rand[A] = 
      randomBoolean.flatMap {
        case true  => r
        case false => s
      }
      
      
    def union[A](g1: Gen[A], g2: Gen[A]):Gen[A] = {
      val sample = mix(g1.sample, g2.sample)
      val exhaustive = interleave(g1.exhaustive, g2.exhaustive)
      Gen(sample, exhaustive)
    }
    
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
    
    def listOfN(size: Gen[Int]): Gen[List[A]] =
      size.flatMap { n => Gen.listOfN(n, this)}
  }
  
  def buildMsg[A](s: A, e: Exception): String =
    "test case: " + s + "\n" +
    "generated an exception: " + e.getMessage + "\n" +
    "stack trace:\n" + e.getStackTrace.mkString("\n")
    
  /* Produce an infinite random stream from a `Gen` and a starting `RNG`. */ 
  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = {
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
      f(z) match {
        case None => Stream.empty
        case x@Some((a,s)) => a #:: unfold(s)(f)
      }
    
    unfold(rng)(rng => Some(g.sample.run(rng)))
  }


  //------------   
  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) =>
      {
        def go(i: Int, j: Int, s: Stream[Option[A]], onEnd: Int => Result): Result =
          if (i == j) Right((Unfalsified, i))
          else s match {
            case Some(h) #:: t =>
              try {
                if (f(h)) go(i + 1, j, s, onEnd)
                else Left(h.toString)
              }
              catch { case e: Exception => Left(buildMsg(h, e)) }
            case None #:: _ => Right((Unfalsified, i))
            case Stream.Empty => onEnd(i)
          }
        go(0, n / 3, a.exhaustive, i => Right((Proven, i))) match {
          case Right((Unfalsified, _)) =>
            val rands = randomStream(a)(rng).map(Some(_))
            go(n / 3, n, rands, i => Right((Unfalsified, i)))
          case s => s
        }
      }
  }
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
 //------------   
}