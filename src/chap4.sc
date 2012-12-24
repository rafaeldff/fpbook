object chap4 {
  /** Ex 1 */
  def map[B, A](o: Option[A])(f: A => B): Option[B] = o match { case None => None; case Some(a) => Some(f(a)) }

  def getOrElse[B >: A, A](o: Option[A])(default: => B): B = o match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B, A](oa: Option[A])(f: A => Option[B]): Option[B] = getOrElse(map(oa)(f))(None)

  def orElse[B >: A, A](oa: Option[A])(ob: Option[B]): Option[B] = {
    val ooa: Option[Option[B]] = map[Option[B], A](oa)(x => Some(x))
    getOrElse(ooa)(ob)
  }

  def filter[A](o: Option[A])(f: A => Boolean): Option[A] = flatMap(o)(x => if (f(x)) Some(x) else None)

  val ops = List(Some(9), Some(42), None)

  ops.map { o => map(o)(_ * 2) }

  ops.map { o => getOrElse(o)(-1000) }

  ops.map { o => flatMap(o) { a => if (a < 10) None else Some(a * 2) } }

  ops.map { o => filter(o)(_ < 10) }

  def mean(seq: Seq[Double]): Option[Double] = seq match {
    case Seq() => None
    case Seq(xs @ _*) => Some(xs.sum / xs.size)
  }

  /** Ex 2 */
  def variance(seq: Seq[Double]): Option[Double] =
    mean(seq).flatMap { m: Double =>
      mean(seq.map { x => math.pow(x - m, 2) })
    }

  val l = List[Double](2, 4, 2, 4, 2, 4)

  mean(l)
  mean(Nil: List[Double])

  variance(Nil: List[Double])
  variance(l)

  /** Ex 3 */

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for (va <- a; vb <- b) yield f(va, vb)

  map2(Some(10), Some(20))(_ + _)
  map2(None: Option[Int], Some(20))(_ + _)
  map2(Some(10), None: Option[Int])(_ + _)

  import java.util.regex._
  def pattern(s: String): Option[Pattern] =
    try {
      Some(Pattern.compile(s))
    }
    catch {
      case e: PatternSyntaxException => None
    }
      
  def mkMatcher(pat: String): Option[String => Boolean] = {
    pattern(pat) map (p => (s: String) => p.matcher(s).matches)
  }
  
    
  
  def bothMatch(pat: String, pat2: String, s: String): Option[Boolean] = {
    type M = String => Boolean
    val fn: (M,M)=>Boolean = {(m1:M, m2:M) => m1(s) && m2(s)}
		map2(mkMatcher(pat), mkMatcher(pat2))(fn)
  }

	bothMatch("ab.*", ".*cd", "abcd")
	bothMatch("ab.*", "xyw", "abcd")
	bothMatch("???", "[^", "abcd")
	
	
  def sequence[T](ls:List[Option[T]]): Option[List[T]] =
    ls.foldRight(Some(Nil):Option[List[T]])((ox:Option[T], oys: Option[List[T]]) => (ox, oys) match {
      case (Some(x), Some(ys)) => Some(x :: ys)
      case _ => None: Option[List[T]]
    })
 
  sequence(List(Option(1),Option(2)))
  sequence(List(Option(1)))
  sequence(List[Option[Int]]())
  sequence(List(None:Option[Int]))
  sequence(List(Some(10), None, Some(20)))
 
  def parsePatterns(a:List[String]): Option[List[Pattern]] =
    sequence(a map pattern)
    
  parsePatterns(List("abcd", "^$"))
  parsePatterns(List())
  parsePatterns(List("abcd", "["))
  
  /** Ex 6 */
  def traverse[A,B](l: List[A])(f: A => Option[B]): Option[List[B]] = {
    l.foldRight(Some(Nil):Option[List[B]]) { (a, obs) => (f(a), obs) match {
      case (Some(b), Some(bs)) => Some(b :: bs)
      case _ => None
    }}
  }
  
  def sequence2[T](a: List[Option[T]]): Option[List[T]] =
    traverse[Option[T],T](a)(identity _)

  sequence2(List(Option(1),Option(2)))
  sequence2(List(Option(1)))
  sequence2(List[Option[Int]]())
  sequence2(List(None:Option[Int]))
  sequence2(List(Some(10), None, Some(20)))
    
  
  def parsePatterns2(a:List[String]): Option[List[Pattern]] = traverse(a)(pattern)
  
  parsePatterns2(List("abcd", "^$"))
  parsePatterns2(List())
  parsePatterns2(List("abcd", "["))

	 
}
