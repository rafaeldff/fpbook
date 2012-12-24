object chap4 {
  /** Ex 1 */
  def map[B, A](o: Option[A])(f: A => B): Option[B] = o match { case None => None; case Some(a) => Some(f(a)) }
                                                  //> map: [B, A](o: Option[A])(f: A => B)Option[B]

  def getOrElse[B >: A, A](o: Option[A])(default: => B): B = o match {
    case None => default
    case Some(a) => a
  }                                               //> getOrElse: [B >: A, A](o: Option[A])(default: => B)B

  def flatMap[B, A](oa: Option[A])(f: A => Option[B]): Option[B] = getOrElse(map(oa)(f))(None)
                                                  //> flatMap: [B, A](oa: Option[A])(f: A => Option[B])Option[B]

  def orElse[B >: A, A](oa: Option[A])(ob: Option[B]): Option[B] = {
    val ooa: Option[Option[B]] = map[Option[B], A](oa)(x => Some(x))
    getOrElse(ooa)(ob)
  }                                               //> orElse: [B >: A, A](oa: Option[A])(ob: Option[B])Option[B]

  def filter[A](o: Option[A])(f: A => Boolean): Option[A] = flatMap(o)(x => if (f(x)) Some(x) else None)
                                                  //> filter: [A](o: Option[A])(f: A => Boolean)Option[A]

  val ops = List(Some(9), Some(42), None)         //> ops  : List[Option[Int]] = List(Some(9), Some(42), None)

  ops.map { o => map(o)(_ * 2) }                  //> res0: List[Option[Int]] = List(Some(18), Some(84), None)

  ops.map { o => getOrElse(o)(-1000) }            //> res1: List[Int] = List(9, 42, -1000)

  ops.map { o => flatMap(o) { a => if (a < 10) None else Some(a * 2) } }
                                                  //> res2: List[Option[Int]] = List(None, Some(84), None)

  ops.map { o => filter(o)(_ < 10) }              //> res3: List[Option[Int]] = List(Some(9), None, None)

  def mean(seq: Seq[Double]): Option[Double] = seq match {
    case Seq() => None
    case Seq(xs @ _*) => Some(xs.sum / xs.size)
  }                                               //> mean: (seq: Seq[Double])Option[Double]

  /** Ex 2 */
  def variance(seq: Seq[Double]): Option[Double] =
    mean(seq).flatMap { m: Double =>
      mean(seq.map { x => math.pow(x - m, 2) })
    }                                             //> variance: (seq: Seq[Double])Option[Double]

  val l = List[Double](2, 4, 2, 4, 2, 4)          //> l  : List[Double] = List(2.0, 4.0, 2.0, 4.0, 2.0, 4.0)

  mean(l)                                         //> res4: Option[Double] = Some(3.0)
  mean(Nil: List[Double])                         //> res5: Option[Double] = None

  variance(Nil: List[Double])                     //> res6: Option[Double] = None
  variance(l)                                     //> res7: Option[Double] = Some(1.0)

  /** Ex 3 */

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for (va <- a; vb <- b) yield f(va, vb)        //> map2: [A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C)Option[C]

  map2(Some(10), Some(20))(_ + _)                 //> res8: Option[Int] = Some(30)
  map2(None: Option[Int], Some(20))(_ + _)        //> res9: Option[Int] = None
  map2(Some(10), None: Option[Int])(_ + _)        //> res10: Option[Int] = None

  def mkMatcher(pat: String): Option[String => Boolean] = {
    import java.util.regex._
    def pattern(s: String): Option[Pattern] =
      try {
        Some(Pattern.compile(s))
      }
      catch {
        case e: PatternSyntaxException => None
      }
    pattern(pat) map (p => (s: String) => p.matcher(s).matches)
  }                                               //> mkMatcher: (pat: String)Option[String => Boolean]
  
    
  
  def bothMatch(pat: String, pat2: String, s: String): Option[Boolean] = {
    type M = String => Boolean
    val fn: (M,M)=>Boolean = {(m1:M, m2:M) => m1(s) && m2(s)}
		map2(mkMatcher(pat), mkMatcher(pat2))(fn)
  }                                               //> bothMatch: (pat: String, pat2: String, s: String)Option[Boolean]

	bothMatch("ab.*", ".*cd", "abcd")         //> res11: Option[Boolean] = Some(true)
	bothMatch("ab.*", "xyw", "abcd")          //> res12: Option[Boolean] = Some(false)
	bothMatch("???", "[^", "abcd")            //> res13: Option[Boolean] = None
	
	
  def sequence[T](ls:List[Option[T]]): Option[List[T]] =
    ls.foldRight(Some(Nil):Option[List[T]])((ox:Option[T], oys: Option[List[T]]) => (ox, oys) match {
      case (Some(x), Some(ys)) => Some(x :: ys)
      case _ => None: Option[List[T]]
    })                                            //> sequence: [T](ls: List[Option[T]])Option[List[T]]
 
 sequence(List(Option(1),Option(2)))              //> res14: Option[List[Int]] = Some(List(1, 2))
 sequence(List(Option(1)))                        //> res15: Option[List[Int]] = Some(List(1))
 sequence(List[Option[Int]]())                    //> res16: Option[List[Int]] = Some(List())
 sequence(List(None:Option[Int]))                 //> res17: Option[List[Int]] = None
 sequence(List(Some(10), None, Some(20)))         //> res18: Option[List[Int]] = None
}