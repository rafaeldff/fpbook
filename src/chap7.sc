
object chap7 {
  trait Par[T]

  object Par {
    def unit[T](t: => T): Par[T] = ???
    def get[T](p: Par[T]): T = ???
    def map2[A, B, R](a: Par[A], b: Par[B])(f: (A,B)=>R): Par[R] = ???
  }

  def sum(as: IndexedSeq[Int]): Par[Int] =
    if (as.size <= 1) Par.unit(as.headOption getOrElse 0)
    else {
      val (l, r) = as.splitAt(as.length / 2)
      Par.map2(sum(l), sum(r))(_ + _)
    }                                             //> sum: (as: IndexedSeq[Int])chap7.Par[Int]
}