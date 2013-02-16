
object chap7 {
	trait Par[T]
	
	object Par {
		def unit[T](t: => T): Par[T] = ???
		def get[T](p: Par[T]):T = ???
	}
	
	
  def sum(as: IndexedSeq[Int]): Int =
    if (as.size <= 1) as.headOption getOrElse 0
    else {
      val (l, r) = as.splitAt(as.length / 2)
      val sumL: Par[Int] = Par.unit(sum(l))
      val sumR: Par[Int] = Par.unit(sum(r))
      Par.get(sumL) + Par.get(sumR)
    }                                             //> sum: (as: IndexedSeq[Int])Int
    
}