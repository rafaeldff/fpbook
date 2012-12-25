object chap5 {
  trait Stream[+A] {
    def uncons: Option[(A, Stream[A])]
    def isEmpty: Boolean = uncons.isEmpty
    def toList: List[A] = this.uncons match {
    	case None => Nil
    	case Some((x, xs)) => x :: xs.toList
    }
    
    def take(n:Int): Stream[A] =
	    if (n <= 0)
	    	Stream.empty
	    else this.uncons match {
	    	case None => Stream.empty
	      case Some((x, xs)) => Stream.cons(x, xs.take(n-1))
	    }
	    
	  def foldRight[B](z: => B)(f: (A, => B) => B): B =
			uncons match {
				case Some((h, t)) => f(h, t.foldRight(z)(f))
				case None => z
			}
  }
  
  object Stream {
    def empty[A]: Stream[A] =
      new Stream[A] { def uncons = None }
      
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      new Stream[A] {
        lazy val uncons = Some((hd, tl))
      }

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty
      else cons(as.head, apply(as.tail: _*))
  }
  
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
  	f(z) match {
  		case None => Stream.empty
  		case x@Some((a,s)) => Stream.cons(a, unfold(s)(f))
  	}                                         //> unfold: [A, S](z: S)(f: S => Option[(A, S)])chap5.Stream[A]
  	
  	
  val ones = unfold(1)(_ => Some((1, 1)))         //> ones  : chap5.Stream[Int] = chap5$Stream$$anon$2@3595ecae
  ones.take(10).toList                            //> res0: List[Int] = List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
  
  def from(i:Int) = unfold(i)(x => Some((x+1, x+1)))
                                                  //> from: (i: Int)chap5.Stream[Int]
  
  
  from(3).take(10).toList                         //> res1: List[Int] = List(4, 5, 6, 7, 8, 9, 10, 11, 12, 13)
  
  
   
}