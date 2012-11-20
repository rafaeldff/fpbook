object chap4 {
	def map[B,A](o:Option[A])(f: A => B): Option[B] = o match { case None => None; case Some(a) => Some(f(a))}
                                                  
                                                  
	def getOrElse[B>:A,A](o:Option[A])(default: => B): B = o match {
		case None => default
		case Some(a) => a
	}
	
	def flatMap[B,A](oa:Option[A])(f: A => Option[B]): Option[B] = getOrElse(map(oa)(f))(None)
                                                  
                                                  
	def orElse[B>:A,A](oa:Option[A])(ob: Option[B]): Option[B]   = {
	  val oa2: Option[Option[A]] = flatMap(oa)(Some(_))
	}
                                                  
	def filter[A](o:Option[A])(f: A => Boolean): Option[A] = flatMap(o)(x => if (f(x)) Some(x) else None)
                                                  
	
	val ops = List(Some(9),Some(42),None)
	
	ops.map {o => map(o)(_ * 2) }
	
	ops.map {o => getOrElse(o)(-1000) }
	
	ops.map {o => flatMap(o){a => if (a < 10) None else Some(a * 2)}}
                                                  
 	ops.map {o => filter(o)(_ < 10)}
}