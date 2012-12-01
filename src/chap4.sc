object chap4 {
 	/** Ex 1 */
	def map[B,A](o:Option[A])(f: A => B): Option[B] = o match { case None => None; case Some(a) => Some(f(a))}
                                                  //> map: [B, A](o: Option[A])(f: A => B)Option[B]
                                                  
	def getOrElse[B>:A,A](o:Option[A])(default: => B): B = o match {
		case None => default
		case Some(a) => a
	}                                         //> getOrElse: [B >: A, A](o: Option[A])(default: => B)B
	
	def flatMap[B,A](oa:Option[A])(f: A => Option[B]): Option[B] = getOrElse(map(oa)(f))(None)
                                                  //> flatMap: [B, A](oa: Option[A])(f: A => Option[B])Option[B]
                                                  
                                                  
	def orElse[B>:A,A](oa:Option[A])(ob: Option[B]): Option[B] = {
	  val ooa: Option[Option[B]] = map[Option[B],A](oa)(x => Some(x))
		getOrElse(ooa)(ob)
	}                                         //> orElse: [B >: A, A](oa: Option[A])(ob: Option[B])Option[B]
                                                  
	def filter[A](o:Option[A])(f: A => Boolean): Option[A] = flatMap(o)(x => if (f(x)) Some(x) else None)
                                                  //> filter: [A](o: Option[A])(f: A => Boolean)Option[A]
                                                  
	
	val ops = List(Some(9),Some(42),None)     //> ops  : List[Option[Int]] = List(Some(9), Some(42), None)
	
	ops.map {o => map(o)(_ * 2) }             //> res0: List[Option[Int]] = List(Some(18), Some(84), None)
	
	ops.map {o => getOrElse(o)(-1000) }       //> res1: List[Int] = List(9, 42, -1000)
	
	ops.map {o => flatMap(o){a => if (a < 10) None else Some(a * 2)}}
                                                  //> res2: List[Option[Int]] = List(None, Some(84), None)
                                                  
 	ops.map {o => filter(o)(_ < 10)}          //> res3: List[Option[Int]] = List(Some(9), None, None)
 	
 	
 	def mean(seq: Seq[Double]): Option[Double] = seq match {
 		case Seq() 				=> None
 		case Seq(xs @ _*) => Some(xs.sum / xs.size)
 	}                                         //> mean: (seq: Seq[Double])Option[Double]
 	
 	/** Ex 2 */
 	def variance(seq: Seq[Double]): Option[Double] =
 		mean(seq).flatMap {m:Double =>
 			mean(seq.map {x => math.pow(x - m,2)})
 		}                                 //> variance: (seq: Seq[Double])Option[Double]
 		
 		
 	val l = List[Double](2,4,2,4,2,4)         //> l  : List[Double] = List(2.0, 4.0, 2.0, 4.0, 2.0, 4.0)
 	
 	mean(l)                                   //> res4: Option[Double] = Some(3.0)
 	mean(Nil:List[Double])                    //> res5: Option[Double] = None
 	
 	variance(Nil:List[Double])                //> res6: Option[Double] = None
 	variance(l)                               //> res7: Option[Double] = Some(1.0)
 	
}