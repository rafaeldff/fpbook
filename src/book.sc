object book {

	// Chap 2
	
	def format(x:Any) = x match { case true => "T"; case false => "F"; case x:Int => "%3d" format x}
                                                  //> format: (x: Any)String
	
	type Pred[T] = T => Boolean
	
  def lift[A,B,C,D](f: (B,C) => D)(g: A => B, h: A =>  C): A => D =
    a => f(g(a),h(a))                             //> lift: [A, B, C, D](f: (B, C) => D)(g: A => B, h: A => C)A => D
    
    
  val div3: Pred[Int] = _ % 3 == 0                //> div3  : Int => Boolean = <function1>
  
  val div5: Pred[Int] = _ % 5 == 0                //> div5  : Int => Boolean = <function1>
  
  val div3and5: Pred[Int] =
    lift((_:Boolean) && (_:Boolean))(div3, div5)  //> div3and5  : Int => Boolean = <function1>
    
  val liftedAnd = lift[Int, Boolean, Boolean, Boolean](_ && _) _
                                                  //> liftedAnd  : (Int => Boolean, Int => Boolean) => Int => Boolean = <function2
                                                  //| >
                                                  
  val div3and5_b = liftedAnd(div3, div5)          //> div3and5_b  : Int => Boolean = <function1>
  
  def lift3[A,B,C,D,E](f: (B,C,D) => E)(g: A=>B, h:A=>C, i:A=>D): A => E =
    a => f(g(a),h(a),i(a))                        //> lift3: [A, B, C, D, E](f: (B, C, D) => E)(g: A => B, h: A => C, i: A => D)A 
                                                  //| => E

	def curry3_2[B,C,D,E](f: (B,C,D)=>E): (B,C) => D => E =
	  (b,c) => d => f(b,c,d)                  //> curry3_2: [B, C, D, E](f: (B, C, D) => E)(B, C) => D => E

  def liftU[A,B,C,D](f: B => C => D)(g: A => B, h: A =>  C): A => D =
    a => f(g(a))(h(a))                            //> liftU: [A, B, C, D](f: B => (C => D))(g: A => B, h: A => C)A => D
	  
	  
  def lift3wlift[A,B,C,D,E](f: (B,C,D) => E)(g: A=>B, h:A=>C, i:A=>D): A => E = {
		val ff: B => C => D => E = f.curried
		
		val lfAp: A => D => E = liftU(ff)(g,h)
		
		liftU(lfAp)(identity[A], i)
  }                                               //> lift3wlift: [A, B, C, D, E](f: (B, C, D) => E)(g: A => B, h: A => C, i: A =
                                                  //| > D)A => E


  val dec: Int => Int = _ - 1                     //> dec  : Int => Int = <function1>
  val nop: Int => Int = identity[Int]             //> nop  : Int => Int = <function1>
  val inc: Int => Int = _ + 1                     //> inc  : Int => Int = <function1>
  
	val concat = lift3[Int,Int,Int,Int,String](_ + " " + _ + " " + _)(dec,nop,inc)
                                                  //> concat  : Int => String = <function1>
	val concat_wlift = lift3wlift[Int,Int,Int,Int,String](_ + " " + _ + " " + _)(dec,nop,inc)
                                                  //> concat_wlift  : Int => String = <function1>
	
	concat(10)                                //> res0: String = 9 10 11
	concat_wlift(10)                          //> res1: String = 9 10 11
  
  
  //(for (i <- 0 to 4) yield List(identity[Int] _, div3, div5, div3and5, div3and5_b).map(_.apply(i)).map(format)).map(println)
                                                  
                                                  0
                                                  //> res2: Int(0) = 0

}