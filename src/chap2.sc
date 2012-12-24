object chap2 {

	/*
	 *  Chap 2
	 */
	def format(x:Any) = x match { case true => "T"; case false => "F"; case x:Int => "%3d" format x}
                                                  //> format: (x: Any)String
	
	type Pred[T] = T => Boolean
	
	//EXERCISE 10
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

  def liftU[A,B,C,D](f: B => C => D)(g: A => B, h: A =>  C): A => D =
    a => f(g(a))(h(a))                            //> liftU: [A, B, C, D](f: B => (C => D))(g: A => B, h: A => C)A => D
	  
	//EXERCISE 11
  def lift3wlift[A,B,C,D,E](f: (B,C,D) => E)(g: A=>B, h:A=>C, i:A=>D): A => E = {
		val ff: B => C => D => E = f.curried
		
		val lfA_DE : A => D => E = liftU(ff)(g,h)
		
		val lfA_E = liftU(lfA_DE)(identity[A], i)
		
		lfA_E
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
  
  
  (for (i <- 0 to 4) yield List(identity[Int] _, div3, div5, div3and5, div3and5_b).map(_.apply(i)).map(format)).map(println)
                                                  //> List(  0, T, T, T, T)
                                                  //| List(  1, F, F, F, F)
                                                  //| List(  2, F, F, F, F)
                                                  //| List(  3, T, F, F, F)
                                                  //| List(  4, F, F, F, F)
                                                  //| res2: scala.collection.immutable.IndexedSeq[Unit] = Vector((), (), (), (), 
                                                  //| ())
 
  //EXERCISE 12
  def trivialFib(n:Int):Int = n match {
    case 0 | 1 => n
    case _ => trivialFib(n-1) + trivialFib(n-2)
  }                                               //> trivialFib: (n: Int)Int
  
  def fib(n:Int):Int = {
    def loop(n:Int, x1:Int, x2:Int):Int = n match {
      case 0 => x1
      case 1 => x2
      case _ => loop(n-1, x2, x1 + x2)
    }
		loop(n, 0, 1)
  }                                               //> fib: (n: Int)Int
  
  (0 to 10).map (x => (trivialFib(x), fib(x))) foreach (println)
                                                  //> (0,0)
                                                  //| (1,1)
                                                  //| (1,1)
                                                  //| (2,2)
                                                  //| (3,3)
                                                  //| (5,5)
                                                  //| (8,8)
                                                  //| (13,13)
                                                  //| (21,21)
                                                  //| (34,34)
                                                  //| (55,55)

}