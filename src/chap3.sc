object chap3 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  def tail[T](l: List[T]): List[T] = l match {
    case Nil => Nil
    case x :: xs => xs
  }                                               //> tail: [T](l: List[T])List[T]
  
  def drop[T](l:List[T])(n:Int): List[T] = (l, n) match {
		case (Nil, _) => Nil
		case (x :: xs, n) if n > 0 =>  drop(xs)( n-1)
		case (x :: xs, 0) => x :: xs
  }                                               //> drop: [T](l: List[T])(n: Int)List[T]
  
  def dropWhile[T](l: List[T])(p: T => Boolean):List[T] = l match {
  	case Nil => Nil
  	case x :: xs if p(x) => dropWhile(xs)(p)
  	case _ => l
  }                                               //> dropWhile: [T](l: List[T])(p: T => Boolean)List[T]
  
  def setHead[T](l:List[T])(h:T) = l match {
  	case Nil => Nil
  	case x :: xs => h :: xs
  }                                               //> setHead: [T](l: List[T])(h: T)List[T]
  
  def foldRight[T,R](l:List[T])(z:R)(f: (T,R) => R):R = l match {
  	case Nil => z
  	case x :: xs => f(x, foldRight(xs)(z)(f))
  }                                               //> foldRight: [T, R](l: List[T])(z: R)(f: (T, R) => R)R
  
  def foldLeft[T,R](l:List[T])(z:R)(f: (R, T) => R ):R = l match {
  	case Nil => z
  	case x :: xs => foldLeft(xs)(f(z,x))(f)
  }                                               //> foldLeft: [T, R](l: List[T])(z: R)(f: (R, T) => R)R
  
  def foldLeft2[T,R](l:List[T])(z:R)(f: (R, T) => R ):R =  {
   foldRight(rev(l))(z){(a,b) => f(b,a)}
 }                                                //> foldLeft2: [T, R](l: List[T])(z: R)(f: (R, T) => R)R
  
  def length[T](l: List[T]):Int = foldLeft(l)(0) {(c,_) => c + 1}
                                                  //> length: [T](l: List[T])Int
                                                  
 
  // 1 :: (2 :: Nil))
  // (Nil f 1) f 2 -> 2 :: (1 :: Nil)
  def rev[T](l: List[T]):List[T] = foldLeft(l)(Nil:List[T]){(l, x) => x :: l}
                                                  //> rev: [T](l: List[T])List[T]
  
  def append[T](l1: List[T], l2: List[T]):List[T] = foldRight(l1)(l2)(_ :: _)
                                                  //> append: [T](l1: List[T], l2: List[T])List[T]
                                                  
  /* l = [1,2] :: ([3,4] :: Nil)append(
     l = Cons([1,2], Cons([3,4], Nil))
     append([1,2], append([3,4], Nil))
       append([3,4], Nil) -> append(Cons(3, Cons(4, Nil)), Nil) -> foldRight(Cons(3, Cons(4, Nil)))(Nil)(Cons) -> Cons(3, Cons(4, Nil)) => O(2)
     
     append([1,2], [3,4]) ->  O(2)
  */
  def appendAll[T](l: List[List[T]]):List[T] =
		foldRight(l)(Nil:List[T])(append _)
                                                  //> appendAll: [T](l: List[List[T]])List[T]
                                                  
 	def map[A,B](list: List[A])(f: A => B): List[B] =
		foldRight(list)(Nil:List[B]){(x, rl) => f(x) :: rl}
                                                  //> map: [A, B](list: List[A])(f: A => B)List[B]
                                                  
  def flatMap[A,B](list: List[A])(f: A => List[B]):List[B] =
  	foldRight(list)(Nil:List[B]){(x, rl) => append(f(x), rl)}
                                                  //> flatMap: [A, B](list: List[A])(f: A => List[B])List[B]
                                                  
  def filter[A](list: List[A])(p: A => Boolean): List[A] =
  	flatMap(list){(x:A) => if (p(x)) List(x) else Nil:List[A]}
                                                  //> filter: [A](list: List[A])(p: A => Boolean)List[A]
                          
  

  val l = (1 to 10).toList                        //> l  : List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  val l2 = (11 to 13).toList                      //> l2  : List[Int] = List(11, 12, 13)
  val ll = for (i <- (0 to 10).toList) yield (0 to i).toList
                                                  //> ll  : List[List[Int]] = List(List(0), List(0, 1), List(0, 1, 2), List(0, 1,
                                                  //|  2, 3), List(0, 1, 2, 3, 4), List(0, 1, 2, 3, 4, 5), List(0, 1, 2, 3, 4, 5,
                                                  //|  6), List(0, 1, 2, 3, 4, 5, 6, 7), List(0, 1, 2, 3, 4, 5, 6, 7, 8), List(0,
                                                  //|  1, 2, 3, 4, 5, 6, 7, 8, 9), List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
                                                  

	tail(l)                                   //> res0: List[Int] = List(2, 3, 4, 5, 6, 7, 8, 9, 10)
	tail(Nil)                                 //> res1: List[Nothing] = List()
	setHead(l)(100)                           //> res2: List[Int] = List(100, 2, 3, 4, 5, 6, 7, 8, 9, 10)
	
	(0 to 3).map {i => dropWhile(l){_ <= i}}  //> res3: scala.collection.immutable.IndexedSeq[List[Int]] = Vector(List(1, 2, 
                                                  //| 3, 4, 5, 6, 7, 8, 9, 10), List(2, 3, 4, 5, 6, 7, 8, 9, 10), List(3, 4, 5, 6
                                                  //| , 7, 8, 9, 10), List(4, 5, 6, 7, 8, 9, 10))
                                                  
	l.foldRight(1)(_ * _)                     //> res4: Int = 3628800
 	foldRight(l)(1)(_ * _)                    //> res5: Int = 3628800
 	foldRight(l)(Nil:List[Int])(_ :: _)       //> res6: List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
 	
 	l.foldLeft(1)(_ * _)                      //> res7: Int = 3628800
 	foldLeft(l)(1)(_ * _)                     //> res8: Int = 3628800
 	foldLeft(l)(Nil:List[Int]){(x,y) => y :: x}
                                                  //> res9: List[Int] = List(10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
 
                                                  
 	foldLeft2(l)(1)(_ * _)                    //> res10: Int = 3628800
 	foldLeft2(l)(Nil:List[Int]){(x,y) => y :: x}
                                                  //> res11: List[Int] = List(10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
                                                  
 	length(l)                                 //> res12: Int = 10
 	rev(l)                                    //> res13: List[Int] = List(10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
 	
 	l ::: l2                                  //> res14: List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)
 	append(l, l2)                             //> res15: List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)
 	appendAll(ll)                             //> res16: List[Int] = List(0, 0, 1, 0, 1, 2, 0, 1, 2, 3, 0, 1, 2, 3, 4, 0, 1, 
                                                  //| 2, 3, 4, 5, 0, 1, 2, 3, 4, 5, 6, 0, 1, 2, 3, 4, 5, 6, 7, 0, 1, 2, 3, 4, 5, 
                                                  //| 6, 7, 8, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
 	l.map {_ + 1}                             //> res17: List[Int] = List(2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
 	map(l){_ + 1}                             //> res18: List[Int] = List(2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
 
 	filter(l)(x => x > 3 && x < 10);          //> res19: List[Int] = List(4, 5, 6, 7, 8, 9)
 
 	sealed trait Tree[+A]
	case class Leaf[A](value: A) extends Tree[A]
	case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
	
	val t = Branch(Branch(Branch(Branch(Leaf(30),Leaf(40)),Leaf(2)),Leaf(1)),Branch(Leaf(42),Leaf(99)))
                                                  //> t  : chap3.Branch[Int] = Branch(Branch(Branch(Branch(Leaf(30),Leaf(40)),Lea
                                                  //| f(2)),Leaf(1)),Branch(Leaf(42),Leaf(99)))
  def size[T](tree:Tree[T]):Int = tree match {
  	case Leaf(_) => 1
  	case Branch (l,r) => 1 + size(l) + size(r)
  }                                               //> size: [T](tree: chap3.Tree[T])Int
  
  def maximum(tree:Tree[Int]):Int = tree match {
  	case Leaf(n) => n
  	case Branch(l:Tree[Int],r:Tree[Int]) => maximum(l) max maximum(r)
  }                                               //> maximum: (tree: chap3.Tree[Int])Int
  
  def depth[T](tree:Tree[T]):Int = tree match {
  	case Leaf(_) => 1
  	case Branch(l,r) => 1 + (depth(l) max depth(r))
  }                                               //> depth: [T](tree: chap3.Tree[T])Int
  
  def foldTree[A,B](tree:Tree[A])(f: (B,B) => B)(g: A => B):B  = tree match {
  	case Leaf(a) => g(a)
  	case Branch(l,r) => f(foldTree(l)(f)(g), foldTree(r)(f)(g))
  }                                               //> foldTree: [A, B](tree: chap3.Tree[A])(f: (B, B) => B)(g: A => B)B
  
  /*  Using folds */
  def size2[T](tree:Tree[T]):Int = foldTree[T,Int](tree)(1 + _ + _)(_ => 1)
                                                  //> size2: [T](tree: chap3.Tree[T])Int
  def maximum2(tree:Tree[Int]):Int = foldTree[Int,Int](tree)(_ max _)(identity _)
                                                  //> maximum2: (tree: chap3.Tree[Int])Int
	def depth2[T](tree:Tree[T]):Int = foldTree[T,Int](tree)(1 + _ max _)(_ => 1)
                                                  //> depth2: [T](tree: chap3.Tree[T])Int
  
 	List(size _, size2 _).map(_ apply t)      //> res20: List[Int] = List(11, 11)
  
  List(maximum _, maximum2 _).map(_ apply t)      //> res21: List[Int] = List(99, 99)
  
  List(depth _, depth2 _).map(_ apply t)          //> res22: List[Int] = List(5, 5)
}