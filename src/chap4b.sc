object chap4b {
	/** Ex 7 */
  trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this.flatMap(a => Right(f(a)))
    
    def orElse[EE >: E, B >: A](b: Either[EE, B]): Either[EE, B] =
      this match {
        case Left(e) => b
        case Right(a) => this
      }
    
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Left(x) => Left(x)
      case Right(y) => f(y)
    }
    
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = (this, b) match {
      case (Right(va), Right(vb)) => Right(f(va, vb))
      case (Right(va), Left(e)) => Left(e)
      case (Left(e), Right(vb)) => Left(e)
      case (Left(e), Left(ee)) => Left(ee)
    }
  }
  
  case class Left[+E](value: E) extends Either[E, Nothing]
	case class Right[+A](value: A) extends Either[Nothing, A]

	/* Ex 8*/
  def traverseEither[T, E, R, EE](l:List[T])(f: T => Either[EE,R]): Either[EE, List[R]] =
		l.foldRight(Right(Nil):Either[EE,List[R]]) { (t, ers) => (f(t), ers) match {
		  case (Right(r), Right(rs)) => Right(r :: rs)
		  case (Left(ee), _) => Left(ee)
		  case (_, Left(ee)) => Left(ee)
		}}                                //> traverseEither: [T, E, R, EE](l: List[T])(f: T => chap4b.Either[EE,R])chap4
                                                  //| b.Either[EE,List[R]]

	val fe = (x:Int) => if (x <= 0) Left("Negative") else Right(12 / x)
                                                  //> fe  : Int => Product with Serializable with chap4b.Either[String,Int] = <fu
                                                  //| nction1>

  traverseEither(List[Int]())(fe)                 //> res0: chap4b.Either[String,List[Int]] = Right(List())
  traverseEither(List(1, 2, 3, 4, 6))(fe)         //> res1: chap4b.Either[String,List[Int]] = Right(List(12, 6, 4, 3, 2))
  traverseEither(List(1, 2, 3, 0, 4, 6))(fe)      //> res2: chap4b.Either[String,List[Int]] = Left(Negative)
  traverseEither(List(0, -1, 0, -10))(fe)         //> res3: chap4b.Either[String,List[Int]] = Left(Negative)
	
	
	def sequenceEither[T, E](l: List[Either[E,T]]): Either[E,List[T]] =
		traverseEither(l)(identity _)     //> sequenceEither: [T, E](l: List[chap4b.Either[E,T]])chap4b.Either[E,List[T]]
                                                  //| 
 sequenceEither(List(Right(1), Right(2)))         //> res4: chap4b.Either[Nothing,List[Int]] = Right(List(1, 2))
 sequenceEither(List(Right(1)))                   //> res5: chap4b.Either[Nothing,List[Int]] = Right(List(1))
 sequenceEither(List())                           //> res6: chap4b.Either[Nothing,List[Nothing]] = Right(List())
 
 sequenceEither(List(Right(1), Right(2), Left("error")))
                                                  //> res7: chap4b.Either[String,List[Int]] = Left(error)
                                                  
  case class Person(name: Name, age: Age)
  case class Name(val value: String)
  case class Age(val value: Int)
  
      
  type AccumEither[E, T] = Either[List[E], T]
  def map2Accum[A,B,C,E](a: AccumEither[E, A])(b: AccumEither[E, B])(f: (A,B) => C): AccumEither[E, C] =
    (a, b) match {
      case (Right(va), Right(vb)) => Right(f(va, vb))
      case (Right(va), Left(e)) => Left(e)
      case (Left(e), Right(vb)) => Left(e)
      case (Left(e), Left(ee)) => Left(e ::: ee)
    }                                             //> map2Accum: [A, B, C, E](a: chap4b.AccumEither[E,A])(b: chap4b.AccumEither[E
                                                  //| ,B])(f: (A, B) => C)chap4b.AccumEither[E,C]
    
  def accumLeft[E](e:E): AccumEither[E, Nothing] = Left(List(e))
                                                  //> accumLeft: [E](e: E)chap4b.AccumEither[E,Nothing]
  def accumRight[R](r:R): AccumEither[Nothing, R] = Right(r)
                                                  //> accumRight: [R](r: R)chap4b.AccumEither[Nothing,R]
  def mkName(name: String): AccumEither[String, Name] =
    if (name == "" || name == null) accumLeft("Name is empty.")
    else accumRight(new Name(name))               //> mkName: (name: String)chap4b.AccumEither[String,chap4b.Name]
    
  def mkAge(age: Int): AccumEither[String, Age] =
    if (age < 0) accumLeft("Age is out of range.")
    else accumRight(new Age(age))                 //> mkAge: (age: Int)chap4b.AccumEither[String,chap4b.Age]
    
  def mkPerson(name: String, age: Int): AccumEither[String, Person] =
  	map2Accum(mkName(name))(mkAge(age))(Person(_, _))
                                                  //> mkPerson: (name: String, age: Int)chap4b.AccumEither[String,chap4b.Person]
  
    
  mkPerson("name", 10)                            //> res8: chap4b.AccumEither[String,chap4b.Person] = Right(Person(Name(name),Ag
                                                  //| e(10)))
  mkPerson("", 100)                               //> res9: chap4b.AccumEither[String,chap4b.Person] = Left(List(Name is empty.))
                                                  //| 
 	mkPerson("the name", -10)                 //> res10: chap4b.AccumEither[String,chap4b.Person] = Left(List(Age is out of r
                                                  //| ange.))
 	mkPerson("", -1)                          //> res11: chap4b.AccumEither[String,chap4b.Person] = Left(List(Name is empty.,
                                                  //|  Age is out of range.))
 	
 	
}