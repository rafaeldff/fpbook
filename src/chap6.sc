object chap6 {
  trait RNG {
    def nextInt: (Int, RNG)
  }

  object RNG {
    def simple(seed: Long): RNG = new RNG {
      def nextInt = {
        val seed2 = (seed * 0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
        ((seed2 >>> 16).asInstanceOf[Int], simple(seed2))
      }
    }
  }
  
  def unit[S, A](a: A): State[S, A] = State((s: S) => (a, s))
                                                  //> unit: [S, A](a: A)chap6.State[S,A]

  case class State[S, +A](run: S => (A, S)) {
    def map[B](f: A => B): State[S, B] =
      flatMap { a => unit(f(a)) }

    def flatMap[B](f: A => State[S, B]): State[S, B] = State({ s =>
      val (a, sA) = run(s)
      f(a).run(sA)
    })
  }

  type Rand[+A] = State[RNG, A]



  val nextInt: Rand[Int] = State(rng => rng.nextInt)
                                                  //> nextInt  : chap6.Rand[Int] = State(<function1>)

  /** Ex 5 */
  def positiveMax(n: Int): Rand[Int] =
    nextInt.map { i => val nd = (i.toDouble / Int.MaxValue) * n; nd.toInt.abs }
                                                  //> positiveMax: (n: Int)chap6.Rand[Int]

  positiveMax(1000).run(RNG.simple(System.currentTimeMillis))
                                                  //> res0: (Int, chap6.RNG) = (919,chap6$RNG$$anon$1@5d4f38ff)

  /** Ex 1, 9 */
  def positiveInt: Rand[Int] =
    nextInt.flatMap { randomInt =>
      if (randomInt == Int.MinValue)
        positiveInt
      else
        unit(randomInt.abs)
    }                                             //> positiveInt: => chap6.Rand[Int]

  positiveInt.run(RNG.simple(System.currentTimeMillis))
                                                  //> res1: (Int, chap6.RNG) = (1980910154,chap6$RNG$$anon$1@2ad600e3)

  /** Ex 2, 6 */
  def nextDouble: Rand[Double] =
    nextInt.map { randomInt => randomInt.abs.toDouble / Int.MaxValue }
                                                  //> nextDouble: => chap6.Rand[Double]

  val doubles = Stream.iterate((0d, RNG.simple(0))) {
    case (i, rng) => nextDouble.run(rng)
  }                                               //> doubles  : scala.collection.immutable.Stream[(Double, chap6.RNG)] = Stream(
                                                  //| (0.0,chap6$RNG$$anon$1@42793673), ?)

  doubles.map { case (i, _) => i }.take(10).force //> res2: scala.collection.immutable.Stream[Double] = Stream(0.0, 0.0, 0.001970
                                                  //| 788930529165, 0.08326200306567456, 0.35328528487742195, 0.7292044967083281,
                                                  //|  0.18266122424167638, 0.18459529531402294, 0.9744344479285341, 0.9464994412
                                                  //| 597731)

  /** Ex 3, 7 */
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    for {
    	a <- ra
    	b <- rb
   } yield f(a,b)                                 //> map2: [A, B, C](ra: chap6.Rand[A], rb: chap6.Rand[B])(f: (A, B) => C)chap6.
                                                  //| Rand[C]
   
  def intDouble: Rand[(Int, Double)] =
    map2(nextInt, nextDouble) { (i, d) => (i, d) }//> intDouble: => chap6.Rand[(Int, Double)]

  def flip[A,B](in: (A, B)): (B, A) = in match { case (a, b) => (b, a) }
                                                  //> flip: [A, B](in: (A, B))(B, A)
  def doubleInt: Rand[(Double, Int)] =
    intDouble map flip                            //> doubleInt: => chap6.Rand[(Double, Int)]

  intDouble.run(RNG.simple(10000))                //> res3: ((Int, Double), chap6.RNG) = ((-447478295,0.6423802658181545),chap6$R
                                                  //| NG$$anon$1@798106fa)
  doubleInt.run(RNG.simple(20000))                //> res4: ((Double, Int), chap6.RNG) = ((0.7132686803644843,-894956590),chap6$R
                                                  //| NG$$anon$1@45fb302d)

  def double3: Rand[(Double, Double, Double)] =
    for {
    	d1 <- nextDouble
    	d2 <- nextDouble
    	d3 <- nextDouble
    } yield (d1, d2, d3)                          //> double3: => chap6.Rand[(Double, Double, Double)]
  
  double3.run(RNG.simple(9000))                   //> res5: ((Double, Double, Double), chap6.RNG) = ((0.3875359876023307,0.577945
                                                  //| 1604829846,0.2841290092487489),chap6$RNG$$anon$1@738ae796)

  /** Ex 8 */
  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
    def loop(rands: List[State[S, A]]): State[S, List[A]] = rands match {
      case Nil => unit(Nil)
      case ra :: ras =>
        ra.flatMap { a => loop(ras).map { rest => a :: rest } }
    }

    loop(fs)
  }                                               //> sequence: [S, A](fs: List[chap6.State[S,A]])chap6.State[S,List[A]]

  sequence(List(nextInt, nextInt, nextInt)).run(RNG.simple(99))
                                                  //> res6: (List[Int], chap6.RNG) = (List(38090141, 1575376209, 1102671736),chap
                                                  //| 6$RNG$$anon$1@4a871190)

  /** Ex 4, 8 */
  def ints(i: Int): Rand[List[Int]] = {
    val rands = List.fill(i)(nextInt)
    sequence(rands)
  }                                               //> ints: (i: Int)chap6.Rand[List[Int]]

  ints(10).run(RNG.simple(10))                    //> res7: (List[Int], chap6.RNG) = (List(3847489, 1334288366, 1486862010, 71166
                                                  //| 2464, -1453296530, -775316920, 1157481928, 294681619, -753148084, 697431532
                                                  //| ),chap6$RNG$$anon$1@2a698f02)
                                                  
	/** Ex 12 */
	def getState[S]: State[S, S] = State { s =>	 (s, s) }
                                                  //> getState: [S]=> chap6.State[S,S]
  def putState[S, A](newState:S): State[S, Unit] = State[S,Unit] {oldState =>
    ((), newState)
  }                                               //> putState: [S, A](newState: S)chap6.State[S,Unit]
}