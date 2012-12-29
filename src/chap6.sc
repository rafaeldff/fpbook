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
  
  type State[S, +A] = S => (A, S)
  
  type Rand[+A] = State[RNG, A]
  
  val nextInt: Rand[Int] = rng => rng.nextInt     //> nextInt  : chap6.RNG => (Int, chap6.RNG) = <function1>
  
  def unit[A](a: A): Rand[A] = rng => (a, rng)    //> unit: [A](a: A)chap6.RNG => (A, chap6.RNG)
  
  def map[A,B](rand: Rand[A])(f: A => B): Rand[B] =
    flatMap(rand) {a => unit(f(a))}               //> map: [A, B](rand: chap6.RNG => (A, chap6.RNG))(f: A => B)chap6.RNG => (B, ch
                                                  //| ap6.RNG)
  
  def flatMap[A,B](r: Rand[A])(f: A => Rand[B]): Rand[B] = {rng =>
    val (a, rngA) = r(rng)
    f(a)(rngA)
  }                                               //> flatMap: [A, B](r: chap6.RNG => (A, chap6.RNG))(f: A => (chap6.RNG => (B, ch
                                                  //| ap6.RNG)))chap6.RNG => (B, chap6.RNG)
   
  /** Ex 5 */
  def positiveMax(n: Int): Rand[Int] =
    map(nextInt){i => val nd = (i.toDouble / Int.MaxValue) * n; nd.toInt.abs}
                                                  //> positiveMax: (n: Int)chap6.RNG => (Int, chap6.RNG)

  positiveMax(1000)(RNG.simple(System.currentTimeMillis))
                                                  //> res0: (Int, chap6.RNG) = (926,chap6$RNG$$anon$1@41cb0ed6)

  /** Ex 1, 9 */
	def positiveInt: Rand[Int] =
	  flatMap(nextInt) {randomInt =>
	    if (randomInt == Int.MinValue)
	    	positiveInt
	    else
	    	unit(randomInt.abs)
	  }                                       //> positiveInt: => chap6.RNG => (Int, chap6.RNG)
                                                  
  positiveInt(RNG.simple(System.currentTimeMillis))
                                                  //> res1: (Int, chap6.RNG) = (1982717249,chap6$RNG$$anon$1@62bbf09c)
	
	/** Ex 2, 6 */
	def randomDouble(rng:RNG): (Double, RNG) = {
	  val (randomInt, nextRng) = rng.nextInt
	  val randomDouble = (randomInt.abs.toDouble / Int.MaxValue)
	  (randomDouble, nextRng)
	}                                         //> randomDouble: (rng: chap6.RNG)(Double, chap6.RNG)
	
	def nextDouble: Rand[Double] =
		map(nextInt) { randomInt => randomInt.abs.toDouble / Int.MaxValue }
                                                  //> nextDouble: => chap6.RNG => (Double, chap6.RNG)
                                                  
  
  val doubles = Stream.iterate((0d, RNG.simple(0))) {
  	case (i, rng) => nextDouble(rng)
  }                                               //> doubles  : scala.collection.immutable.Stream[(Double, chap6.RNG)] = Stream(
                                                  //| (0.0,chap6$RNG$$anon$1@531cfec), ?)
                                                  
  doubles.map{case (i, _) => i}.take(10).force    //> res2: scala.collection.immutable.Stream[Double] = Stream(0.0, 0.0, 0.001970
                                                  //| 788930529165, 0.08326200306567456, 0.35328528487742195, 0.7292044967083281,
                                                  //|  0.18266122424167638, 0.18459529531402294, 0.9744344479285341, 0.9464994412
                                                  //| 597731)
	
	/** Ex 3, 7 */
	def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] =
		flatMap(ra) {a => map(rb) {b =>
	    f(a,b)
	  }}                                      //> map2: [A, B, C](ra: chap6.RNG => (A, chap6.RNG), rb: chap6.RNG => (B, chap6
                                                  //| .RNG))(f: (A, B) => C)chap6.RNG => (C, chap6.RNG)
	  
	def intDouble: Rand[(Int,Double)] =
	  map2(nextInt, nextDouble){(i,d) => (i,d)}
                                                  //> intDouble: => chap6.RNG => ((Int, Double), chap6.RNG)
                                                  
  def doubleInt: Rand[(Double, Int)] =
    map(intDouble){case (i,d) => (d,i)}           //> doubleInt: => chap6.RNG => ((Double, Int), chap6.RNG)
    
 
 	intDouble(RNG.simple(10000))              //> res3: ((Int, Double), chap6.RNG) = ((-447478295,0.6423802658181545),chap6$R
                                                  //| NG$$anon$1@45800894)
  doubleInt(RNG.simple(20000))                    //> res4: ((Double, Int), chap6.RNG) = ((0.7132686803644843,-894956590),chap6$R
                                                  //| NG$$anon$1@8e81918)
  
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
  	val (randomD1, nextRng1) = randomDouble(rng)
  	val (randomD2, nextRng2) = randomDouble(nextRng1)
  	val (randomD3, nextRng3) = randomDouble(nextRng2)
  	((randomD1, randomD2, randomD3), nextRng3)
  }                                               //> double3: (rng: chap6.RNG)((Double, Double, Double), chap6.RNG)
  
  /** Ex 8 */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {rng =>
    def loop(rands: List[Rand[A]], curRng:RNG): (List[A], RNG) = rands match {
    	case Nil => (Nil, curRng)
      case ra :: ras =>
        val (a, nextRng) = ra(curRng)
        val (rest, finalRng) = loop(ras, nextRng)
        (a :: rest, finalRng)
    }
    
    loop(fs, rng)
  }                                               //> sequence: [A](fs: List[chap6.RNG => (A, chap6.RNG)])chap6.RNG => (List[A], 
                                                  //| chap6.RNG)
  
  sequence(List(nextInt, nextInt, nextInt))(RNG.simple(99))
                                                  //> res5: (List[Int], chap6.RNG) = (List(38090141, 1575376209, 1102671736),chap
                                                  //| 6$RNG$$anon$1@12b3519c)
  
	/** Ex 4, 8 */
	def ints(i:Int): Rand[List[Int]] = {
		val rands = List.fill(i)(nextInt)
		sequence(rands)
	}                                         //> ints: (i: Int)chap6.RNG => (List[Int], chap6.RNG)
	
  ints(10)(RNG.simple(10))                        //> res6: (List[Int], chap6.RNG) = (List(3847489, 1334288366, 1486862010, 71166
                                                  //| 2464, -1453296530, -775316920, 1157481928, 294681619, -753148084, 697431532
                                                  //| ),chap6$RNG$$anon$1@d1e67eb)
}