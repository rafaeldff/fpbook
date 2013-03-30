
object chap6  {
  import chapter6.StateMonad._
  import chapter6.Randoms._

  /** Ex 5 */
  def positiveMax(n: Int): Rand[Int] =
    nextInt.map { i => val nd = (i.toDouble / Int.MaxValue) * n; nd.toInt.abs }
                                                  //> positiveMax: (n: Int)chapter6.Randoms.Rand[Int]

  positiveMax(1000).run(RNG.simple(System.currentTimeMillis))
                                                  //> res0: (Int, chapter6.Randoms.RNG) = (582,chapter6.Randoms$RNG$$anon$1@1102b5
                                                  //| a9)

  /** Ex 1, 9 */
  def positiveInt: Rand[Int] =
    nextInt.flatMap { randomInt =>
      if (randomInt == Int.MinValue)
        positiveInt
      else
        unit(randomInt.abs)
    }                                             //> positiveInt: => chapter6.Randoms.Rand[Int]

  positiveInt.run(RNG.simple(System.currentTimeMillis))
                                                  //> res1: (Int, chapter6.Randoms.RNG) = (1240894542,chapter6.Randoms$RNG$$anon$1
                                                  //| @45a51ac0)

  /** Ex 2, 6 */
  def nextDouble: Rand[Double] =
    nextInt.map { randomInt => randomInt.abs.toDouble / Int.MaxValue }
                                                  //> nextDouble: => chapter6.Randoms.Rand[Double]

  val doubles = Stream.iterate((0d, RNG.simple(0))) {
    case (i, rng) => nextDouble.run(rng)
  }                                               //> doubles  : scala.collection.immutable.Stream[(Double, chapter6.Randoms.RNG)]
                                                  //|  = Stream((0.0,chapter6.Randoms$RNG$$anon$1@6110e3b5), ?)

  println( doubles.map { case (i, _) => i }.take(10).force )
                                                  //> Stream(0.0, 0.0, 0.001970788930529165, 0.08326200306567456, 0.35328528487742
                                                  //| 195, 0.7292044967083281, 0.18266122424167638, 0.18459529531402294, 0.9744344
                                                  //| 479285341, 0.9464994412597731)

  /** Ex 3, 7 */
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    for {
    	a <- ra
    	b <- rb
   } yield f(a,b)                                 //> map2: [A, B, C](ra: chapter6.Randoms.Rand[A], rb: chapter6.Randoms.Rand[B])(
                                                  //| f: (A, B) => C)chapter6.Randoms.Rand[C]
   
  def intDouble: Rand[(Int, Double)] =
    map2(nextInt, nextDouble) { (i, d) => (i, d) }//> intDouble: => chapter6.Randoms.Rand[(Int, Double)]

  def flip[A,B](in: (A, B)): (B, A) = in match { case (a, b) => (b, a) }
                                                  //> flip: [A, B](in: (A, B))(B, A)
  def doubleInt: Rand[(Double, Int)] =
    intDouble map flip                            //> doubleInt: => chapter6.Randoms.Rand[(Double, Int)]

  intDouble.run(RNG.simple(10000))                //> res2: ((Int, Double), chapter6.Randoms.RNG) = ((-447478295,0.64238026581815
                                                  //| 45),chapter6.Randoms$RNG$$anon$1@91619a4)
  doubleInt.run(RNG.simple(20000))                //> res3: ((Double, Int), chapter6.Randoms.RNG) = ((0.7132686803644843,-8949565
                                                  //| 90),chapter6.Randoms$RNG$$anon$1@53f2c2a3)

  def double3: Rand[(Double, Double, Double)] =
    for {
    	d1 <- nextDouble
    	d2 <- nextDouble
    	d3 <- nextDouble
    } yield (d1, d2, d3)                          //> double3: => chapter6.Randoms.Rand[(Double, Double, Double)]
  
  double3.run(RNG.simple(9000))                   //> res4: ((Double, Double, Double), chapter6.Randoms.RNG) = ((0.38753598760233
                                                  //| 07,0.5779451604829846,0.2841290092487489),chapter6.Randoms$RNG$$anon$1@2361
                                                  //| d10f)

  /** Ex 8 */

  sequence(List(nextInt, nextInt, nextInt)).run(RNG.simple(99))
                                                  //> res5: (List[Int], chapter6.Randoms.RNG) = (List(38090141, 1575376209, 11026
                                                  //| 71736),chapter6.Randoms$RNG$$anon$1@329b0985)

  /** Ex 4, 8 */
  def ints(i: Int): Rand[List[Int]] = {
    val rands = List.fill(i)(nextInt)
    sequence(rands)
  }                                               //> ints: (i: Int)chapter6.Randoms.Rand[List[Int]]

  ints(10).run(RNG.simple(10))                    //> res6: (List[Int], chapter6.Randoms.RNG) = (List(3847489, 1334288366, 148686
                                                  //| 2010, 711662464, -1453296530, -775316920, 1157481928, 294681619, -753148084
                                                  //| , 697431532),chapter6.Randoms$RNG$$anon$1@642052de)
                                                  
	/** Ex 12 (see chapter6.StateMonad) */

}