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
  
	def randomPositive(rng:RNG): (Int, RNG) = {
		val (randomInt, nextRng) = rng.nextInt
		val randomPos = if (randomInt == Int.MinValue) Int.MaxValue else randomInt.abs
		(randomPos, nextRng)
	}                                         //> randomPositive: (rng: chap6.RNG)(Int, chap6.RNG)
	
	def randomDouble(rng:RNG): (Double, RNG) = {
	  val (randomInt, nextRng) = rng.nextInt
	  val randomDouble = (randomInt.abs.toDouble / Int.MaxValue)
	  (randomDouble, nextRng)
	}                                         //> randomDouble: (rng: chap6.RNG)(Double, chap6.RNG)
	
	def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (randomI, nextRng) = rng.nextInt
    val (randomD, nextNextRng) = randomDouble(nextRng)
    ((randomI, randomD), nextNextRng)
	}                                         //> intDouble: (rng: chap6.RNG)((Int, Double), chap6.RNG)
                                                  
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((randomI, randomD), nextRng) = intDouble(rng)
    ((randomD, randomI), nextRng)
  }                                               //> doubleInt: (rng: chap6.RNG)((Double, Int), chap6.RNG)
                                                  
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
  	val (randomD1, nextRng1) = randomDouble(rng)
  	val (randomD2, nextRng2) = randomDouble(nextRng1)
  	val (randomD3, nextRng3) = randomDouble(nextRng2)
  	((randomD1, randomD2, randomD3), nextRng3)
  }                                               //> double3: (rng: chap6.RNG)((Double, Double, Double), chap6.RNG)
  
                                                  
	
	def ints(i:Int)(rng:RNG): (List[Int], RNG) = {
			if (i <= 0)
				(Nil, rng.nextInt._2)
			else {
			  val (nextX, nextRng) = randomPositive(rng)
			  val (list, nextRng2) = ints(i-1)(nextRng)
			  (nextX :: list, nextRng2)
			}
	}                                         //> ints: (i: Int)(rng: chap6.RNG)(List[Int], chap6.RNG)
	
  ints(10)(RNG.simple(10))                        //> res0: (List[Int], chap6.RNG) = (List(3847489, 1334288366, 1486862010, 71166
                                                  //| 2464, 1453296530, 775316920, 1157481928, 294681619, 753148084, 697431532),c
                                                  //| hap6$RNG$$anon$1@c5ec9da)
  
}