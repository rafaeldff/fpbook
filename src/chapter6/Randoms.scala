package chapter6

trait Randoms {
  import StateMonad._
  
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

  type Rand[+A] = State[RNG, A]

  val nextInt: Rand[Int] = State(rng => rng.nextInt)
  
  def nextDouble: Rand[Double] =
    nextInt.map { randomInt => randomInt.abs.toDouble / Int.MaxValue }
}

object Randoms extends Randoms
