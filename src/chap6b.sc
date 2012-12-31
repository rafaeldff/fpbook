object chap6b {
  import chapter6.StateMonad._

  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input
  
  case class Machine(locked: Boolean, candies: Int, coins: Int)
  val initialMachine = new Machine(true, 10, 0)   //> initialMachine  : chap6b.Machine = Machine(true,10,0)
  
  object CandyLeft {
 		def unapply(candies: Int) = candies > 0
  }
  
  def rules(input: Input)(machine:Machine): Machine = (input, machine) match {
  	case (Coin, m@Machine(true, CandyLeft(), _)) =>
  		m.copy(locked=false, coins=m.coins+1)
  	case (Turn, m@Machine(false, CandyLeft(), _)) =>
  		m.copy(locked=true, candies=m.candies-1)
  	case (_, m) => m
  }                                               //> rules: (input: chap6b.Input)(machine: chap6b.Machine)chap6b.Machine

	def simulateMachine(inputs: List[Input]): State[Machine, Int] = {
	  for {
	  	_ <-	sequence(inputs.map {input =>
	  	 	modify[Machine] (rules(input))
	  	})
			m <- getState
	  } yield m.coins
	}                                         //> simulateMachine: (inputs: List[chap6b.Input])chapter6.StateMonad.State[chap6
                                                  //| b.Machine,Int]
                                                  
  simulateMachine( Coin :: Turn :: Coin :: Turn :: Coin :: Nil ).run(initialMachine)
                                                  //> res0: (Int, chap6b.Machine) = (3,Machine(false,8,3))
}