object chap6b {
  import chapter6.StateMonad._

  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input
  
  case class Machine(locked: Boolean, candies: Int, coins: Int)
  val initialMachine = new Machine(true, 10, 0)   //> initialMachine  : chap6b.Machine = Machine(true,10,0)
  
  def insertCoin: State[Machine, Int] =
    for {
      machine <- getState[Machine]
      val coins = machine.coins
      val newCoins = if (machine.locked) coins + 1 else coins
      val newMachine = if (machine.candies > 0) machine.copy(locked=false, coins=newCoins) else machine
      _ <- setState(newMachine)
    } yield {println("insertcoin"); println(machine);  println(newMachine); newCoins}
                                                  //> insertCoin: => chapter6.StateMonad.State[chap6b.Machine,Int]
    
  def turnKnob: State[Machine, Int] =
	  for {
	    machine <- getState[Machine]
	    val newMachine = if (!machine.locked) machine.copy(locked=true, candies=machine.candies-1) else machine
	    _ <- setState(newMachine)
	  } yield  {println("turnknob"); println(machine);  println(newMachine); newMachine.coins}
                                                  //> turnKnob: => chapter6.StateMonad.State[chap6b.Machine,Int]
    
  val singleRun = for {
    x <- insertCoin
    _ <- turnKnob
    y <- insertCoin
    _ <- turnKnob
    z <- insertCoin
  } yield z                                       //> singleRun  : chapter6.StateMonad.State[chap6b.Machine,Int] = State(<functio
                                                  //| n1>)
                                                  
  singleRun.run(initialMachine)                   //> insertcoin
                                                  //| Machine(true,10,0)
                                                  //| Machine(false,10,1)
                                                  //| turnknob
                                                  //| Machine(false,10,1)
                                                  //| Machine(true,9,1)
                                                  //| insertcoin
                                                  //| Machine(true,9,1)
                                                  //| Machine(false,9,2)
                                                  //| turnknob
                                                  //| Machine(false,9,2)
                                                  //| Machine(true,8,2)
                                                  //| insertcoin
                                                  //| Machine(true,8,2)
                                                  //| Machine(false,8,3)
                                                  //| res0: (Int, chap6b.Machine) = (3,Machine(false,8,3))

	def simulateMachine(inputs: List[Input]): State[Machine, Int] = {
		val stateActions = inputs.map {
			case Coin => insertCoin
			case Turn => turnKnob
		}
		sequence(stateActions).map{xs => xs.last}
	}                                         //> simulateMachine: (inputs: List[chap6b.Input])chapter6.StateMonad.State[chap
                                                  //| 6b.Machine,Int]
                                                  
  simulateMachine( Coin :: Turn :: Coin :: Turn :: Coin :: Nil ).run(initialMachine)
                                                  //> insertcoin
                                                  //| Machine(true,10,0)
                                                  //| Machine(false,10,1)
                                                  //| turnknob
                                                  //| Machine(false,10,1)
                                                  //| Machine(true,9,1)
                                                  //| insertcoin
                                                  //| Machine(true,9,1)
                                                  //| Machine(false,9,2)
                                                  //| turnknob
                                                  //| Machine(false,9,2)
                                                  //| Machine(true,8,2)
                                                  //| insertcoin
                                                  //| Machine(true,8,2)
                                                  //| Machine(false,8,3)
                                                  //| res1: (Int, chap6b.Machine) = (3,Machine(false,8,3))
}