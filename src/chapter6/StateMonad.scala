package chapter6

object StateMonad {
  def unit[S, A](a: A): State[S, A] = State((s: S) => (a, s))

  case class State[S, +A](run: S => (A, S)) {
    def map[B](f: A => B): State[S, B] =
      flatMap { a => unit(f(a)) }

    def flatMap[B](f: A => State[S, B]): State[S, B] = State({ s =>
      val (a, sA) = run(s)
      f(a).run(sA)
    })
  }

  def getState[S]: State[S, S] = State { s => (s, s) }
  def setState[S, A](newState: S): State[S, Unit] = State[S, Unit] { oldState =>
    ((), newState)
  }

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- getState
    _ <- setState(f(s))
  } yield ()

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
    def loop(rands: List[State[S, A]]): State[S, List[A]] = rands match {
      case Nil => unit(Nil)
      case ra :: ras =>
        ra.flatMap { a => loop(ras).map { rest => a :: rest } }
    }

    loop(fs)
  }
}