
import java.util.concurrent.ThreadPoolExecutor
import java.util.concurrent.Executors
import java.util.concurrent.ExecutorService
import java.util.concurrent.Callable

object chap7 {
  sealed trait Par[T]

  case class Forked[T](p: () => Par[T]) extends Par[T]
  case class Const[T](t: T) extends Par[T]

  object Par {
    def unit[A](a: A): Par[A] = {print(s"c($a) "); Const(a)}
    
    def map2[A, B, R](a: Par[A], b: Par[B])(f: (A, B) => R): Par[R] = (a, b) match {
      case (Const(a), Const(b)) => {println(s"$a + $b"); Const(f(a, b))}
      case (Forked(a), Forked(b)) => {print("f"); Forked { () => map2(a(), b())(f) }}
      case (Forked(a), b: Const[B]) => Forked { () => map2(a(), b)(f) }
      case (a: Const[A], Forked(b)) => Forked { () => map2(a, b())(f) }
    }
    
    def fork[A](a: => Par[A]): Par[A] = { print("."); Forked(() => a) }
    //def async[A](a: => A): Par[A] = fork(unit(a))
    def run[T](p: Par[T])(pool: ExecutorService): T = {
      p match {
        case Const(t) => t
        case Forked(inner) => pool.submit(new Callable[T]() {
          def call() = run({print("v"); inner()})(pool)
        }).get()
      }
    }
  }

  def sum(as: IndexedSeq[Int]): Par[Int] =
    if (as.size <= 1) Par.unit(as.headOption getOrElse 0)
    else {
      val (l, r) = as.splitAt(as.length / 2)
      println(s"l=$l r=$r")
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }                                             //> sum: (as: IndexedSeq[Int])chap7.Par[Int]
    
  val pool = Executors.newFixedThreadPool(4)      //> pool  : java.util.concurrent.ExecutorService = java.util.concurrent.ThreadP
                                                  //| oolExecutor@41942912[Running, pool size = 0, active threads = 0, queued tas
                                                  //| ks = 0, completed tasks = 0]
                                                  
  val parSum = sum(0 to 10)                       //> l=Range(0, 1, 2, 3, 4) r=Range(5, 6, 7, 8, 9, 10)
                                                  //| ..fparSum  : chap7.Par[Int] = Forked(<function0>)
  
  println("--------------")                       //> --------------
  
  Par.run(parSum)(pool)                           //> vl=Range(0, 1) r=Range(2, 3, 4)
                                                  //| ..fl=Range(5, 6, 7) r=Range(8, 9, 10)
                                                  //| ..ffvl=Range(0) r=Range(1)
                                                  //| ..fl=Range(2) r=Range(3, 4)
                                                  //| ..ffl=Range(5) r=Range(6, 7)
                                                  //| ..fl=Range(8) r=Range(9, 10)
                                                  //| ..fffvc(0) c(1) 0 + 1
                                                  //| c(2) l=Range(3) r=Range(4)
                                                  //| ..fc(5) l=Range(6) r=Range(7)
                                                  //| ..fc(8) l=Range(9) r=Range(10)
                                                  //| ..fffvc(3) c(4) 3 + 4
                                                  //| 2 + 7
                                                  //| 1 + 9
                                                  //| c(6) c(7) 6 + 7
                                                  //| 5 + 13
                                                  //| c(9) c(10) 9 + 10
                                                  //| 8 + 19
                                                  //| 18 + 27
                                                  //| 10 + 45
                                                  //| res0: Int = 55
  
  pool.shutdown()
}