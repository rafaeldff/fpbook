import java.util.concurrent.{ThreadPoolExecutor, Executors, ExecutorService, Callable, Future, FutureTask, TimeUnit}

object chap7 {
  type  Par[T] = ExecutorService => Future[T]

  object Par {
    def unit[A](a: A): Par[A] = {_ =>
    	new Future[A] {
    	  def get = a
    	  def get(timeout:Long,unit:TimeUnit) = a
    	  def isCancelled = false
    	  def isDone = true
    	  def cancel(ignored: Boolean) = false
    	}
    }
    
    def map2[A, B, R](a: Par[A], b: Par[B])(f: (A, B) => R): Par[R] =  {executorService =>
    	unit(f(a(executorService).get, b(executorService).get))(executorService)
    }
    
    def fork[A](a: => Par[A]): Par[A] = {executorService =>
      val f: Future[A] = executorService.submit(new Callable[A] {def call:A = a(executorService).get })
			f
    }
    
    def async[A](a: => A): Par[A] = fork(unit(a))
    
    def asyncF[A,B](f: A => B): A => Par[B] = {(a:A) => async(f(a))}
    
    def run[T](p: Par[T])(pool: ExecutorService): Future[T] = p(pool)
  }

  def sum(as: IndexedSeq[Int]): Par[Int] =
    if (as.size <= 1) {Par.unit(as.headOption getOrElse 0)}
    else {
      val (l, r) = as.splitAt(as.length / 2)
      //println(s"l=$l r=$r THREAD ${Thread.currentThread.getId}")
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }                                             //> sum: (as: IndexedSeq[Int])java.util.concurrent.ExecutorService => java.util
                                                  //| .concurrent.Future[Int]
    
  val pool = Executors.newFixedThreadPool(4)      //> pool  : java.util.concurrent.ExecutorService = java.util.concurrent.ThreadP
                                                  //| oolExecutor@26830cbb[Running, pool size = 0, active threads = 0, queued tas
                                                  //| ks = 0, completed tasks = 0]
  val parSum = sum(0 to 10)                       //> parSum  : java.util.concurrent.ExecutorService => java.util.concurrent.Futu
                                                  //| re[Int] = <function1>
  
  val future = Par.run(parSum)(pool)              //> future  : java.util.concurrent.Future[Int] = chap7$Par$$anonfun$unit$1$$ano
                                                  //| n$1@76d697d9
                                                  
                                                  
                                                  
  println(s"CALLER: ${Thread.currentThread.getId}")
                                                  //> CALLER: 1
  
  val f: Int => Int = {i => /*Thread.sleep(2*1000);*/ i * i }
                                                  //> f  : Int => Int = <function1>
                                                  
  println("bef")                                  //> bef
  val af = Par.asyncF(f)(10)                      //> af  : java.util.concurrent.ExecutorService => java.util.concurrent.Future[I
                                                  //| nt] = <function1>
  println("aft")                                  //> aft
  
  Par.run(af)(pool).get                           //> res0: Int = 100
  
  pool.shutdown()
}