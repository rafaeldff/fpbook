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

    
    def run[T](p: Par[T])(pool: ExecutorService): Future[T] = {println("run"); p(pool)}
  }

  def sum(as: IndexedSeq[Int]): Par[Int] =
    if (as.size <= 1) {println(s"BASE $as"); Par.unit(as.headOption getOrElse 0)}
    else {
      val (l, r) = as.splitAt(as.length / 2)
      println(s"l=$l r=$r THREAD ${Thread.currentThread.getId}")
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }                                             //> sum: (as: IndexedSeq[Int])java.util.concurrent.ExecutorService => java.util
                                                  //| .concurrent.Future[Int]
    
  val pool = Executors.newFixedThreadPool(4)      //> pool  : java.util.concurrent.ExecutorService = java.util.concurrent.ThreadP
                                                  //| oolExecutor@25b199c3[Running, pool size = 0, active threads = 0, queued tas
                                                  //| ks = 0, completed tasks = 0]
                                                  
  val parSum = sum(0 to 10)                       //> l=Range(0, 1, 2, 3, 4) r=Range(5, 6, 7, 8, 9, 10) THREAD 1
                                                  //| parSum  : java.util.concurrent.ExecutorService => java.util.concurrent.Futu
                                                  //| re[Int] = <function1>
  
  
  val f = Par.run(parSum)(pool)                   //> run
                                                  //| l=Range(0, 1) r=Range(2, 3, 4) THREAD 8
                                                  //| l=Range(0) r=Range(1) THREAD 9
                                                  //| BASE Range(0)
                                                  //| BASE Range(1)
                                                  //| l=Range(2) r=Range(3, 4) THREAD 10
                                                  //| BASE Range(2)
                                                  //| l=Range(3) r=Range(4) THREAD 11
                                                  //| BASE Range(3)
                                                  //| BASE Range(4)
                                                  //| l=Range(5, 6, 7) r=Range(8, 9, 10) THREAD 9
                                                  //| l=Range(5) r=Range(6, 7) THREAD 11
                                                  //| BASE Range(5)
                                                  //| l=Range(6) r=Range(7) THREAD 8
                                                  //| BASE Range(6)
                                                  //| BASE Range(7)
                                                  //| l=Range(8) r=Range(9, 10) THREAD 10
                                                  //| BASE Range(8)
                                                  //| l=Range(9) r=Range(10) THREAD 11
                                                  //| BASE Range(9)
                                                  //| BASE Range(10)
                                                  //| f  : java.util.concurrent.Future[Int] = chap7$Par$$anonfun$unit$1$$anon$1@1
                                                  //| 21157c9
                                                  
                                                  
                                                  
  println(s"CALLER: ${Thread.currentThread.getId}")
                                                  //> CALLER: 1
  println(f.get)                                  //> 55
  
  pool.shutdown()
}