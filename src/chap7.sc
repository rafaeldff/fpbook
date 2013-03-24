import java.util.concurrent.{ThreadPoolExecutor, Executors, ExecutorService, Callable, Future, FutureTask, TimeUnit}

object chap7 {
  type  Par[T] = ExecutorService => Future[T]

  object Par {
    private def resolved[A](a: =>A): Future[A] =
    	new Future[A] {
    	  def get = a
    	  def get(timeout:Long,unit:TimeUnit) = a
    	  def isCancelled = false
    	  def isDone = true
    	  def cancel(ignored: Boolean) = false
    	}
     
    def unit[A](a: A): Par[A] = {_ =>
    	resolved(a)
    }
    
    def product[A,B](fa: Par[A], fb: Par[B]): Par[(A,B)] = {executorService => unit( (fa(executorService).get, fb(executorService).get) )(executorService)  }
		def map[A,B](pa: Par[A])(f: A => B): Par[B] = {executorService => unit(f(pa(executorService).get))(executorService)}
		
		def flatMap[A,R](pa: Par[A])(f: A => Par[R]): Par[R] = {executorService =>
			f(pa(executorService).get)(executorService)
		}
    
    def map2[A, B, R](a: Par[A], b: Par[B])(f: (A, B) => R): Par[R] =  map(product(a,b)){case (va,vb) => f(va,vb)}
    
    def map3[A,R](pa: Par[A], pb: Par[A], pc: Par[A])(f: (A, A, A) => R): Par[R] = {
    	val papb: Par[(A,A)] = map2(pa, pb) {(a,b) => (a,b)}
    	map2(papb, pc) {(ab, c) => val (a,b) = ab; f(a, b, c) }
    }
    
    def map4[A,R](pa: Par[A], pb: Par[A], pc: Par[A], pd: Par[A])(f: (A, A, A, A) => R): Par[R] = {
    	val papb: Par[(A,A)] = map2(pa, pb) {(a,b) => (a,b)}
    	val papbpc: Par[(A,A,A)] = map2(papb, pc) {(ab, c) => val (a,b) = ab; (a,b,c)}
    	map2(papbpc, pd) {(abc, d) => val (a,b,c) = abc; f(a, b, c, d) }
    }
    
    def map5[A,R](pa: Par[A], pb: Par[A], pc: Par[A], pd: Par[A], pe: Par[A])(f: (A, A, A, A, A) => R): Par[R] = {
    	val papb: Par[(A,A)] = map2(pa, pb) {(a,b) => (a,b)}
    	val papbpc: Par[(A,A,A)] = map2(papb, pc) {(ab, c) => val (a,b) = ab; (a,b,c)}
    	val papbpcpd: Par[(A,A,A,A)] = map2(papbpc, pd) {(abc, d) => val (a,b, c) = abc; (a,b,c,d)}
    	map2(papbpcpd, pe) {(abcd, e) => val (a,b,c,d) = abcd; f(a, b, c, d, e) }
    }
    
    def fork[A](a: => Par[A]): Par[A] = {executorService =>
      val f: Future[A] = executorService.submit(new Callable[A] {def call:A = a(executorService).get })
			f
    }
    
    
    def choice[A](pa: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] = {
      flatMap(pa) {a => if (a) ifTrue else ifFalse }
    }
    
    def async[A](a: => A): Par[A] = fork(unit(a))
    
    def asyncF[A,B](f: A => B): A => Par[B] = {(a:A) => async(f(a))}
    
    def run[T](p: Par[T])(pool: ExecutorService): Future[T] = p(pool)
    
    def sequence[A](l: List[Par[A]]):Par[List[A]] =
    	l.foldRight(unit(Nil:List[A])) {(pb,plb) =>
    	  map2(pb, plb){(b,lb) => b :: lb}
    	}
    	
    def parMap[A,B](l:List[A])(f: A=>B):Par[List[B]] = fork {
    	val parList: List[Par[B]] = l.map(asyncF(f))
    	sequence(parList)
    }
    
    def parFilter[A](l:List[A])(f: A => Boolean):Par[List[A]] = fork {
      val listParBools: List[Par[Boolean]] = l map asyncF(f)
      var parListBools = sequence(listParBools)

			map(parListBools){listBools =>
			  val pairs = l zip listBools
			  pairs.flatMap {case (a, true) => List(a); case _ => List()}
			}
    }
    
    def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
			p(e).get == p2(e).get
      
  }

  def sum0(as: IndexedSeq[Int]): Par[Int] =
    if (as.size <= 1) {Par.unit(as.headOption getOrElse 0)}
    else {
      val (l, r) = as.splitAt(as.length / 2)
      //println(s"l=$l r=$r THREAD ${Thread.currentThread.getId}")
      Par.map2(Par.fork(sum0(l)), Par.fork(sum0(r)))(_ + _)
    }                                             //> sum0: (as: IndexedSeq[Int])java.util.concurrent.ExecutorService => java.uti
                                                  //| l.concurrent.Future[Int]
                                                  
  def fold[A,B](as: IndexedSeq[A])(g: A => B)(f: (B,B) => B): Par[B] =
    if (as.size <= 1) {Par.unit(g(as.head))}
    else {
      val (l, r) = as.splitAt(as.length / 2)
      //println(s"l=$l r=$r THREAD ${Thread.currentThread.getId}")
      val res: Par[B] = Par.map2(Par.fork(fold(l)(g)(f)), Par.fork(fold(r)(g)(f)))(f) ;
      res
    }                                             //> fold: [A, B](as: IndexedSeq[A])(g: A => B)(f: (B, B) => B)java.util.concurr
                                                  //| ent.ExecutorService => java.util.concurrent.Future[B]
    
  def sum = {(l:IndexedSeq[Int]) => fold(l)(identity[Int])(_+_) }
                                                  //> sum: => IndexedSeq[Int] => (java.util.concurrent.ExecutorService => java.ut
                                                  //| il.concurrent.Future[Int])
  
  val a = Par.async(42+1)                         //> a  : java.util.concurrent.ExecutorService => java.util.concurrent.Future[In
                                                  //| t] = <function1>
  val S = Executors.newFixedThreadPool(1)         //> S  : java.util.concurrent.ExecutorService = java.util.concurrent.ThreadPool
                                                  //| Executor@233d0d04[Running, pool size = 0, active threads = 0, queued tasks 
                                                  //| = 0, completed tasks = 0]
                                                  
  //Par.run(Par.fork  (a))(S).get
    
  val pool = Executors.newFixedThreadPool(4)      //> pool  : java.util.concurrent.ExecutorService = java.util.concurrent.ThreadP
                                                  //| oolExecutor@7d2193ae[Running, pool size = 0, active threads = 0, queued tas
                                                  //| ks = 0, completed tasks = 0]
  val parSum = sum(0 to 10)                       //> parSum  : java.util.concurrent.ExecutorService => java.util.concurrent.Futu
                                                  //| re[Int] = <function1>
  
  val future = Par.run(parSum)(pool)              //> future  : java.util.concurrent.Future[Int] = chap7$Par$$anon$1@2470b02c
                                                  
                                                 
                                                  
  println(s"CALLER: ${Thread.currentThread.getId}")
                                                  //> CALLER: 1
  println(future.get)                             //> 55
  
  val f: Int => Int = {i => /*Thread.sleep(2*1000);*/ i * i }
                                                  //> f  : Int => Int = <function1>
                                                  
  println("bef")                                  //> bef
  val af = Par.asyncF(f)(10)                      //> af  : java.util.concurrent.ExecutorService => java.util.concurrent.Future[I
                                                  //| nt] = <function1>
  println("aft")                                  //> aft
  
  Par.run(af)(pool).get                           //> res0: Int = 100
  
  println("bef")                                  //> bef
  val r = Par.parMap(List(1,2,3,4,5))(f)          //> r  : java.util.concurrent.ExecutorService => java.util.concurrent.Future[Li
                                                  //| st[Int]] = <function1>
  println("aft")                                  //> aft
  Par.run(r)(pool).get                            //> res1: List[Int] = List(1, 4, 9, 16, 25)
  println("end")                                  //> end
  
  pool.shutdown()
}