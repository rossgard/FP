package scala.fp.chapter7

import org.scalatest.FunSuite
import java.util.concurrent._

class Exercises extends FunSuite {

  type Par[A] = ExecutorService => Future[A]

  object Par {

    /**
     * Injects a constant into a parallel computation.
     */
    def unit[A](a: A): Par[A] = {
      (es: ExecutorService) => UnitFuture(a)
    }

    /* Simple future for wrapping a constant value. */
    case class UnitFuture[A](get: A) extends Future[A] {
      def isDone = true

      def get(timeout: Long, units: TimeUnit) = get

      def isCancelled = false

      def cancel(evenIfRunning: Boolean): Boolean = false
    }

    /**
     * Combines the results of two parallel computations with a binary function.
     */
    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
      (es: ExecutorService) => {
        val af = a(es)
        val bf = b(es)
        UnitFuture(f(af.get, bf.get))
      }

    /**
     * Spawns a parallel computation. The computation will not be spawned until forced by run.
     */
    def fork[A](a: => Par[A]): Par[A] =
      es => es.submit(new Callable[A] {
        def call(): A = a(es).get
      })

    def async[A](a: => A): Par[A] = fork(unit(a))

    /**
     * Extracts a value from a Par by actually performing the computation.
     */
    def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  }


  /**
   * summing a sequence of values.
   */
  def sum(as: IndexedSeq[Int]): Int =
    if (as.size <= 1) as.headOption getOrElse 0
    else {
      val (l, r) = as.splitAt(as.length / 2)
      sum(l) + sum(r)
    }

  def sum2(as: IndexedSeq[Int]): Par[Int] =
    if (as.size <= 1) Par.unit(as.headOption getOrElse 0)
    else {
      val (l, r) = as.splitAt(as.length / 2)
      Par.map2(sum2(l), sum2(r))(_ + _)
    }

  def sum3(as: IndexedSeq[Int]): Par[Int] =
    if (as.isEmpty) Par.unit(0)
    else {
      val (l, r) = as.splitAt(as.length / 2)
      Par.map2(Par.fork(sum3(l)), Par.fork(sum3(r)))(_ + _)
    }

  test("Exercise 1. ...") {
    println(sum(IndexedSeq(1, 2, 3, 4, 5, 6)));
  }

  test("Exercise 2. Representations for par") {

  }

  test("Exercise 3. Implement functions of the API") {

  }
}
