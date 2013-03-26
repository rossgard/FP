package scala.fp.chapter2

class Chapter2 {

  // Partial application
  val timesTwo: Int => Int = _ * 2
  timesTwo(2)

  val f = (x : Double) => math.Pi / 2 - x

  val cos = f andThen math.sin

  def join[A](a : A)(f : (A,A) => A) : A = f(a,a)

  val joinWithOne = join(1) _

  def gcd(x : Int, y : Int) : Int = {
    val a = x.abs
    val b = y.abs

    if (b == 0) a else gcd(b, a % b)
  }

  // Not tail recursive
  def factorial(n : Int) : Int = {
    if (n < 2) 1 else n * factorial(n - 1)
  }

  def _factorial(n:Int) : Int = {
    def loop(n: Int, accu : Int) : Int = {
      if (n < 2) accu
      else loop(n - 1, accu * n)
    }
    loop(n, 1)
  }






}
