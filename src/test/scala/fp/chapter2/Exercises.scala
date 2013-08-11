package scala.fp.chapter2

import org.scalatest.FunSuite

class Exercises extends FunSuite {

  case class Box(height: Double, width: Double)

  def wider(x: Box, y: Box): Box = if (x.width > y.width) x else y

  def taller(x: Box, y: Box): Box = if (x.height > y.height) x else y

  test("1. Use the underscore notation to get a nicer-looking definition of taller and wider") {

    def greaterBy(x: Box, y: Box, f: Box => Double): Box = if (f(x) > f(y)) x else y

    def wider_(x: Box, y: Box): Box = greaterBy(x, y, p => p.width)
    def taller_(x: Box, y: Box) = greaterBy(x, y, p => p.height)

    def wider(x: Box, y: Box): Box = greaterBy(x, y, _.width)
    def taller(x: Box, y: Box): Box = greaterBy(x, y, _.height)
  }

  test("2. Define absolute as a higher-order function that calls abs on the return value of any given function") {
    def even(n: Int): Boolean = n % 2 == 0
    def negative(n: Int): Boolean = n < 0

    def odd(n: Int): Boolean = !(even(n))
    def positive(n: Int): Boolean = !(negative(n))

    def not(p: Int => Boolean): Int => Boolean = n => !(p(n))

    val odd_ = not(even)
    val positive_ = not(negative)

    def absolute(f: Int => Int): Int => Int =
      n => f(n).abs
  }

  test("3. Make your absolute function (from before) polymorphic like we did with not") {
    def absolute[A](f: A => Int): A => Int =
      a => f(a).abs
  }

  test("4. Write a new method divisibleBy that returns a predicate checking whether a given number is divisible by k") {
    type Pred[A] = A => Boolean

    def divisibleBy(k: Int): Pred[Int] = _ % k == 0
  }

  test("5. Revite even so that it calls divisableBy") {
    type Pred[A] = A => Boolean

    def divisibleBy(k: Int): Pred[Int] = _ % k == 0

    val even = divisibleBy(2)
  }

  test("6. Write a predicate that checks whether a number is divisable by both 3 and 5") {
    type Pred[A] = A => Boolean
    def divisibleBy(k: Int): Pred[Int] = _ % k == 0

    val divisibleBy3And5: Pred[Int] = (n: Int) => divisibleBy(3)(n) && divisibleBy(5)(n)
    divisibleBy3And5(15)

    val divisibleBy3Or5: Pred[Int] = (n: Int) => divisibleBy(3)(n) || divisibleBy(5)(n)
    divisibleBy3Or5(15)

    def lift[A](f: (Boolean, Boolean) => Boolean, g: Pred[A], h: Pred[A]): Pred[A] = (a: A) => f(g(a), h(a))

    val _divisibleBy3And5: Pred[Int] = lift[Int](_ && _, divisibleBy(3), divisibleBy(5))
    val _divisibleBy3Or5: Pred[Int] = lift[Int](_ || _, divisibleBy(3), divisibleBy(5))

    _divisibleBy3Or5(15)
  }

  test("7. Higher-order function that performs currying") {
    def curry[A, B, C](f: (A, B) => C): A => B => C = a => b => f(a, b)
  }

  test("8. Function that performs the inverse, uncurrying") {
    def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)
  }

  test("9. Higher-order function that composes two functions") {
    def compose[A, B, C](f: B => C, g: A => B): A => C = x => f(g(x))
  }

  test("10. Write lift3, which lifts a function that takes three arguments") {
    def lift3[A, B, C, D, E](f: (B, C, D) => E)(g: A => B, h: A => C, i: A => D): A => E = a => f(g(a), h(a), i(a))
  }

  test("11. Reuse lift function in your definition of lift3") {
    def lift[A, B, C, D](f: (B, C) => D)(g: A => B, h: A => C): A => D = a => f(g(a), h(a))

    def lift3[A, B, C, D, E](f: (B, C, D) => E)(g: A => B, h: A => C, i: A => D): A => E = a => lift[A, C, D, E](f(g(a), _, _))(h, i)(a)
  }

  // 0,1,1,2,3
  test("12. fibonacci") {
    def fib(n: Int): Int = {
      if (n < 2) n else fib(n - 1) + fib(n - 2)
    }
  }

  test("12. fibonacci tail recursive") {
    def fib(n: Int): Int = {
      def loop(n: Int, x: Int, accu: Int): Int = {
        if (n == 0) x else loop(n - 1, accu, x + accu)
      }
      loop(n, 0, 1)
    }
  }

  test("13. Write function iterateWhile") {
    type Pred[A] = A => Boolean

    def sqrt(n: Double): Double = {
      def f(x: Double) = (x * x) - n
      iterateWhile(2.0)(x => x - f(x) / (2 * x),
        x => f(x).abs > 1e-14)
    }

    def iterateWhile[A](a: A)(f: A => A, p: Pred[A]): A = if (p(a)) iterateWhile(f(a))(f, p) else a
  }






}
