package scala.fp.chapter4

sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case Right(a) => Right(a)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = for {a <- this; b1 <- b} yield f(a, b1)

  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es match {
      case Nil => Right(Nil)
      case h :: t => (f(h) map2 traverse(t)(f))(_ :: _)
    }

  def traverse_1[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es.foldRight[Either[E, List[B]]](Right(Nil))((a, b) => f(a).map2(b)(_ :: _))

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(x => x)

  /*
    There are a number of variations on `Option` and `Either`. If we want to accumulate multiple errors,
    a simple approach is a new data type that lets us keep a list of errors in the data constructor that represents failures:

    trait Partial[+A,+B]
    case class Errors[+A](get: Seq[A]) extends Partial[A,Nothing]
    case class Success[+B](get: B) extends Partial[Nothing,B]

    This type is called `Validation` in the Scalaz library. You can implement `map`, `map2`, `sequence`, and so on for this type in
    such a way that errors are accumulated when possible (`flatMap` is not able to accumulate errors--can you see why?).
    This idea can even be generalized further - we don't need to accumulate failing values into a list,
    we can accumulate values using any user-supplied binary function.

    It's also possible to use `Either[List[E],_]` directly to accumulate errors, using different implementations of helper functions like
    `map2` and `sequence`.
 */
}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Double, y: Double): Either[Exception, Double] =
    try {
      Right(x / y)
    } catch {
      case e: Exception => Left(e)
    }
}


