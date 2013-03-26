package scala.fp.chapter5

trait Stream[+A] {

  def uncons: Option[(A, Stream[A])]

  def isEmpty: Boolean = uncons.isEmpty

  def toList: List[A] =
    uncons match {
      case Some((h, t)) => h :: t.toList
      case _ => List()
    }

  def take(n: Int): Stream[A] =
    uncons match {
      case Some((h, t)) if (n > 0) => Stream.cons(h, t.take(n - 1))
      case _ => Stream.empty
    }

  def takeWhile(p: A => Boolean): Stream[A] =
    uncons match {
      case Some((h, t)) if p(h) => Stream.cons(h, t takeWhile p)
      case _ => Stream.empty
    }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    uncons match {
      case Some((h, t)) => f(h, t.foldRight(z)(f))
      case None => z
    }

  def exists (p: A => Boolean): Boolean =
  foldRight(false)((a, b) => p(a) || b)


}

object Stream {
  def empty[A]: Stream[A] =
    new Stream[A] {
      def uncons = None
    }

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
    new Stream[A] {
      lazy val uncons = Some((hd, tl))
    }

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
}