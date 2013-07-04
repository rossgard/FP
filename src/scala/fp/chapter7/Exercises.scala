package scala.fp.chapter7

import org.scalatest.FunSuite

class Exercises extends FunSuite {

  def sum(as: IndexedSeq[Int]): Int =
    if (as.size <= 1) as.headOption getOrElse 0
    else {
      val (l, r) = as.splitAt(as.length / 2)
      sum(l) + sum(r)
    }

  test("Exercise 1. ...") {
    sum(IndexedSeq())
  }


}
