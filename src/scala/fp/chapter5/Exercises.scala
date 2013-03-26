package scala.fp.chapter5

import org.scalatest.FunSuite

class Exercises extends FunSuite {

  test("Exercise 1 - Write a function to convert a Stream to a List") {
    println(Stream(1, 2, 3).toList)
    assert(List(1, 2, 3) == Stream(1, 2, 3).toList)
  }

  test("Exercise 2 - Write a function take for returning the first n elements of a Stream") {
    println(Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).take(3).toList)
    assert(List(1, 2, 3) == Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).take(3).toList)
  }

  test("Exercise 3 - Write the function takeWhile for returning all staring elements of a Stream that match a given predicate") {
    println(Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).takeWhile(_ <= 5).toList)
    assert(List(1, 2, 3, 4, 5) == Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).takeWhile(_ <= 5).toList)
  }



}
