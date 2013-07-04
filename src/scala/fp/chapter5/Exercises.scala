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

  test("Exercise 4 - Implement forAll, which checks that all elements in the Stream match a given predicate.") {
    // You should terminate the traversal as soon as it encounters a non-matching value
    assert(true == Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).forAllByFoldRight(_ <= 10))
    assert(false == Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15).forAllByFoldRight(_ <= 10))
  }

  test("Exercise 5 - Use foldRight to implement takeWhile") {
    println(Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).takeWhileByFoldRight(_ <= 5).toList)
    assert(List(1, 2, 3, 4, 5) == Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).takeWhileByFoldRight(_ <= 5).toList)
  }

  test("Exercise 6 - Implement map, filter, append, and flatMap by using foldRight") {
    println(Stream(1, 2, 3, 4, 5).mapByFoldRight(_ * 2))
    assert(List(2, 4, 6, 8, 10) == Stream(1, 2, 3, 4, 5).mapByFoldRight(_ * 2).toList)

    println(Stream(1, 2, 3, 4, 5).filterByFoldRight(_ <= 3))
    assert(List(1, 2, 3) == Stream(1, 2, 3, 4, 5).filterByFoldRight(_ <= 3).toList)

    println(Stream(1, 2, 3, 4, 5).appendByFoldRight(Stream(6, 7, 8, 9, 10)))
    assert(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) == Stream(1, 2, 3, 4, 5).appendByFoldRight(Stream(6, 7, 8, 9, 10)).toList)

    println(Stream(1, 2, 3, 4, 5).flatMapByFoldRight((x: Int) => Stream(x + 1)))
    assert(List(2, 3, 4, 5, 6) == Stream(1, 2, 3, 4, 5).flatMapByFoldRight((x: Int) => Stream(x + 1)).toList)

    println(Stream.ones.take(5).toList)
    println(Stream.ones.existsByFoldRight(_ % 2 != 0))
    println(Stream.ones.mapByFoldRight(_ + 1).existsByFoldRight(_ % 2 == 0))
    println(Stream.ones.forAllByFoldRight(_ != 1))
  }

  test("Exercise 7 - Generalize ones slightly to the function constant") {
    println(Stream.constant(5).take(10).toList)
  }

  test("Exercise 8 - Write a function that generates an infinite streams of integers, starting from n, then n + 1, n + 2, etc") {
    println(Stream.from(5).take(5).toList)
  }

  test("Exercise 9 - Write a function fibs that generates the infinite stream of Fibonacci numbers") {
    println(List(1, 2, 3, 5, 8) == Stream.fibs.take(5).toList)
  }

  test("Exercise 10 - unfold") {
    // Write function unfold
  }

  test("Exercise 11 - Write fibs, from, constant, and ones in the terms of unfold") {
    assert(List(0, 1, 1, 2, 3, 5, 8) == Stream.fibsViaUnfold.take(7).toList)
    assert(List(5, 6, 7, 8, 9) == Stream.fromViaUnfold(5).take(5).toList)
    assert(List(5, 5, 5, 5, 5) == Stream.constantViaUnfold(5).take(5).toList)
    assert(List(1, 1, 1, 1, 1) == Stream.onesViaUnfold.take(5).toList)
  }

  test("Exercise 12 - Use unfold to implement map, take, takeWhile, and zip") {
    assert(List(2, 4, 6, 8, 10) == Stream(1, 2, 3, 4, 5).mapViaUnfold((x: Int) => x * 2).take(5).toList)
  }

  test("Exercise 13 - implement startsWith using function you have already written") {
    assert(Stream.startsWith(Stream(1, 2, 3, 4, 5), Stream(1, 2, 3)))
  }

  test("Exercise 14 - tails") {
    println(Stream(1, 2, 3, 4, 5).tails)
  }

  test("Generalize tails to the function scanRight, which is like foldRight that returns a stream of the intermediate results") {
    println(Stream(1,2,3).scanRight(0)(_ + _).toList)
  }


}
