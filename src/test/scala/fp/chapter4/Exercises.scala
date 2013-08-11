package scala.fp.chapter4

import org.scalatest.FunSuite
import Option._

class Exercises extends FunSuite {

  test("Exercise 1 - Implementing functions on Option") {

    val lastName: Option[String] = Some("Rossgard")
    val lastNameNotSet: Option[String] = None

    // Map
    println("map: " + lastName.map(a => a))
    println("map: " + lastNameNotSet.map(a => a))

    // FlatMap
    println("flatMap: " + lastName.flatMap(a => Some(a)))
    println("flatMap: " + lastNameNotSet.flatMap(a => Some(a)))

    // GetOrElse
    println("getOrElse: " + lastName.getOrElse("Default last name"))
    println("getOrElse: " + lastNameNotSet.getOrElse("Default last name"))

    // Or else
    println("orElse: " + lastName.orElse(Some("Default last name")))
    println("orElse: " + lastNameNotSet.orElse(Some("Default last name")))

    // Filter
    println("filter: " + lastName.filter(_ == "Rossgard"))
    println("filter: " + lastNameNotSet.filter(_ == "Rossgard"))
  }

  test("Exercise 2 - Implement the variance function") {
    println(variance(Seq(1, 5)))
  }

  test("Exercise 3 - Write generic function map2, that combines two Option values using a binary function") {
    val number1: Option[Int] = Some(10)
    val number2: Option[Int] = Some(20)

    println(map2(number1, number2)(_ + _))
  }

  test("Exercise 4 - Re-implement bothMatch") {
    val string1 = "test"
    val string2 = "test"

    println(bothMatch("test", string1, string2))
  }

  test("Exercise 5 - Write a function Sequence") {
    println(sequence(List(Some(1), Some(2), Some(3))))
  }

  test("Exercise 6 - Write a function Traverse") {
    println(traverse(List[Int](1, 2, 3))((e: Int) => Some(e)))
  }

  test("Excercise 7 - Implement versions of map, flatMap, orElse, and map2 on Either that operates on the Right value") {
    // done
  }

  test("Exercise 8 - Implement sequence and traverse for Either") {
    // done
  }

  test("Exercise 9 - See either") {
    // done
  }
}
