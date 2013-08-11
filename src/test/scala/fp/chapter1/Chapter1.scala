package scala.fp.chapter1

import org.scalatest.FunSuite

class Chapter1 extends FunSuite {

  test("hello world") {
    val r1 = "Hello world"
    val r2 = "Hello world"

    println(r1.reverse)
    println(r2.reverse)
    println("Hello world".reverse)
  }

  test("interwining branching logic with displaying result") {
    case class Player(name : String, score : Int) {
      def declareWinner(p : Player) : Unit = println(p.name + " is the winner!")

      def winner(p1 : Player, p2 : Player) : Unit = if (p1.score > p2.score) declareWinner(p1) else declareWinner(p2)

      val sue = Player("Sue", 7)
      val bob = Player("Bob", 8)

      winner(sue, bob)
    }
  }

  test("referentially transparent and impure part separated") {
    case class Player(name : String, score : Int) {
      def maxScore(p1 : Player, p2 : Player) : Player = {
        if (p1.score > p2.score) p1 else p2
      }

      def declareWinner(p : Player) : Unit = println(p.name + " is the winner!")

      def winner(p1 : Player, p2 : Player) {
        declareWinner(maxScore(p1,p2))
      }

      val players = List(Player("Sue", 7),
        Player("Bob", 8),
        Player("Joe", 4))

      val winner = players.reduceLeft(maxScore) // can now do this
      declareWinner(winner)
    }
  }
}
