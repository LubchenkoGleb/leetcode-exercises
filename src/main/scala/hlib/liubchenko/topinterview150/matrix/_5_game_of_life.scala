package hlib.liubchenko.topinterview150.matrix

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _5_game_of_life extends AnyWordSpec with Matchers {
  def gameOfLife(board: Array[Array[Int]]): Unit = ???

  "gameOfLife" should {
    "work as expected #1" in {
      val matrix = Array(
        Array(0, 1, 0),
        Array(0, 0, 1),
        Array(1, 1, 1),
        Array(0, 0, 0)
      )
      gameOfLife(matrix)
      matrix.map(_.toList).toList shouldBe List(
        List(0, 0, 0),
        List(1, 0, 1),
        List(0, 1, 1),
        List(0, 1, 0)
      )
    }

    "work as expected #2" in {
      val matrix = Array(
        Array(1, 1),
        Array(0, 1)
      )
      gameOfLife(matrix)
      matrix.map(_.toList).toList shouldBe List(
        List(1, 1),
        List(1, 1)
      )
    }
  }
}
