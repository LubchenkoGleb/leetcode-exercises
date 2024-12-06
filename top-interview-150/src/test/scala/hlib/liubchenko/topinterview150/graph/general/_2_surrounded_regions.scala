package hlib.liubchenko.topinterview150.graph.general

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _2_surrounded_regions extends AnyWordSpec with Matchers {
  def solve(board: Array[Array[Char]]): Unit = ???

  "solve" should {
    pending

    "work as expected #1" in {
      val graph = Array(
        Array('X', 'X', 'X', 'X'),
        Array('X', 'O', 'O', 'X'),
        Array('X', 'X', 'O', 'X'),
        Array('X', 'O', 'X', 'X')
      )
      solve(graph)
      graph.map(_.toList).toList shouldBe List(
        List('X', 'X', 'X', 'X'),
        List('X', 'X', 'X', 'X'),
        List('X', 'X', 'X', 'X'),
        List('X', 'O', 'X', 'X')
      )
    }

    "work as expected #2" in {
      val graph = Array(
        Array('X')
      )
      solve(graph)
      graph.map(_.toList).toList shouldBe List(List('X'))
    }
  }
}
