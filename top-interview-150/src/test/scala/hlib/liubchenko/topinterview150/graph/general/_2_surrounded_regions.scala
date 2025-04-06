package hlib.liubchenko.topinterview150.graph.general

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _2_surrounded_regions extends AnyWordSpec with Matchers {
  def solve(board: Array[Array[Char]]): Unit = {
    val (m, n) = (board.length, board.head.length)

    def markNotConnected(i: Int, j: Int): Unit = if (i >= 0 && i < m && j >= 0 && j < n && board(i)(j) == 'O') {
      board(i)(j) = '.'
      markNotConnected(i + 1, j)
      markNotConnected(i - 1, j)
      markNotConnected(i, j + 1)
      markNotConnected(i, j - 1)
    }

    (0 until m).foreach { i =>
      markNotConnected(i, 0)
      markNotConnected(i, n - 1)
    }

    (0 until n).foreach { j =>
      markNotConnected(0, j)
      markNotConnected(m - 1, j)
    }

    for {
      i <- 0 until m
      j <- 0 until n
    } {
      if (board(i)(j) == '.') board(i)(j) = 'O'
      else if (board(i)(j) == 'O') board(i)(j) = 'X'
    }
  }

  "solve" should {
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
