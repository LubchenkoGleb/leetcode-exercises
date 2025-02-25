package hlib.liubchenko.topinterview150.backtracking

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _7_word_search extends AnyWordSpec with Matchers {
  def exist(board: Array[Array[Char]], word: String): Boolean = {
    val h = board.length
    val w = board.head.length

    def check(word: String, i: Int, j: Int): Boolean =
      if (word.isEmpty) true
      else if (i < 0 || i >= h || j < 0 || j >= w || word.head != board(i)(j)) false
      else {
        val rem = word.tail
        board(i)(j) = '#'
        val res = check(rem, i + 1, j) || check(rem, i - 1, j) || check(rem, i, j + 1) || check(rem, i, j - 1)
        board(i)(j) = word.head
        res
      }

    val iterator = for { i <- board.indices.iterator; j <- board.head.indices.iterator } yield (i, j)
    iterator.exists { case (i, j) => check(word, i, j) }
  }

  "exist" should {
    "work as expected #1" in {
      val board = Array(
        Array('A', 'B', 'C', 'E'),
        Array('S', 'F', 'C', 'S'),
        Array('A', 'D', 'E', 'E')
      )
      exist(board, "ABCCED") shouldBe true
      exist(board, "SEE") shouldBe true
      exist(board, "ABCB") shouldBe false
    }
  }
}
