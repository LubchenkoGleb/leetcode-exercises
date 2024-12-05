package hlib.liubchenko.topinterview150.backtracking

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _7_word_search extends AnyWordSpec with Matchers {
  def exist(board: Array[Array[Char]], word: String): Boolean = ???

  "exist" should {
    "work as expected #1" in {
      exist(
        Array(
          Array('A', 'B', 'C', 'E'),
          Array('S', 'F', 'C', 'S'),
          Array('A', 'D', 'E', 'E')
        ),
        "ABCCED"
      ) shouldBe true
    }

    "work as expected #2" in {
      exist(
        Array(
          Array('A', 'B', 'C', 'E'),
          Array('S', 'F', 'C', 'S'),
          Array('A', 'D', 'E', 'E')
        ),
        "SEE"
      ) shouldBe true
    }
  }
}
