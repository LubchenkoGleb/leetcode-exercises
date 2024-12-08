package hlib.liubchenko._2024.day4

import hlib.liubchenko._2024.Utils
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Challenge2 extends AnyWordSpec with Matchers {
  def findWords(lines: List[String]): Int = {
    val inputArray = lines.map(_.toArray).toArray

    def sc(i: Int, j: Int, value: Char): Boolean =
      i >= 0 && i < inputArray.head.length &&
        j >= 0 && j < inputArray.length &&
        inputArray(i)(j) == value

    def m(i: Int, j: Int) = sc(i, j, 'M')

    def a(i: Int, j: Int) = sc(i, j, 'A')

    def s(i: Int, j: Int) = sc(i, j, 'S')

    def validate(letters: Boolean*): Boolean = letters.forall(identity)

    val res = for {
      i <- inputArray.indices
      j <- inputArray.head.indices
    } yield {
      val res = (
        validate(m(i, j), a(i + 1, j + 1), s(i + 2, j + 2)) ||
          validate(s(i, j), a(i + 1, j + 1), m(i + 2, j + 2))
      ) && (
        validate(m(i, j + 2), a(i + 1, j + 1), s(i + 2, j)) ||
          validate(s(i, j + 2), a(i + 1, j + 1), m(i + 2, j))
      )
      if (res) 1 else 0
    }

    res.sum
  }

  "Day #4 Challenge #2" should {
    "work as expected #1" in {
      findWords(
        List(
          "MMMSXXMASM",
          "MSAMXMSMSA",
          "AMXSXMAAMM",
          "MSAMASMSMX",
          "XMASAMXAMM",
          "XXAMMXXAMA",
          "SMSMSASXSS",
          "SAXAMASAAA",
          "MAMMMXMMMM",
          "MXMXAXMASX"
        )
      ) shouldBe 9
    }

    "work as expected #2" in {
      val input = Utils.readInputFile(4)
      findWords(input) shouldBe 1921
    }
  }
}
