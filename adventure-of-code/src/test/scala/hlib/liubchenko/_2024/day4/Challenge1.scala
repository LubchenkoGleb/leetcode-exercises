package hlib.liubchenko._2024.day4

import hlib.liubchenko._2024.Utils
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Challenge1 extends AnyWordSpec with Matchers {
  def findWords(lines: List[String]): Int = {
    val inputArray = lines.map(_.toArray).toArray

    def sc(i: Int, j: Int, value: Char): Boolean =
      i >= 0 && i < inputArray.head.length &&
        j >= 0 && j < inputArray.length &&
        inputArray(i)(j) == value
    def x(i: Int, j: Int) = sc(i, j, 'X')
    def m(i: Int, j: Int) = sc(i, j, 'M')
    def a(i: Int, j: Int) = sc(i, j, 'A')
    def s(i: Int, j: Int) = sc(i, j, 'S')
    def validate(letters: Boolean*): Int = if (letters.forall(identity)) 1 else 0

    val res = for {
      i <- inputArray.indices
      j <- inputArray.head.indices
    } yield validate(x(i, j), m(i, j + 1), a(i, j + 2), s(i, j + 3)) + // →
      validate(x(i, j), m(i, j - 1), a(i, j - 2), s(i, j - 3)) + // ←
      validate(x(i, j), m(i + 1, j), a(i + 2, j), s(i + 3, j)) + // ↑
      validate(x(i, j), m(i - 1, j), a(i - 2, j), s(i - 3, j)) + // ↓
      validate(x(i, j), m(i - 1, j - 1), a(i - 2, j - 2), s(i - 3, j - 3)) + // ↖
      validate(x(i, j), m(i - 1, j + 1), a(i - 2, j + 2), s(i - 3, j + 3)) + // ↗
      validate(x(i, j), m(i + 1, j + 1), a(i + 2, j + 2), s(i + 3, j + 3)) + // ↘
      validate(x(i, j), m(i + 1, j - 1), a(i + 2, j - 2), s(i + 3, j - 3)) // ↙

    res.sum
  }

  "Day #4 Challenge #1 findWords" should {
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
      ) shouldBe 18
    }

    "work as expected #2" in {
      val input = Utils.readInputFile(4)
      findWords(input) shouldBe 2530
    }
  }
}
