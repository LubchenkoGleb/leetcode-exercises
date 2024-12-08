package hlib.liubchenko._2024.day7

import hlib.liubchenko._2024.Utils
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.annotation.tailrec

class Challenge2 extends AnyWordSpec with Matchers {
  def calibrate(lines: List[String]): Long = {
    def validate(expected: Long, numbers: List[Long]): Boolean = {
      def applyOp(a: Long, b: Long, c: Char) = c match {
        case '+' => a + b
        case '*' => a * b
        case '|' => (a.toString + b.toString).toLong
      }

      @tailrec
      def calc(numbers: List[Long], operations: List[Char]): Long = numbers match {
        case a :: b :: Nil => applyOp(a, b, operations.head)
        case a :: b :: rem => calc(applyOp(a, b, operations.head) +: rem, operations.tail)
      }

      val n = numbers.length - 1
      combinations(n, List('+', '*', '|')).exists { calc(numbers, _) == expected }
    }

    lines
      .map { str =>
        str.split(": ") match {
          case Array(expected, numbers) => expected.toLong -> numbers.split(" ").map(_.toLong).toList
        }
      }
      .collect { case (expected, numbers) if validate(expected, numbers) => expected }
      .sum
  }

  def combinations(n: Int, values: List[Char]): List[List[Char]] =
    if (n == 0) List(Nil)
    else values.flatMap(v => combinations(n - 1, values).map(rem => v :: rem))

  "Day #7 Challenge #1" should {
    "work as expected #1" in {
      calibrate(
        List(
          "190: 10 19",
          "3267: 81 40 27",
          "83: 17 5",
          "156: 15 6",
          "7290: 6 8 6 15",
          "161011: 16 10 13",
          "192: 17 8 14",
          "21037: 9 7 18 13",
          "292: 11 6 16 20"
        )
      ) shouldBe 11387
    }

    "work as expected #2" in {
      val input = Utils.readInputFile(7)
      calibrate(input) shouldBe 275791737999003L
    }
  }
}
