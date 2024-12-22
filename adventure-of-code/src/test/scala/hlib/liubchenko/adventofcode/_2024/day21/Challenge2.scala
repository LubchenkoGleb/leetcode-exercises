package hlib.liubchenko.adventofcode._2024.day21

import hlib.liubchenko.adventofcode._2024.Utils
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Challenge2 extends AnyWordSpec with Matchers {
  def enterCodes(codes: List[String]): Long = codes
    .map(enterCode)
    .zip(codes)
    .map { case (commandLen, code) =>
      val numberPart = code.filter(_.isDigit).toInt
      commandLen * numberPart
    }
    .sum

  def enterCode(code: String): Long =
    navigateNumericKeypad(code).map { navigateDirectionalKeypad(_, 25) }.min

  def navigateNumericKeypad(code: String, x: Int = 2, y: Int = 3): List[String] = if (code.isEmpty) List("")
  else {
    val (targetX, targetY) = code.head match {
      case 'A' => (2, 3)
      case '0' => (1, 3)
      case '1' => (0, 2)
      case '2' => (1, 2)
      case '3' => (2, 2)
      case '4' => (0, 1)
      case '5' => (1, 1)
      case '6' => (2, 1)
      case '7' => (0, 0)
      case '8' => (1, 0)
      case '9' => (2, 0)
    }

    val yDirection = if (targetY < y) "^" else "v"
    val yMoves = Seq.fill(math.abs(targetY - y))(yDirection).mkString

    val xDirection = if (targetX < x) "<" else ">"
    val xMoves = Seq.fill(math.abs(targetX - x))(xDirection).mkString

    val moves =
      if (y == 3 && targetX == 0) List(yMoves + xMoves)
      else if (x == 0 && targetY == 3) List(xMoves + yMoves)
      else List(yMoves + xMoves, xMoves + yMoves).distinct

    for {
      rem <- navigateNumericKeypad(code.tail, targetX, targetY)
      move <- moves
    } yield move + "A" + rem
  }

  def navigateDirectionalKeypad(code: String, level: Int): Long = {
    val memo = collection.mutable.HashMap.empty[(String, Int), Long]

    def loop(code: String, level: Int): Long = {
      def value = if (level == 0) code.length
      else
        code
          .foldLeft(('A', 0L)) { case ((curr, acc), target) =>
            def cord(c: Char) = c match {
              case 'A' => (2, 0)
              case '^' => (1, 0)
              case 'v' => (1, 1)
              case '<' => (0, 1)
              case '>' => (2, 1)
            }

            val (x, y) = cord(curr)
            val (targetX, targetY) = cord(target)

            val yDirection = if (targetY < y) "^" else "v"
            val yMoves = Seq.fill(math.abs(targetY - y))(yDirection).mkString

            val xDirection = if (targetX < x) "<" else ">"
            val xMoves = Seq.fill(math.abs(targetX - x))(xDirection).mkString

            val moves =
              if (x == 0 && targetY == 0) List(xMoves + yMoves)
              else if (y == 0 && targetX == 0) List(yMoves + xMoves)
              else List(yMoves + xMoves, xMoves + yMoves).distinct

            val res = moves.map(_ + "A").map { loop(_, level - 1) }
            (target, acc + res.min)
          }
          ._2

      memo.getOrElseUpdate((code, level), value)
    }

    loop(code, level)
  }

  "Day #21 Challenge #2" should {
    "work as expected #1" in {
      // Code:
      // +---+---+---+
      // | 7 | 8 | 9 |
      // +---+---+---+
      // | 4 | 5 | 6 |
      // +---+---+---+
      // | 1 | 2 | 3 |
      // +---+---+---+
      //     | 0 | A | <
      //     +---+---+
      //     +---+---+
      //  L2 | ^ | A | <(v<<A)
      // +---+---+---+
      // | < | v | > |
      // +---+---+---+
      //     +---+---+
      //  L1 | ^ | A | v(v<A) <(<A) <(A)
      // +---+---+---+
      // | < | v | > |
      // +---+---+---+
      //     +---+---+
      //   I | ^ | A |
      // +---+---+---+
      // | < | v | > |   v<A<A
      // +---+---+---+
      enterCodes(List("029A")) shouldBe 2379451789590L
    }

    "work as expected #2" in {
      val input = Utils.readInputFile(21)
      enterCodes(input) shouldBe 242337182910752L
    }
  }

  "navigateDirectionalKeypad" should {
    "work as expected" in {
      navigateDirectionalKeypad("<", 1) shouldBe 4
      navigateDirectionalKeypad("<", 2) shouldBe 10
      navigateDirectionalKeypad("<A^A>^^AvvvA", 2) shouldBe 68
      navigateDirectionalKeypad("<", 25) shouldBe 12192864310L
    }
  }
}
