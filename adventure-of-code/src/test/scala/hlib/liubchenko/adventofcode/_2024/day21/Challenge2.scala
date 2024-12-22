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

  def enterCode(code: String): Long = {
    def loop(level: Int)(code: String): Long = {
      println(s"level: $level, code: $code")
      if (level == 25) code.length
      else {
        val codes = navigateDirectionalKeypad(code)
        codes.map(loop(level + 1)).min
      }
    }

    navigateNumericKeypad(code).map { loop(1) }.min
  }

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

  def navigateDirectionalKeypad(code: String, x: Int = 2, y: Int = 0): List[String] = if (code.isEmpty) List("")
  else {
    val (targetX, targetY) = code.head match {
      case 'A' => (2, 0)
      case '^' => (1, 0)
      case 'v' => (1, 1)
      case '<' => (0, 1)
      case '>' => (2, 1)
    }

    val yDirection = if (targetY < y) "^" else "v"
    val yMoves = Seq.fill(math.abs(targetY - y))(yDirection).mkString

    val xDirection = if (targetX < x) "<" else ">"
    val xMoves = Seq.fill(math.abs(targetX - x))(xDirection).mkString

    val moves =
      if (x == 0 && targetY == 0) List(xMoves + yMoves)
      else if (y == 0 && targetX == 0) List(yMoves + xMoves)
      else List(yMoves + xMoves, xMoves + yMoves).distinct

    for {
      rem <- navigateDirectionalKeypad(code.tail, targetX, targetY)
      move <- moves
    } yield move + "A" + rem
  }

  "Day #21 Challenge #2" should {
    "work as expected #1" in {
      enterCodes(List.empty) shouldBe 0
    }

    "work as expected #2" in {
      val input = Utils.readInputFile(21)
      enterCodes(input) shouldBe 0
    }
  }
}
