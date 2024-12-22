package hlib.liubchenko.adventofcode._2024.day21

import hlib.liubchenko.adventofcode._2024.Utils
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Challenge1 extends AnyWordSpec with Matchers {
  def enterCodes(codes: List[String]): Int = codes
    .map(enterCode)
    .zip(codes)
    .map { case (command, code) =>
      val length = command.length
      val numberPart = code.filter(_.isDigit).toInt
      println(s"code: $code, $length * $numberPart, $command")
      length * numberPart
    }
    .sum

  def enterCode(code: String): String = {
    navigateNumericKeypad(code)
      .flatMap(navigateDirectionalKeypad(_))
      .flatMap(navigateDirectionalKeypad(_))
      .minBy(_.length)
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

  "Day #21 Challenge #1" should {
    // +---+---+---+
    // | 7 | 8 | 9 |
    // +---+---+---+
    // | 4 | 5 | 6 |
    // +---+---+---+
    // | 1 | 2 | 3 |
    // +---+---+---+
    //     | 0 | A |
    //     +---+---+

    //      +---+---+
    //     | ^ | A |
    // +---+---+---+
    // | < | v | > |
    // +---+---+---+

    "work as expected #1" in {
      enterCodes(List("029A")) shouldBe 1972
    }

    "work as expected #2" in {
      val input = List(
        "029A", // 68 * 29
        "980A", // 60 * 980
        "179A", // 68 * 179,
        "456A", // 64 * 456
        "379A" //  64 * 379
      )
      enterCodes(input) shouldBe 126384
    }

    "work as expected with input file" in {
      val input = Utils.readInputFile(21)
      enterCodes(input) shouldBe 197560
    }
  }

  "navigateNumericKeypad" should {
    "work as expected" in {
      navigateNumericKeypad("029A") should contain theSameElementsAs List("<A^A^^>AvvvA", "<A^A>^^AvvvA")
      navigateNumericKeypad("379A") should contain theSameElementsAs List("^A^^<<A>>AvvvA", "^A<<^^A>>AvvvA")
    }
  }

  "navigateDirectionalKeyPad" should {
    "work as expected #0" in {
      navigateDirectionalKeypad("<").flatMap(navigateDirectionalKeypad(_)) should contain theSameElementsAs
        List("v<A<AA>>^A", "<vA<AA>>^A")
    }

    "work as expected #1" in {
      val expected = List(
        "v<<A>>^A<A>AvA^<AA>Av<AAA^>A",
        "v<<A>>^A<A>AvA<^AA>Av<AAA^>A",
        "v<<A>>^A<A>AvA^<AA>A<vAAA^>A",
        "v<<A>>^A<A>AvA<^AA>A<vAAA^>A",
        "v<<A>>^A<A>AvA^<AA>Av<AAA>^A",
        "v<<A>>^A<A>AvA<^AA>Av<AAA>^A",
        "v<<A>>^A<A>AvA^<AA>A<vAAA>^A",
        "v<<A>>^A<A>AvA<^AA>A<vAAA>^A"
      )
      navigateDirectionalKeypad("<A^A>^^AvvvA") should contain theSameElementsAs expected
    }

    "work as expected #2" in {
      val expected = List(
        "<A>A<AAv<AA>>^AvAA^Av<AAA^>A",
        "<A>A<AAv<AA>>^AvAA^A<vAAA^>A",
        "<A>A<AAv<AA>>^AvAA^Av<AAA>^A",
        "<A>A<AAv<AA>>^AvAA^A<vAAA>^A"
      )
      navigateDirectionalKeypad("^A^^<<A>>AvvvA") should contain theSameElementsAs expected
    }
  }

  "enterCode" should {
    "work as expected" in {
      enterCode("379A").length shouldBe 64
    }
  }
}
