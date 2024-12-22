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

//    val res =
//      if (yDirection == "v") xMoves + yMoves
//      else yMoves + xMoves

    val moves =
      if (targetX == 0 && y == 3) List(yMoves + xMoves)
      else if (targetX == 2 && y == 0) List(xMoves + yMoves)
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

    val res =
      if (yDirection == "v") yMoves + xMoves
      else xMoves + yMoves

    navigateDirectionalKeypad(code.tail, targetX, targetY).map(res + "A" + _)
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
//        "029A", // 68 * 29
//        "980A", // 60 * 980
//        "179A", // 68 * 179,
//        "456A", // 64 * 456
        "379A" //  64 * 379
      )
      enterCodes(input) shouldBe 126384
    }

    "work as expected with input file" in {
      val input = Utils.readInputFile(21)
      enterCodes(input) shouldBe 0
    }
  }

  "navigateNumericKeypad" should {
    "work as expected" in {
      navigateNumericKeypad("029A") shouldBe List("<A^A^^>AvvvA")
      navigateNumericKeypad("379A") shouldBe List("^A^^<<A>>AvvvA") // <A^A>^^AvvvA
    }
  }

  "navigateDirectionalKeyPad" should {
    "work as expected #1" in {
      val l2 = "v<<A>>^A<A>AvA<^AA>Av<AAA>^A"
      navigateDirectionalKeypad("<A^A>^^AvvvA") shouldBe List(l2)
      navigateDirectionalKeypad(l2) shouldBe List(
        "v<A<AA>>^AvAA<^A>Av<<A>>^AvA^Av<A>^Av<<A>^A>AAvA^Av<A<A>>^AAAvA<^A>A"
      )
    }

    "work as expected #2" in {
      val l2 = "<A>A<AAv<AA>>^AvAA^Av<AAA>^A" // l=28
      navigateDirectionalKeypad("^A^^<<A>>AvvvA") shouldBe List(l2)
      // l1 - 1(^A) 2(^^<<A) 3(>>A) 4(vvvA)
      // l2 - 1(<A) 2(>A) 3(<A) 4(A) 5(v<A) 6(A) 7(>>^A) 8(vA) 9(A) 10(^A) 11(v<A) 12(A) 13(A) 14(>^A)
      // 1:^  - <A
      // 2:A  - >A
      // 3:^  - <A
      // 4:^  - A
      // 5:<  - v<A
      // 6:<  - A
      // 7:A  - >>^A
      // 8:>  - vA
      // 9:>  - A
      // 10:A - ^A
      // 11:v - v<A
      // 12:v - A
      // 13:v - A
      // 14:A - >^A

      //      +---+---+
      //     | ^ | A |
      // +---+---+---+
      // | < | v | > |
      // +---+---+---+
      // l2 - 1:<A 2:>A 3:<A 4:A 5:v<A 6:A>>^ 7:A 8:vA 9:A 10:^A 11:v<A 12:A 13:A 14:>^A
      // l2 - 1:<    2:A    3:>  4:A  5:<    6:A    7:A 8:v   9:<  10:A    11:A 12:>  13:> 14:^   15:A  16:v   17:A   18:A 19:^  20:A  21:v   22:<  23:A    24:A 25:A 26:>  27:^   28:A
      // l3 - 1:v<<A 2:>>^A 3:vA 4:^A 5:v<<A 6:>>^A 7:A 8:v<A 9:<A 10:>>^A 11:A 12:vA 13:A 14:<^A 15:>A 16:v<A 17:>^A 18:A 19:<A 20:>A 21:v<A 22:<A 23:>>^A 24:A 25:A 26:vA 27<^A: 28:>A - actual
      // 1:<  - v<<A +
      // 2:A  - >>^A +
      // 3:>  - vA   +
      // 4:A  - ^A   +
      // 5:<  - v<<A +
      // 6:A  - >>^A
      // 7:A  - A
      // 8:v  - v<A
      // 9:<  - <A
      // 10:A - >>^A
      // 11:A - A
      // 12:> - vA
      // 13:> - A
      // 14:^ - ^<A
      // 15:A - >A
      // 16:v - v<A
      // 17:A - >^A
      // 18:A - A
      // 19:^ - <A
      // 20:A - >A
      // 21:v - v<A
      // 22:< - <A
      // 23:A - >>^A
      // 24:A - A
      // 25:A - A
      // 26:> - vA
      // 27:^ - ^<A
      // 28:A - >A
      navigateDirectionalKeypad(l2) shouldBe List(
        "v<<A>>^AvA^Av<<A>>^AAv<A<A>>^AAvAA<^A>Av<A>^AA<A>Av<A<A>>^AAAvA<^A>A"
      )
    }
  }

  "enterCode" should {
    "work as expected" in {
      enterCode("379A").length shouldBe 64
    }
  }
}
