package hlib.liubchenko.adventofcode._2024.day13

import hlib.liubchenko.adventofcode._2024.Utils
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Challenge1 extends AnyWordSpec with Matchers {
  def winPrize(lines: List[String]): Int = {
    def calc_v2(aX: Int, aY: Int, bX: Int, bY: Int, prizeX: Int, prizeY: Int): Int = {
      val winningCombinations = for {
        n1 <- 0 to 100
        n2 <- 0 to 100
        if aX * n1 + bX * n2 == prizeX && aY * n1 + bY * n2 == prizeY
        _ = println(s"n1: $n1, n2: $n2")
      } yield n1 * 3 + n2

      winningCombinations.minOption.getOrElse(0)
    }

    def calc(aX: Float, aY: Float, bX: Float, bY: Float, prizeX: Float, prizeY: Float): Int = {
      val n2 = (prizeY - (aY * (prizeX / aX)) + (aY * bX / aX)) / (bY - (aY * bX / aX))
      val n1 = (prizeX - bX * n2) / aX
      val (rN1, rN2) = (math.round(n1), math.round(n2))

      val isValid = rN1 <= 100 &&
        rN2 <= 100 &&
        aX * rN1 + bX * rN2 == prizeX &&
        aY * rN1 + bY * rN2 == prizeY
      println(s"n1: $n1, n2: $n2, isValid: $isValid")

      if (isValid) rN1 * 3 + rN2 else 0
    }


    val aR = "Button A: X\\+(\\d+), Y\\+(\\d+)".r
    val bR = "Button B: X\\+(\\d+), Y\\+(\\d+)".r
    val prizeR = "Prize: X=(\\d+), Y=(\\d+)".r

    lines
      .sliding(3, 4)
      .map { case aR(aX, aY) :: bR(bX, bY) :: prizeR(prizeX, prizeY) :: Nil =>
        // calc(aX.toInt, aY.toInt, bX.toInt, bY.toInt, prizeX.toInt, prizeY.toInt)
         calc(aX.toFloat, aY.toFloat, bX.toFloat, bY.toFloat, prizeX.toFloat, prizeY.toFloat)
      }
      .sum
  }

  def calc_v1(aX: Float, aY: Float, bX: Float, bY: Float, prizeX: Float, prizeY: Float): Int = {
    val n2 = (prizeY - (aY * (prizeX / aX)) + (aY * bX / aX)) / (bY - (aY * bX / aX))
    val n1 = (prizeX - bX * n2) / aX
    val (rN1, rN2) = (math.round(n1), math.round(n2))

    val isValid = rN1 <= 100 &&
      rN2 <= 100 &&
      aX * rN1 + bX * rN2 == prizeX &&
      aY * rN1 + bY * rN2 == prizeY
    println(s"n1: $n1, n2: $n2, isValid: $isValid")

    if (isValid) rN1 * 3 + rN2 else 0
  }

  "Day #13 Challenge #1" should {
    "work as expected #1" in {
      winPrize(
        List(
          "Button A: X+94, Y+34",
          "Button B: X+22, Y+67",
          "Prize: X=8400, Y=5400",
          "",
          "Button A: X+26, Y+66",
          "Button B: X+67, Y+21",
          "Prize: X=12748, Y=12176",
          "",
          "Button A: X+17, Y+86",
          "Button B: X+84, Y+37",
          "Prize: X=7870, Y=6450",
          "",
          "Button A: X+69, Y+23",
          "Button B: X+27, Y+71",
          "Prize: X=18641, Y=10279"
        )
      ) shouldBe 480
    }

    "work as expected #2" in {
      val input = Utils.readInputFile(13)
      winPrize(input) shouldBe 27105
    }
  }
}
