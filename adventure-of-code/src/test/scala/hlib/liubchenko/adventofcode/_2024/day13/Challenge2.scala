package hlib.liubchenko.adventofcode._2024.day13

import hlib.liubchenko.adventofcode._2024.Utils
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Challenge2 extends AnyWordSpec with Matchers {
  private val shift = 10000000000000L

  def solveEquations(aX: Int, aY: Int, bX: Int, bY: Int, prizeX: Long, prizeY: Long): Long = {
    // Coefficients for the transformed equation
    val A = bY * aX - bX * aY
    val B = prizeY * aX - prizeX * aY

    // Check if A divides B
    if (A == 0 || B % A != 0) 0
    else {
      val n2 = B / A
      val n1 = (prizeX - bX * n2) / aX

      if (aX * n1 + bX * n2 == prizeX && aY * n1 + bY * n2 == prizeY) n1 * 3 + n2
      else 0
    }
  }

  def winPrize(lines: List[String]): Long = {
    val aR = "Button A: X\\+(\\d+), Y\\+(\\d+)".r
    val bR = "Button B: X\\+(\\d+), Y\\+(\\d+)".r
    val prizeR = "Prize: X=(\\d+), Y=(\\d+)".r

    lines
      .sliding(3, 4)
      .map { case aR(aX, aY) :: bR(bX, bY) :: prizeR(prizeX, prizeY) :: Nil =>
        solveEquations(aX.toInt, aY.toInt, bX.toInt, bY.toInt, prizeX.toLong + shift, prizeY.toLong + shift)
      }
      .sum
  }

  "Day #13 Challenge #2" should {
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
      ) shouldBe 875318608908L
    }

    "work as expected #2" in {
      val input = Utils.readInputFile(13)
      winPrize(input) shouldBe 101726882250942L
    }
  }
}
