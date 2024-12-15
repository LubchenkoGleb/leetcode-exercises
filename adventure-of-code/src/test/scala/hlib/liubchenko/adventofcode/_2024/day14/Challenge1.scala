package hlib.liubchenko.adventofcode._2024.day14

import hlib.liubchenko.adventofcode._2024.Utils
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Challenge1 extends AnyWordSpec with Matchers {
  def countRobots(robots: List[String], width: Int, height: Int, seconds: Int): Long = {
    val regex = "p=(\\d+),(\\d+) v=([-\\d]+),([-\\d]+)".r

    val (q1, q2, q3, q4) = robots
      .map { case regex(x, y, vX, vY) => (x.toInt, y.toInt, vX.toInt, vY.toInt) }
      .map { case (x, y, vX, vY) =>
        val fX = (x + seconds * vX) % width
        val fY = (y + seconds * vY) % height
        (if (fX >= 0) fX else width + fX, if (fY >= 0) fY else height + fY)
      }
      .foldLeft((0, 0, 0, 0)) { case ((q1, q2, q3, q4), (x, y)) =>
        val (wMid, hMid) = (width / 2, height / 2)
        if (x < wMid && y < hMid) (q1 + 1, q2, q3, q4)
        else if (x > wMid && y < hMid) (q1, q2 + 1, q3, q4)
        else if (x < wMid && y > hMid) (q1, q2, q3 + 1, q4)
        else if (x > wMid && y > hMid) (q1, q2, q3, q4 + 1)
        else (q1, q2, q3, q4)
      }

    q1.toLong * q2 * q3 * q4
  }

  "Day #14 Challenge #1" should {
    "work as expected #1" in {
      countRobots(
        List(
          "p=0,4 v=3,-3",
          "p=6,3 v=-1,-3",
          "p=10,3 v=-1,2",
          "p=2,0 v=2,-1",
          "p=0,0 v=1,3",
          "p=3,0 v=-2,-2",
          "p=7,6 v=-1,-3",
          "p=3,0 v=-1,-2",
          "p=9,3 v=2,3",
          "p=7,3 v=-1,2",
          "p=2,4 v=2,-3",
          "p=9,5 v=-3,-3"
        ),
        width = 11,
        height = 7,
        seconds = 100
      ) shouldBe 12
    }

    "work as expected #2" in {
      val input = Utils.readInputFile(14)
      countRobots(input, 101, 103, 100) shouldBe 228690000
    }
  }
}
