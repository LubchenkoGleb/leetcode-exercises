package hlib.liubchenko.adventofcode._2024.day8

import hlib.liubchenko.adventofcode._2024.Utils
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Challenge2 extends AnyWordSpec with Matchers {
  def countAntinodes(lines: List[String]): Int = {
    val map = lines.map(_.toArray).toArray

    val antennas = for {
      i <- map.indices
      j <- map.head.indices
      if map(i)(j) != '.'
    } yield (i, j)

    def getAntinodes(y1: Int, x1: Int, y2: Int, x2: Int): List[(Int, Int)] = {
      val (leftX, rightX, xDiff) = (math.min(x1, x2), math.max(x1, x2), math.abs(x1 - x2))
      val (topY, bottomY, yDiff) = (math.min(y1, y2), math.max(y1, y2), math.abs(y1 - y2))

      def move(y: Int, x: Int, yBy: Int, xBy: Int) = {
        (y + yDiff * yBy, x + xDiff * xBy)
      }

      def generate(y: Int, x: Int, yBy: Int, xBy: Int): List[(Int, Int)] = {
        if (y < 0 || y >= map.length || x < 0 || x >= map.head.length) Nil
        else {
          val (nextY, nextX) = move(y, x, yBy, xBy)
          (y, x) :: generate(nextY, nextX, yBy, xBy)
        }
      }

      if ((leftX, topY) == (x1, y1) || (leftX, topY) == (x2, y2))
        generate(topY, leftX, -1, -1) ::: generate(bottomY, rightX, 1, 1)
      else generate(topY, rightX, -1, 1) ::: generate(bottomY, leftX, 1, -1)
    }

    antennas
      .groupBy { case (i, j) => map(i)(j) }
      .values
      .flatMap(_.combinations(2))
      .flatMap { case IndexedSeq((y1, x1), (y2, x2)) => getAntinodes(y1, x1, y2, x2) }
      .toSet
      .size
  }

  "Day #8 Challenge #1" should {
    "work as expected #1" in {
      countAntinodes(
        List(
          "............",
          "........0...",
          ".....0......",
          ".......0....",
          "....0.......",
          "......A.....",
          "............",
          "............",
          "........A...",
          ".........A..",
          "............",
          "............"
        )
      ) shouldBe 34
    }

    "work as expected #2" in {
      val input = Utils.readInputFile(8)
      countAntinodes(input) shouldBe 1045
    }
  }
}
