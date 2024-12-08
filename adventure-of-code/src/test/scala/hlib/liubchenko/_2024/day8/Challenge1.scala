package hlib.liubchenko._2024.day8

import hlib.liubchenko._2024.Utils
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Challenge1 extends AnyWordSpec with Matchers {
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

      val possibleAntinodes =
        if ((leftX, topY) == (x1, y1) || (leftX, topY) == (x2, y2))
          List((topY - yDiff, leftX - xDiff), (bottomY + yDiff, rightX + xDiff))
        else List((topY - yDiff, rightX + xDiff), (bottomY + yDiff, leftX - xDiff))

      possibleAntinodes.filter { case (y, x) => y >= 0 && y < map.length && x >= 0 && x < map.head.length }
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
      ) shouldBe 14
    }

    "work as expected #2" in {
      val input = Utils.readInputFile(8)
      countAntinodes(input) shouldBe 303
    }
  }
}
