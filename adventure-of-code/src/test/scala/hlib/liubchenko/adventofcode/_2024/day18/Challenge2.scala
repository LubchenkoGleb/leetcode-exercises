package hlib.liubchenko.adventofcode._2024.day18

import hlib.liubchenko.adventofcode._2024.Utils
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Challenge2 extends AnyWordSpec with Matchers {
  def findCorruptedBit(coordinates: List[String], startFrom: Int, width: Int, height: Int): (Int, Int) = {
    val map = Array.fill(height)(Array.fill(width)(Int.MaxValue))
    val parsedCoordinates =
      coordinates
        .map { _.split(",") }
        .map { case Array(x, y) => (x.toInt, y.toInt) }
        .zipWithIndex
        .map { case ((x, y), i) => if (i < startFrom) map(y)(x) = -1; (x, y) }
        .drop(startFrom)

    def findPath(corruptedByte: (Int, Int)): Int = {
      for { i <- map.indices; j <- map.head.indices if map(i)(j) != -1 } yield map(i)(j) = Int.MaxValue
      map(corruptedByte._2)(corruptedByte._1) = -1
      map(0)(0) = 1

      val stack = collection.mutable.Stack((0, 0))

      while (stack.nonEmpty) {
        val (x, y) = stack.pop()
        def update(xM: Int, yM: Int): Unit = {
          val inBorder = x + xM >= 0 && x + xM < width && y + yM >= 0 && y + yM < height
          lazy val element = map(y + yM)(x + xM)
          if (inBorder && element != -1) {
            map(y + yM)(x + xM) = math.min(map(y)(x) + 1, element)
            if (map(y + yM)(x + xM) != element) stack.push((x + xM, y + yM))
          }
        }

        List((1, 0), (0, 1), (-1, 0), (0, -1)).foreach { case (x, y) => update(x, y) }
      }

      map(height - 1)(width - 1)
    }

    parsedCoordinates.find { findPath(_) == Int.MaxValue }.get
  }

  "Day #18 Challenge #2" should {
    "work as expected #1" in {
      val input = List(
        "5,4",
        "4,2",
        "4,5",
        "3,0",
        "2,1",
        "6,3",
        "2,4",
        "1,5",
        "0,6",
        "3,3",
        "2,6",
        "5,1",
        "1,2",
        "5,5",
        "2,5",
        "6,5",
        "1,4",
        "0,4",
        "6,4",
        "1,1",
        "6,1",
        "1,0",
        "0,5",
        "1,6",
        "2,0"
      )
      findCorruptedBit(input, 12, 7, 7) shouldBe (6, 1)
    }

    "work as expected #2" in {
      pending
      val input = Utils.readInputFile(18)
      findCorruptedBit(input, 454, 71, 71) shouldBe (8, 51)
    }
  }
}
