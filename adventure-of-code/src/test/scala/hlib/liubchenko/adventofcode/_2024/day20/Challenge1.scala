package hlib.liubchenko.adventofcode._2024.day20

import hlib.liubchenko.adventofcode._2024.Utils
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Challenge1 extends AnyWordSpec with Matchers {
  def findPath(lines: List[String], betterThen: Int): Int = {
    val (height, width) = (lines.length, lines.head.length)
    val map = Array.fill(height)(Array.fill(width)(Int.MaxValue))

    val wall = -1
    var (startY, startX) = (0, 0)
    var (endY, endX) = (0, 0)

    for {
      i <- (0 until height)
      j <- (0 until width)
    } yield lines(i)(j) match {
      case '#' => map(i)(j) = wall
      case 'S' => startY = i; startX = j; map(i)(j) = 0
      case 'E' => endY = i; endX = j
      case '.' => ()
    }

    def printMap(): Unit = {
      val str = map
        .map(_.map {
          case Int.MaxValue => "."
          case `wall`       => "#"
          case v            => v.toString
        }.mkString("\t"))
        .mkString("", "\n", "\n")
      println(str)
    }

    def find(): Int = {
      val stack = collection.mutable.Stack((startX, startY))

      while (stack.nonEmpty) {
        val (x, y) = stack.pop()

        //        printMap()

        def update(xM: Int, yM: Int): Unit = {
          val inBorder = x + xM >= 0 && x + xM < width && y + yM >= 0 && y + yM < height
          lazy val element = map(y + yM)(x + xM)
          if (inBorder && element != wall) {
            map(y + yM)(x + xM) = math.min(map(y)(x) + 1, element)
            if (map(y + yM)(x + xM) != element) stack.push((x + xM, y + yM))
          }
        }

        List((1, 0), (0, 1), (-1, 0), (0, -1)).foreach { case (x, y) => update(x, y) }
      }

      map(endY)(endX)
    }

    def reset(): Unit = for {
      i <- (0 until height)
      j <- (0 until width)
    } {
      if (map(i)(j) != wall) map(i)(j) = Int.MaxValue
      map(startY)(startX) = 0
    }

    val originalTime = find()
    var cheatsCount = 0

    for {
      i <- (1 until height - 1)
      j <- (1 until width - 1)
      if map(i)(j) == wall
    } {
      reset()
      map(i)(j) = Int.MaxValue
      val res = find()
      // println(s"($i, $j): $res")
      if (originalTime - res >= betterThen) {
        printMap()
        cheatsCount += 1
      }
      map(i)(j) = wall
    }

    cheatsCount
  }

  "Day #20 Challenge #1" should {
    "work as expected #1" in {
      val input = List(
        "###############",
        "#...#...#.....#",
        "#.#.#.#.#.###.#",
        "#S#...#.#.#...#",
        "#######.#.#.###",
        "#######.#.#...#",
        "#######.#.###.#",
        "###..E#...#...#",
        "###.#######.###",
        "#...###...#...#",
        "#.#####.#.###.#",
        "#.#...#.#.#...#",
        "#.#.#.#.#.#.###",
        "#...#...#...###",
        "###############"
      )
      findPath(input, 10) shouldBe 10
    }

    "work as expected #2" in {
      val input = Utils.readInputFile(20)
      findPath(input, 100) shouldBe 1497
    }
  }
}
