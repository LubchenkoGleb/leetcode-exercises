package hlib.liubchenko.adventofcode._2024.day16

import hlib.liubchenko.adventofcode._2024.Utils
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Challenge1 extends AnyWordSpec with Matchers {
  def findPath(lines: List[String]): Int = {
    implicit val size: Size = Size(lines.head.length, lines.length)
    val map = Array.fill(size.h)(Array.fill(size.w)(Int.MaxValue))

    val wall = -1
    var (endY, endX) = (0, 0)
    var (headX, headY, direction) = (0, 0, '>')

    for {
      i <- 0 until size.h
      j <- 0 until size.w
    } yield lines(i)(j) match {
      case '#' => map(i)(j) = wall
      case 'S' => headY = i; headX = j; map(i)(j) = 0
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
      val stack = collection.mutable.Stack((headX, headY, direction))

      while (stack.nonEmpty) {
        // printMap()

        val (x, y, direction) = stack.pop()

        def update(xM: Int, yM: Int, directionM: Char): Unit = {
          val inBorder = x + xM >= 0 && x + xM < size.w && y + yM >= 0 && y + yM < size.h
          lazy val element = map(y + yM)(x + xM)
          if (inBorder && element != wall) {
            val rotationPrice = if (directionM == direction) 0 else 1000
            map(y + yM)(x + xM) = math.min(map(y)(x) + 1 + rotationPrice, element)
            if (map(y + yM)(x + xM) != element) stack.push((x + xM, y + yM, directionM))
          }
        }

        List((1, 0, '>'), (0, 1, 'v'), (-1, 0, '<'), (0, -1, '^')).foreach { case (x, y, d) => update(x, y, d) }
      }

      map(endY)(endX)
    }

    find()
  }

  case class Size(w: Int, h: Int)
  def inBorder(y: Int, x: Int)(implicit size: Size): Boolean = x >= 0 && x < size.w && y >= 0 && y < size.h

  "Day #16 Challenge #1" should {
    "work as expected #1" in {
      val input = List(
        "###############",
        "#.......#....E#",
        "#.#.###.#.###.#",
        "#.....#.#...#.#",
        "#.###.#####.#.#",
        "#.#.#.......#.#",
        "#.#.#####.###.#",
        "#...........#.#",
        "###.#.#####.#.#",
        "#...#.....#.#.#",
        "#.#.#.###.#.#.#",
        "#.....#...#.#.#",
        "#.###.#.#.#.#.#",
        "#S..#.....#...#",
        "###############"
      )
      findPath(input) shouldBe 7036
    }

    "work as expected #2" in {
      val input = Utils.readInputFile(16)
      findPath(input) shouldBe 147628
    }
  }
}
