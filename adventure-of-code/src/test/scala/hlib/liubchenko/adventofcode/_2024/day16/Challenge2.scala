package hlib.liubchenko.adventofcode._2024.day16

import hlib.liubchenko.adventofcode._2024.Utils
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.mutable.ArrayBuffer

class Challenge2 extends AnyWordSpec with Matchers {
  def findPath(lines: List[String], shortestDistance: Int): Int = {
    implicit val size: Size = Size(lines.head.length, lines.length)
    val map = Array.fill(size.h)(Array.fill(size.w)(Int.MaxValue))

    val wall = -1
    var (endY, endX) = (0, 0)
    var (headX, headY) = (0, 0)
    val direction = '>'

    for {
      i <- 0 until size.h
      j <- 0 until size.w
    } yield lines(i)(j) match {
      case '#' => map(i)(j) = wall
      case 'S' => headY = i; headX = j
      case 'E' => endY = i; endX = j; map(i)(j) = 0
      case '.' => ()
    }

    def printMap(): Unit = {
      val str = map
        .map(_.map {
          case Int.MaxValue => "."
          case `wall`       => "#"
          case v            => v.toString
        }.map(_.padTo(8, ' ')).mkString)
        .mkString("", "\n", "\n")
      println(str)
    }

    def find(startY: Int, startX: Int, direction: Char, endY: Int, endX: Int): Int = {
      val stack = collection.mutable.Stack((startX, startY, direction))

      while (stack.nonEmpty) {
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

    // Find shortest distance from end to each other vertex
    find(endY, endX, 'v', headY, headX)
    printMap()

    val allPaths = ArrayBuffer[List[(Int, Int)]]()

    def dfs(y: Int, x: Int, d: Char, length: Int, path: Set[(Int, Int)]): Unit =
      if (map(y)(x) + length <= shortestDistance) {
        if (y == endY && x == endX) {
          if (length == shortestDistance) allPaths.append(path.toList)
        } else
          List((1, 0, 'v'), (0, 1, '>'), (-1, 0, '^'), (0, -1, '<'))
            .map { case (yM, xM, dM) => (y + yM, x + xM, dM, if (dM == d) 0 else 1000) }
            .filter { case (yN, xN, _, _) =>
              yN >= 0 && yN < size.h && xN >= 0 && xN < size.w &&
                map(yN)(xN) != wall &&
                !path.contains(yN -> xN)
            }
            .foreach { case (yN, xN, dM, rotationPrice) =>
              dfs(yN, xN, dM, length + 1 + rotationPrice, path ++ Set(yN -> xN))
            }
      }

    dfs(headY, headX, direction, 0, Set(headY -> headX))
    allPaths.flatten.distinct.length
  }

  case class Size(w: Int, h: Int)
  def inBorder(y: Int, x: Int)(implicit size: Size): Boolean = x >= 0 && x < size.w && y >= 0 && y < size.h

  "Day #16 Challenge #2" should {
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
      findPath(input, 7036) shouldBe 45
    }

    "work as expected #2" in {
      val input = List(
        "#################",
        "#...#...#...#..E#",
        "#.#.#.#.#.#.#.#.#",
        "#.#.#.#...#...#.#",
        "#.#.#.#.###.#.#.#",
        "#...#.#.#.....#.#",
        "#.#.#.#.#.#####.#",
        "#.#...#.#.#.....#",
        "#.#.#####.#.###.#",
        "#.#.#.......#...#",
        "#.#.###.#####.###",
        "#.#.#...#.....#.#",
        "#.#.#.#####.###.#",
        "#.#.#.........#.#",
        "#.#.#.#########.#",
        "#S#.............#",
        "#################"
      )
      findPath(input, 11048) shouldBe 64
    }

    "work as expected #3" in {
      val input = Utils.readInputFile(16)
      findPath(input, 147628) shouldBe 670
    }
  }
}
