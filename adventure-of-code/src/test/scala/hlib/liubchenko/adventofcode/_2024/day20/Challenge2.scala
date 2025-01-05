package hlib.liubchenko.adventofcode._2024.day20

import hlib.liubchenko.adventofcode._2024.Utils
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Challenge2 extends AnyWordSpec with Matchers {
  def findPath(lines: List[String], allowedCheat: Int, betterThen: Int): Int = {
    implicit val size: Size = Size(lines.head.length, lines.length)
    val map = Array.fill(size.h)(Array.fill(size.w)(Int.MaxValue))

    val wall = -1
    var (endY, endX) = (0, 0)
    var cheatsCount = 0
    var (headX, headY) = (0, 0)

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
      val stack = collection.mutable.Stack((headX, headY))

      while (stack.nonEmpty) {
        val (x, y) = stack.pop()

        def update(xM: Int, yM: Int): Unit = {
          val inBorder = x + xM >= 0 && x + xM < size.w && y + yM >= 0 && y + yM < size.h
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

    find()
    printMap()

    while (!(headX == endX && headY == endY)) {
      val currDist = map(headY)(headX)
      cheatsCount += iterateDiamond(allowedCheat, headY, headX).count { case Point(i, j, dist) =>
        map(i)(j) != wall && currDist < map(i)(j) && map(i)(j) - (currDist + dist) >= betterThen
      }
      val (xMove, yMove) = List((0, 1), (1, 0), (-1, 0), (0, -1)).find { case (x, y) =>
        inBorder(headY + y, headX + x) && map(headY + y)(headX + x) == currDist + 1
      }.get
      headX = headX + xMove
      headY = headY + yMove
    }

    cheatsCount
  }

  case class Point(y: Int, x: Int, dist: Int)
  // . . . . . 5
  // . . . . . 4 5
  // . . . . . 3 4 5
  // . . . . . 2 3 4 5
  // . . . . . 1 2 3 4 5
  // 5 4 3 2 1 0 1 2 3 4 5
  // . . . . . 1 . . . . .
  // . . . . . 2 . . . . .
  // . . . . . 3 . . . . .
  // . . . . . 4 . . . . .
  // . . . . . 5 . . . . .
  def iterateDiamond(n: Int, y: Int, x: Int)(implicit size: Size): Seq[Point] = for {
    i <- -n to n
    k = if (i < 0) n + i else n - i
    j <- -k to k
    if inBorder(y + i, x + j) && !(i == 0 && j == 0)
  } yield Point(y + i, x + j, math.abs(j) + math.abs(i))

  case class Size(w: Int, h: Int)
  def inBorder(y: Int, x: Int)(implicit size: Size): Boolean = x >= 0 && x < size.w && y >= 0 && y < size.h

  "Day #20 Challenge #2" should {
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
      findPath(input, 2, 10) shouldBe 10
      findPath(input, 6, 70) shouldBe 16
    }

    "work as expected #2" in {
      val input = Utils.readInputFile(20)
      findPath(input, 2, 100) shouldBe 1497
      findPath(input, 20, 100) shouldBe 1030809
    }
  }

  "iterateDiamond" should {
    "work as expected" in {
      // #	#	  #	    #	  #     #     #
      // #	#	  #	    (1,3) #	    #     #
      // #	#	  (2,2) (2,3) (2,4) #     #
      // #	(3,1) (3,2) .     (3,4) (3,5) #
      // #	#	  (4,2) (4,3) (4,4) #     #
      // #	#	  #	    (5,3) #	    #     #
      // #	#	  #	    #	  #	    #     #
      iterateDiamond(2, 3, 3)(Size(7, 7)) shouldBe List(
        Point(1, 3, 2),
        Point(2, 2, 2),
        Point(2, 3, 1),
        Point(2, 4, 2),
        Point(3, 1, 2),
        Point(3, 2, 1),
        Point(3, 4, 1),
        Point(3, 5, 2),
        Point(4, 2, 2),
        Point(4, 3, 1),
        Point(4, 4, 2),
        Point(5, 3, 2)
      )
    }
  }
}
