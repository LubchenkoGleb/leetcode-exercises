package hlib.liubchenko._2024.day6

import hlib.liubchenko._2024.Utils
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.annotation.tailrec

class Challenge2 extends AnyWordSpec with Matchers {
  def findRoute(lines: List[String]): Int = {
    val map = lines.map(_.toArray).toArray

    @tailrec
    def search(i: Int, j: Int): Option[(Int, Int)] =
      if (i >= map.length) None
      else if (j >= map(i).length) search(i + 1, 0)
      else if (map(i)(j) == '^') Some((i, j))
      else search(i, j + 1)

    val (initialI, initialJ) = search(0, 0).get
    val initialDirection = map(initialI)(initialJ)

    def checkLoop() = {
//      println("before:\n" + map.map(_.mkString).mkString("\n"))

      var (i, j) = (initialI, initialJ)
      var direction = initialDirection
      var (isExit, isLoop) = (false, false)
      updateCurrentPosition(i, j)

      def updateCurrentPosition(nextI: Int, nextJ: Int): Unit =
        if (map(nextI)(nextJ) != '#' && map(nextI)(nextJ) != '0') {
          map(i)(j) = {
            direction match {
              case '^' | 'v' =>
                map(i)(j) match {
                  case '-' => '+'
                  case '+' => isLoop = true; '+'
                  case _   => '|'
                }
              case '<' | '>' =>
                map(i)(j) match {
                  case '|' => '+'
                  case '+' => isLoop = true; '+'
                  case _   => '-'
                }
            }
          }
        }

      def move(nextI: Int, nextJ: Int): Unit = {
        updateCurrentPosition(nextI, nextJ)
        map(nextI)(nextJ) match {
          case '#' | '0' =>
            direction = direction match {
              case '^' => '>'
              case '>' => 'v'
              case '<' => '^'
              case 'v' => '<'
            }
          case _ =>
            i = nextI; j = nextJ
        }
      }

      while (!isExit && !isLoop) {
//        if(map(6)(3) == '0')
//          println(s"debug: map($i)($j)=${map(i)(j)}, direction: $direction\n" + map.map(_.mkString).mkString("\n") + s"\nloop: $isLoop\n")

        val (nextI, nextJ) = direction match {
          case '^' => (i - 1, j)
          case '>' => (i, j + 1)
          case '<' => (i, j - 1)
          case 'v' => (i + 1, j)
        }
        if (nextI < 0 || nextI > map.length - 1 || nextJ < 0 || nextJ > map(0).length - 1) isExit = true
        else move(nextI, nextJ)
      }

      if (isLoop) println("after:\n" + map.map(_.mkString).mkString("\n") + s"\nloop: $isLoop\n")
      resetMap()

      isLoop
    }

    def resetMap(): Unit = {
      for {
        i <- map.indices
        j <- map.head.indices
        if map(i)(j) != '#'
      } map(i)(j) = '.'
      map(initialI)(initialJ) = initialDirection
    }

    val res = for {
      i <- map.indices
      j <- map.head.indices
      if map(i)(j) == '.'
    } yield {
      map(i)(j) = '0'
      checkLoop()
    }

    res.count(identity)
  }

  "Day #6 Challenge #2 findRoute" should {
    "work as expected #1" in {
      findRoute(
        List(
          "....#.....",
          ".........#",
          "..........",
          "..#.......",
          ".......#..",
          "..........",
          ".#..^.....",
          "........#.",
          "#.........",
          "......#..."
        )
      ) shouldBe 41
    }

    "work as expected #2" in {
      val input = Utils.readInputFile(6)
      findRoute(input) shouldBe 0
    }
  }
}
