package hlib.liubchenko.adventofcode._2024.day12

import hlib.liubchenko.adventofcode._2024.Utils
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.mutable

class Challenge2 extends AnyWordSpec with Matchers {

  def fencePrice(lines: List[String]): Int = {
    val map = lines.map(_.toArray).toArray
    val (w, h) = (map.head.length, map.length)
    val moves = List((1, 0), (0, 1), (-1, 0), (0, -1))

    case class Point(y: Int, x: Int) {
      def inBorder: Boolean = y >= 0 && y < h && x >= 0 && x < w
      def move(shift: (Int, Int)): Point = Point(y + shift._1, x + shift._2)
      def compareValue(that: Point): Boolean = map(y)(x) == map(that.y)(that.x)
    }

    val visited = mutable.Set.empty[Point]

    def bfs(point: Point): List[Point] = if (visited.add(point))
      point :: moves
        .map(point.move)
        .filter(_.inBorder)
        .filter(_.compareValue(point))
        .flatMap(bfs)
    else Nil

    val clusters = for {
      y <- 0 until h
      x <- 0 until w
      p = Point(y, x)
      if !visited.contains(p)
    } yield bfs(p)

    def countSides(cluster: Set[Point]): Int = {
      def countSequentialElements(elements: Set[Int]) =
        elements.toList.sorted
          .sliding(2)
          .map {
            case a :: b :: Nil => if (a + 1 == b) 0 else 1
            case a :: Nil      => 0
          }
          .sum + 1

      val horizontalBorders = cluster
        .flatMap { case Point(y, x) =>
          val above = if (cluster.contains(Point(y - 1, x))) Nil else List('^' -> Point(y, x))
          val below = if (cluster.contains(Point(y + 1, x))) Nil else List('v' -> Point(y, x))
          above ++ below
        }
        .groupMap(t => t._1 -> t._2.y)(_._2.x)

      val verticalBorders = cluster
        .flatMap { case Point(y, x) =>
          val left = if (cluster.contains(Point(y, x - 1))) Nil else List('<' -> Point(y, x))
          val right = if (cluster.contains(Point(y, x + 1))) Nil else List('>' -> Point(y, x))
          left ++ right
        }
        .groupMap(t => t._1 -> t._2.x)(_._2.y)

      val res = (horizontalBorders.values ++ verticalBorders.values)
        .map(countSequentialElements)
        .sum

      res
    }

    clusters
      .map(_.toSet)
      .map { cluster =>
        val sides = countSides(cluster)
        sides * cluster.size
      }
      .sum
  }

  "Day #12 Challenge #2" should {
    "work as expected #1" in {
      val input = List(
        "AAAA",
        "BBCD",
        "BBCC",
        "EEEC"
      )
      fencePrice(input) shouldBe 80
    }

    "work as expected #2" in {
      val input = List(
        "EEEEE",
        "EXXXX",
        "EEEEE",
        "EXXXX",
        "EEEEE"
      )
      fencePrice(input) shouldBe 236
    }

    "work as expected #3" in {
      val input = List(
        "RRRRIICCFF",
        "RRRRIICCCF",
        "VVRRRCCFFF",
        "VVRCCCJFFF",
        "VVVVCJJCFE",
        "VVIVCCJJEE",
        "VVIIICJJEE",
        "MIIIIIJJEE",
        "MIIISIJEEE",
        "MMMISSJEEE"
      )
      fencePrice(input) shouldBe 1206
    }

    "work as expected #4" in {
      val input = Utils.readInputFile(12)
      fencePrice(input) shouldBe 946084
    }
  }
}
