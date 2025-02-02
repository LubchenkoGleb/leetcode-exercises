package hlib.liubchenko.adventofcode._2024.day12

import hlib.liubchenko.adventofcode._2024.Utils
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.mutable

class Challenge1 extends AnyWordSpec with Matchers {

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

    clusters.map { cluster =>
      val perimeter = cluster.iterator.map { p => 4 - moves.map(p.move).count(cluster.contains) }.sum
      perimeter * cluster.size
    }.sum
  }

  "Day #12 Challenge #1" should {
    "work as expected #1" in {
      val input = List(
        "AAAA",
        "BBCD",
        "BBCC",
        "EEEC"
      )
      fencePrice(input) shouldBe 140
    }

    "work as expected #2" in {
      val input = List(
        "OOOOO",
        "OXOXO",
        "OOOOO",
        "OXOXO",
        "OOOOO"
      )
      fencePrice(input) shouldBe 772
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
      fencePrice(input) shouldBe 1930
    }

    "work as expected #4" in {
      val input = Utils.readInputFile(12)
      fencePrice(input) shouldBe 1550156
    }
  }
}
