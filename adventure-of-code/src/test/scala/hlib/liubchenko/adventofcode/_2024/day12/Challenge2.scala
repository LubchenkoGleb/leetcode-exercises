//package hlib.liubchenko.adventofcode._2024.day12
//
//import hlib.liubchenko.adventofcode._2024.Utils
//import org.scalatest.matchers.should.Matchers
//import org.scalatest.wordspec.AnyWordSpec
//
//import scala.collection.mutable
//
//class Challenge2 extends AnyWordSpec with Matchers {
//
//  def fencePrice(lines: List[String]): Int = {
//    val map = lines.map(_.toArray).toArray
//    val (w, h) = (map.head.length, map.length)
//    val moves = List((1, 0), (0, 1), (-1, 0), (0, -1))
//
//    case class Point(y: Int, x: Int) {
//      def inBorder: Boolean = y >= 0 && y < h && x >= 0 && x < w
//      def move(shift: (Int, Int)): Point = Point(y + shift._1, x + shift._2)
//      def compareValue(that: Point): Boolean = map(y)(x) == map(that.y)(that.x)
//    }
//
//    val visited = mutable.Set.empty[Point]
//
//    def bfs(point: Point): List[Point] = if (visited.add(point))
//      point :: moves
//        .map(point.move)
//        .filter(_.inBorder)
//        .filter(_.compareValue(point))
//        .flatMap(bfs)
//    else Nil
//
//    val clusters = for {
//      y <- 0 until h
//      x <- 0 until w
//      p = Point(y, x)
//      if !visited.contains(p)
//    } yield bfs(p)
//
//    def loop(head: Point, direction: Char, cluster: Set[Point], visited: Set[(Char, Point)], acc: Int): Int = {
//      println(s"point: $head, direction: $direction, acc: $acc, ")
//      if (visited.contains(direction -> head)) acc
//      else {
//        val sameSideShift = direction match {
//          case '>' => 0 -> 1
//          case 'v' => 1 -> 0
//          case '<' => 0 -> -1
//          case '^' => -1 -> 0
//        }
//        val sameSideMove = head.move(sameSideShift)
//
////        def turnMoveShift = direction match {
////          case '>' => 1 -> 0
////          case 'v' => 0 -> -1
////          case '<' => -1 -> 0
////          case '^' => 0 -> 1
////        }
////        def turnMove = head.move(turnMoveShift)
//        def nextDirection = {
//          val posibleMoves = direction match {
//            case '>' | '<' => List(((1, 0), 'v'), ((-1, 0), '^'))
//            case 'v' | '^' => List(((0, 1), '>'), ((0, -1), '<'))
//          }
//
//          posibleMoves.collectFirst { case (m, d) if cluster.contains(head.move(m)) => d }
//        }.getOrElse { cu}
//
//        val isSameMove = cluster.contains(sameSideMove)
//
//        if (isSameMove) loop(sameSideMove, direction, cluster, visited + (direction -> head), acc)
//        else {
//          val nextDiractionMaybe = nextDirection
//          loop(head, nextDiractionMaybe.get, cluster, visited + (direction -> head), acc + 1)
//        }
//      }
//    }
//
//    clusters
//      .map(_.toSet)
//      .map { cluster =>
////        val (ySides, xSides) = cluster.foldLeft((Set.empty[(Int, Char)], Set.empty[(Int, Char)])) {
////          case ((yAcc, xAcc), point) =>
////            def hasBorder(yM: Int, xM: Int) = {
////              val moved = point.move((yM, xM))
////              moved.inBorder && cluster.contains(moved)
////            }
////
////            val hasTop = hasBorder(-1, 0)
////            val hasBottom = hasBorder(1, 0)
////            val hasLeft = hasBorder(0, -1)
////            val hasRight = hasBorder(0, 1)
////
////            val topUpdate = if (!hasTop) Some((point.y, '^')) else None
////            val bottomUpdate = if (!hasBottom) Some((point.y, 'v')) else None
////            val leftUpdate = if (!hasLeft) Some((point.x, '<')) else None
////            val rightUpdate = if (!hasRight) Some((point.x, '>')) else None
////
////            (yAcc ++ topUpdate ++ bottomUpdate, xAcc ++ leftUpdate ++ rightUpdate)
////        }
////        val sides = ySides.size + xSides.size
//
//        val startPoint = cluster.minBy(p => (p.y, p.x))
//        val border = cluster.filterNot(p => moves.map(p.move).forall(cluster.contains))
//        val sides = loop(startPoint, '>', border, Set.empty, 0)
//        println("")
//
//        sides * cluster.size
//      }
//      .sum
//  }
//
//  "Day #12 Challenge #2" should {
//    "work as expected #1" in {
//      val input = List(
//        "AAAA",
//        "BBCD",
//        "BBCC",
//        "EEEC"
//      )
//      fencePrice(input) shouldBe 80
//    }
//
//    "work as expected #2" in {
//      val input = List(
//        "EEEEE",
//        "EXXXX",
//        "EEEEE",
//        "EXXXX",
//        "EEEEE"
//      )
//      fencePrice(input) shouldBe 436
//    }
//
//    "work as expected #3" in {
//      val input = List(
//        "RRRRIICCFF",
//        "RRRRIICCCF",
//        "VVRRRCCFFF",
//        "VVRCCCJFFF",
//        "VVVVCJJCFE",
//        "VVIVCCJJEE",
//        "VVIIICJJEE",
//        "MIIIIIJJEE",
//        "MIIISIJEEE",
//        "MMMISSJEEE"
//      )
//      fencePrice(input) shouldBe 1081
//    }
//
//    "work as expected #4" in {
//      val input = Utils.readInputFile(12)
//      fencePrice(input) shouldBe 659354
//    }
//  }
//}
