package hlib.liubchenko.leetcode75.graph_bfs

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _1_nearest_exit_from_entrance_in_maze extends AnyWordSpec with Matchers {
  sealed trait Solution {
    def nearestExit(maze: Array[Array[Char]], entrance: Array[Int]): Int
  }

  // Mine, correct but memory limit
  object Solution1 extends Solution {
    def nearestExit(maze: Array[Array[Char]], entrance: Array[Int]): Int = {
      val (w, h) = (maze.length, maze(0).length)

      def availableMoves(visitedCels: Set[(Int, Int)], x: Int, y: Int): List[(Int, Int)] = {
        def checkCell(x: Int, y: Int) =
          Option.when(x >= 0 && y >= 0 && x < w && y < h && maze(x)(y) == '.' && !visitedCels.contains(x -> y))(x -> y)
        List((x + 1) -> y, (x - 1) -> y, x -> (y + 1), x -> (y - 1)).flatMap { case (x, y) => checkCell(x, y) }
      }

      def loop(visitedCels: Set[(Int, Int)], acc: Int, currX: Int, currY: Int): Int =
        if ((currX == 0 || currX == (w - 1) || currY == 0 || currY == (h - 1)) && acc != 0) acc
        else
          availableMoves(visitedCels, currX, currY)
            .map { case (nextX, nextY) => loop(visitedCels + (currX -> currY), acc + 1, nextX, nextY) }
            .minOption
            .getOrElse(Int.MaxValue)

      val res = loop(Set.empty, 0, entrance(0), entrance(1))
      if (res == Int.MaxValue) -1 else res

      //      def loop(visitedCels: Set[(Int, Int)], acc: Int, currX: Int, currY: Int): Int = {
      //        println(s"loop(visitedCels: $visitedCels, acc: $acc, currX: $currX, currY: $currY)")
      //        if ((currX == 0 || currX == (w - 1) || currY == 0 || currY == (h - 1)) && acc != 0) {
      //          println("exit found")
      //          acc
      //        } else {
      //          val moves = availableMoves(visitedCels, currX, currY)
      //          println(s"moves: ${moves.mkString(", ")}")
      //          moves
      //            .map { case (nextX, nextY) => loop(visitedCels + (currX -> currY), acc + 1, nextX, nextY) }
      //            .minOption
      //            .getOrElse(Int.MaxValue)
      //        }
      //      }
    }
  }

  object Solution2 extends Solution {

    def nearestExit(maze: Array[Array[Char]], entrance: Array[Int]): Int = {
      import scala.collection.mutable

      val (w, h, entX, entY) = (maze.length, maze(0).length, entrance(0), entrance(1))
      val visitedCels = mutable.Set.empty[(Int, Int)]

      def checkCell(x: Int, y: Int) =
        Option.when(x >= 0 && y >= 0 && x < w && y < h && maze(x)(y) == '.' && !visitedCels.contains(x -> y))(x -> y)
      def availableMoves(x: Int, y: Int): List[(Int, Int)] =
        List((x + 1) -> y, (x - 1) -> y, x -> (y + 1), x -> (y - 1)).flatMap { case (x, y) => checkCell(x, y) }

      def loop(acc: Int, currX: Int, currY: Int): Int =
        if ((currX == 0 || currX == (w - 1) || currY == 0 || currY == (h - 1)) && !(currX == entX && currY == entY))
          acc
        else
          availableMoves(currX, currY)
            .map { case pair @ (nextX, nextY) =>
              visitedCels += pair
              val res = loop(acc + 1, nextX, nextY)
              visitedCels -= pair
              res
            }
            .minOption
            .getOrElse(Int.MaxValue)

//      def loop(acc: Int, currX: Int, currY: Int): Int = {
//        println(s"loop(visitedCels: (${visitedCels.mkString(", ")}), acc: $acc, currX: $currX, currY: $currY)")
//        if ((currX == 0 || currX == (w - 1) || currY == 0 || currY == (h - 1)) && !(currX == entX && currY == entY)) {
//          println("exit found")
//          acc
//        } else {
//          val moves = availableMoves(currX, currY)
//          println(s"moves: ${moves.mkString(", ")}")
//          moves
//            .map { case pair @ (nextX, nextY) =>
//              visitedCels += pair
//              val res = loop(acc + 1, nextX, nextY)
//              visitedCels -= pair
//              res
//            }
//            .minOption
//            .getOrElse(Int.MaxValue)
//        }
//      }

      val res = loop(0, entrance(0), entrance(1))
      if (res == Int.MaxValue) -1 else res
    }
  }

  // Not my idea but my implementation, Runtime 88, Memory 100
  object Solution3 extends Solution {
    import scala.annotation.tailrec

    def nearestExit(maze: Array[Array[Char]], entrance: Array[Int]): Int = {
      val (w, h, entX, entY) = (maze.length, maze(0).length, entrance(0), entrance(1))
      maze(entX)(entY) = '+'

      def show(x: Int, y: Int): Unit = println(
        maze.zipWithIndex
          .map { case (row, rI) =>
            row.zipWithIndex.map { case (col, cI) => if (rI == x && cI == y) 'o' else col }.mkString
          }
          .mkString("\n") + "\n"
      )

      def checkCell(x: Int, y: Int) = x >= 0 && y >= 0 && x < w && y < h && maze(x)(y) == '.'
      def availableMoves(x: Int, y: Int): List[(Int, Int)] =
        List((x + 1) -> y, (x - 1) -> y, x -> (y + 1), x -> (y - 1)).collect {
          case pair @ (nX, nY) if checkCell(nX, nY) =>
            maze(nX)(nY) = '+'; show(x, y); pair
        }
      def isExit(x: Int, y: Int) = x == 0 || x == (w - 1) || y == 0 || y == (h - 1)

      @tailrec
      def loop(moves: List[(Int, Int)], pathLength: Int): Int =
        if (moves.isEmpty) -1
        else if (moves.exists { case (x, y) => isExit(x, y) }) pathLength
        else loop(moves.flatMap { case (x, y) => availableMoves(x, y) }, pathLength + 1)

      loop(availableMoves(entX, entY), 1)
    }
  }

  "All solutions" should {
    "work as expected" in {
      List(
//        Solution1,
//        Solution2
        Solution3
      ).map { solution =>
//        println(solution)
//        val m1 = Array(
//          Array('+', '+', '.', '+'),
//          Array('.', '.', '.', '+'),
//          Array('+', '+', '+', '.')
//        )
//        solution.nearestExit(m1, Array(1, 2)) shouldBe 1
//        val m2 = Array(
//          Array('+', '+', '+'),
//          Array('.', '.', '.'),
//          Array('+', '+', '+')
//        )
//        solution.nearestExit(m2, Array(1, 0)) shouldBe 2
//        val m3 = Array(Array('.', '+'))
//        solution.nearestExit(m3, Array(0, 0)) shouldBe -1
//        val m4 = Array(
//          Array('+', '.', '+', '+', '+', '+', '+'),
//          Array('+', '.', '+', '.', '.', '.', '+'),
//          Array('+', '.', '+', '.', '+', '.', '+'),
//          Array('+', '.', '.', '.', '+', '.', '+'),
//          Array('+', '+', '+', '+', '+', '.', '+')
//        )
//        solution.nearestExit(m4, Array(0, 1)) shouldBe 12
//        val m5 = Array(
//          "+.+..+..+...++....+.",
//          ".++.+...+++.+..+.++.",
//          "+....+........+..++.",
//          "...++...+.+..+..+...",
//          "+.....+..+..++.++...",
//          ".+....+.+......+.+++",
//          "...+..+.++.+.....+..",
//          ".....+++...+.++++...",
//          "..+..+.+..+.......+.",
//          "......+.+...+.+...+.",
//          ".+.+..+.+..+.+.....+",
//          "....................",
//          "++++..+...+..+++....",
//          "..++.+.....++..+....",
//          "...+.......+....+...",
//          ".++..+....++.++...+.",
//          "+.....+....+........",
//          "....+...+..+........"
//        ).map(_.toArray)
//        solution.nearestExit(m5, Array(17, 15)) shouldBe 1
        val m6 = Array(
          "...........",
          "...........",
          "...........",
          "...........",
          "...........",
          "...........",
          "...........",
          "...........",
          "...........",
          "...........",
          "..........."
        ).map(_.toArray)
        solution.nearestExit(m6, Array(5, 5)) shouldBe 5
      }
    }
  }
}
