package hlib.liubchenko.topinterview150.backtracking

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _5_n_queens_2 extends AnyWordSpec with Matchers {
  def totalNQueens(n: Int): Int = {
    val board = Array.fill(n, n)(0)

    def mark(i: Int, j: Int): Seq[(Int, Int)] = {
      def updateCollect(x: Int, y: Int) =
        if (x >= 0 && x < n && y >= 0 && y < n && board(x)(y) == 0) { board(x)(y) = 1; List((x, y)) }
        else Nil
      val res = (0 until n).flatMap { k =>
        updateCollect(i, k) ++
          updateCollect(k, j) ++
          updateCollect(i + k, j + k) ++
          updateCollect(i + k, j - k) ++
          updateCollect(i - k, j - k) ++
          updateCollect(i - k, j + k)
      }
      board(i)(j) = 2
      res
    }

    def revert(points: Seq[(Int, Int)]): Unit = points.foreach { case (i, j) => board(i)(j) = 0 }

    def printBoard(msg: String): Unit = println(board.map(_.mkString).mkString(msg + "\n", "\n", "\n"))

    var iterationsCount = 0
    def loop(i: Int): Int = {
      iterationsCount += 1
      if (i == n) {
//        printBoard(s"$i")
        1
      } else
        (0 until n).collect {
          case j if board(i)(j) == 0 =>
            val affected = mark(i, j)
            //          printBoard(s"Before: ($i, $j)")
            val res = loop(i + 1)
            revert(affected)
            //          printBoard(s"After: ($i, $j)")
            res
        }.sum
    }

    val res = loop(0)
    println(iterationsCount)
    res
  }

  "totalNQueens" should {
    "work as expected" in {
      totalNQueens(1) shouldBe 1
      totalNQueens(4) shouldBe 2
      totalNQueens(5) shouldBe 10
      totalNQueens(6) shouldBe 4
      totalNQueens(7) shouldBe 40
      totalNQueens(8) shouldBe 92
      totalNQueens(9) shouldBe 352
    }
  }
}
