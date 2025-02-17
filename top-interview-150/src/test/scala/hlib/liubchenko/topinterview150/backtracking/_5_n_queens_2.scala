package hlib.liubchenko.topinterview150.backtracking

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _5_n_queens_2 extends AnyWordSpec with Matchers {
  def totalNQueens(n: Int): Int = {
    val board = Array.fill(n, n)(0)

    def updateCollect(x: Int, y: Int) =
      if (x >= 0 && x < n && y >= 0 && y < n && board(x)(y) == 0) {
        board(x)(y) = 1; List((x, y))
      }
      else Nil

    def mark(i: Int, j: Int): Seq[(Int, Int)] = (0 until n).flatMap { k =>
      updateCollect(i, k) ++
        updateCollect(k, j) ++
        updateCollect(i + k, j + k) ++
        updateCollect(i + k, j - k) ++
        updateCollect(i - k, j - k) ++
        updateCollect(i - k, j + k)
    }

    def revert(points: Seq[(Int, Int)]): Unit = points.foreach { case (i, j) => board(i)(j) = 0 }

    // @formatter:off
    def loop(i: Int): Int = if (i == n) 1 else (0 until n).collect { case j if board(i)(j) == 0 =>
      val affected = mark(i, j)
      val res = loop(i + 1)
      revert(affected)
      res
    }.sum
    // @formatter:on

    loop(0)
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
