package hlib.liubchenko.topinterview150.backtracking

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec


class _5_n_queens_2 extends AnyWordSpec with Matchers {
  import scala.collection.mutable

  def totalNQueens(n: Int): Int = {
    var res = 0
    val col = mutable.Set.empty[Int]
    val positiveDiagonal = mutable.Set.empty[Int]
    val negativeDiagonal = mutable.Set.empty[Int]

    def check(i: Int, j: Int): Boolean =
      !col.contains(j) && !positiveDiagonal.contains(i + j) && !negativeDiagonal.contains(i - j)

    def mark(i: Int, j: Int): Unit = {
      col.add(j)
      positiveDiagonal.add(i + j)
      negativeDiagonal.add(i - j)
    }

    def revert(i: Int, j: Int): Unit = {
      col.remove(j)
      positiveDiagonal.remove(i + j)
      negativeDiagonal.remove(i - j)
    }

    def loop(i: Int): Unit = if (i == n) res += 1
    else
      (0 until n).filter(check(i, _)).foreach { j =>
        mark(i, j)
        loop(i + 1)
        revert(i, j)
      }

    loop(0)
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
