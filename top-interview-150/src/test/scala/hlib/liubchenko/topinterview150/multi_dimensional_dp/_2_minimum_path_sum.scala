package hlib.liubchenko.topinterview150.multi_dimensional_dp

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _2_minimum_path_sum extends AnyWordSpec with Matchers {
  def minPathSum(grid: Array[Array[Int]]): Int = {
    for {
      i <- grid.indices
      j <- grid.head.indices
    } {
      val rightVal = if (i == 0 && j == 0) 0 else if (j - 1 >= 0) grid(i)(j - 1) else Int.MaxValue
      val topVal = if (i == 0 && j == 0) 0 else if (i - 1 >= 0) grid(i - 1)(j) else Int.MaxValue
      grid(i)(j) = grid(i)(j) + math.min(rightVal, topVal)
    }

    grid.last.last
  }

  "minPathSum" should {
    "work as expected" in {
      var input = Array(
        Array(1, 3, 1),
        Array(1, 5, 1),
        Array(4, 2, 1)
      )
      minPathSum(input) shouldBe 7
    }
  }
}
