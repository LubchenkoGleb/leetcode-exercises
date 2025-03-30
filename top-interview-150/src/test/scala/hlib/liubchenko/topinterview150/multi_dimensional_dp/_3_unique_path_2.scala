package hlib.liubchenko.topinterview150.multi_dimensional_dp

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _3_unique_path_2 extends AnyWordSpec with Matchers {
  def uniquePathsWithObstacles(obstacleGrid: Array[Array[Int]]): Int = {
    val (l, w) = (obstacleGrid.length, obstacleGrid.head.length)

    def loop(i: Int, j: Int): Int =
      if (i >= l || j >= w || obstacleGrid(i)(j) == 1) 0
      else if (i == l - 1 && j == w - 1) -1
      else if (obstacleGrid(i)(j) < 0) obstacleGrid(i)(j)
      else {
        val res = loop(i + 1, j) + loop(i, j + 1)
        obstacleGrid(i)(j) += res
        res
      }

    -loop(0, 0)
  }

  "uniquePathsWithObstacles" should {
    "work as expected #1" in {
      val input = Array(Array(0, 0, 0), Array(0, 1, 0), Array(0, 0, 0))
      uniquePathsWithObstacles(input) shouldBe 2
    }

    "work as expected #2" in {
      val input = Array(Array(0, 1), Array(0, 0))
      uniquePathsWithObstacles(input) shouldBe 1
    }

    "work as expected #3" in {
      val input = Array(
        Array(0, 0, 0, 0, 0),
        Array(0, 0, 0, 0, 1),
        Array(0, 0, 0, 1, 0),
        Array(0, 0, 0, 0, 0)
      )
      uniquePathsWithObstacles(input) shouldBe 10
    }
  }
}
