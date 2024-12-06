package hlib.liubchenko.topinterview150.graph.general

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _1_number_of_islands extends AnyWordSpec with Matchers {
  def numIslands(grid: Array[Array[Char]]): Int = {
      def erase(i: Int, j: Int): Unit = if (i >= 0 && i < grid.length && j >= 0 && j < grid.head.length && grid(i)(j) == '1') {
        grid(i)(j) = '0'
        erase(i + 1, j)
        erase(i - 1, j)
        erase(i, j + 1)
        erase(i, j - 1)
      }

      val islands = for {
        i <- grid.indices
        j <- grid.head.indices
        if grid(i)(j) == '1'
      } yield erase(i, j)

      islands.length
  }

  "numIslands" should {
    "work as expected" in {
      val grid = Array(
        Array('1', '1', '0', '0', '0'),
        Array('1', '1', '0', '0', '0'),
        Array('0', '0', '1', '0', '0'),
        Array('0', '0', '0', '1', '1')
      )
      numIslands(grid) shouldBe 3
    }
  }
}
