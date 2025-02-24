package hlib.liubchenko.topinterview150.binary_search

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _2_search_a_2d_matrix extends AnyWordSpec with Matchers {
  import scala.annotation.tailrec

  def searchMatrix(matrix: Array[Array[Int]], target: Int): Boolean = {
    val m = matrix.length
    val n = matrix.head.length

    @tailrec
    def searchRow(from: Int, to: Int): Int =
      if (from == to) from
      else {
        val half = from + math.max(1, (to - from) / 2)
        if (matrix(half)(0) <= target) searchRow(half, to)
        else searchRow(from, half - 1)
      }
    val row = searchRow(0, m - 1)

    @tailrec
    def checkRow(from: Int, to: Int): Boolean = if (from == to) matrix(row)(from) == target
    else {
      val half = from + ((to - from) / 2)
      if (matrix(row)(half) < target) checkRow(half + 1, to)
      else checkRow(from, half)
    }
    checkRow(0, n - 1)
  }

  "searchMatrix" should {
    "work as expected #1" in {
      val matrix = Array(
        Array(1, 3, 5, 7),
        Array(10, 11, 16, 20),
        Array(23, 30, 34, 60)
      )
      searchMatrix(matrix, 3) shouldBe true
    }

    "work as expected #2" in {
      val matrix = Array(
        Array(1, 3, 5, 7),
        Array(10, 11, 16, 20),
        Array(23, 30, 34, 60)
      )
      searchMatrix(matrix, 13) shouldBe false
    }

    "work as expected #3" in {
      val matrix = Array(
        Array(1),
        Array(3)
      )
      searchMatrix(matrix, 3) shouldBe true
    }

  }
}
