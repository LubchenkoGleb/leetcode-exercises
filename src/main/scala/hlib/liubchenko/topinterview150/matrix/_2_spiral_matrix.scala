package hlib.liubchenko.topinterview150.matrix

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _2_spiral_matrix extends AnyWordSpec with Matchers {
  def spiralOrder(matrix: Array[Array[Int]]): List[Int] = {
    def loop(i: Int, j: Int, topI: Int, rightJ: Int, bottomI: Int, leftJ: Int): List[Int] = {
      println(matrix(i)(j))

      val (nextI, nextJ) =
        if (i == topI && j == rightJ) (i + 1, j)
        else if (i == bottomI && j == rightJ) (i, j - 1)
        else if (i == bottomI && j == leftJ) (i - 1, j)
        else if (i == topI + 1 && j == leftJ) (i, j + 1)
        else if (i == topI) (i, j + 1)
        else if (j == rightJ) (i + 1, j)
        else if (i == bottomI) (i, j - 1)
        else (i - 1, j)

      val (newTopI, newRightJ, newBottomI, newLeftJ) =
        if (i == topI + 1 && j == leftJ) (topI + 1, rightJ - 1, bottomI - 1, leftJ + 1)
        else (topI, rightJ, bottomI, leftJ)

      val nextMoveAvailable: Boolean = i != topI || i != bottomI || j != leftJ || j != rightJ
      matrix(i)(j) :: (if (nextMoveAvailable) loop(nextI, nextJ, newTopI, newRightJ, newBottomI, newLeftJ) else Nil)
    }

    loop(0, 0, 0, matrix(0).length - 1, matrix.length - 1, 0)
  }

  "spiralOrder" should {
    "work as expected" in {
      var input = Array(
        Array(1, 2, 3),
        Array(4, 5, 6),
        Array(7, 8, 9)
      )
//      spiralOrder(input) shouldBe List(1, 2, 3, 6, 9, 8, 7, 4, 5)

      input = Array(
        Array(1, 2, 3, 4),
        Array(5, 6, 7, 8),
        Array(9, 10, 11, 12)
      )
      spiralOrder(input) shouldBe List(1, 2, 3, 4, 8, 12, 11, 10, 9, 5, 6, 7)
    }
  }
  //   1     2     3     4      5    6     7     8     9
  // [1,1] [1,2] [1,3] [2,3] [3,3] [3,2] [3,1] [2,1] [2,2]

  // [1] [2] [3]
  // [8] [9] [4]
  // [7] [6] [5]

  // [1]  [2]  [3]  [4]
  // [12] [13] [14] [5]
  // [11] [16] [15] [6]
  // [10] [9]  [8]  [7]

}
