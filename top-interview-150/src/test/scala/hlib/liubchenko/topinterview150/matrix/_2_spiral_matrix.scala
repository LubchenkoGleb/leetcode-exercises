package hlib.liubchenko.topinterview150.matrix

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _2_spiral_matrix extends AnyWordSpec with Matchers {
  def spiralOrder(matrix: Array[Array[Int]]): List[Int] = {
    val (w, h) = (matrix(0).length - 1, matrix.length - 1)

    def loop(i: Int, j: Int, direction: Char, level: Int, count: Int): List[Int] = {
      // println(matrix(i)(j))

      val (nextI, nextJ, nextDirection) = direction match {
        case 'r' if j == w - level => (i + 1, j, 'b')
        case 'r'                   => (i, j + 1, 'r')
        case 'b' if i == h - level => (i, j - 1, 'l')
        case 'b'                   => (i + 1, j, 'b')
        case 'l' if j == level     => (i - 1, j, 'u')
        case 'l'                   => (i, j - 1, 'l')
        case 'u' if i == 1 + level => (i, j + 1, 'r')
        case 'u'                   => (i - 1, j, 'u')
      }
      val nextLevel = if (direction == 'u' && i == 1 + level) level + 1 else level

      val nextMoveAvailable: Boolean = count != matrix(0).length * matrix.length - 1
      matrix(i)(j) :: (if (nextMoveAvailable) loop(nextI, nextJ, nextDirection, nextLevel, count + 1) else Nil)
    }

    loop(0, 0, 'r', 0, 0)
  }

  "spiralOrder" should {
    "work as expected" in {
      var input = Array(
        Array(1, 2, 3),
        Array(4, 5, 6),
        Array(7, 8, 9)
      )
      spiralOrder(input) shouldBe List(1, 2, 3, 6, 9, 8, 7, 4, 5)

      input = Array(
        Array(1, 2, 3, 4),
        Array(5, 6, 7, 8),
        Array(9, 10, 11, 12)
      )
      spiralOrder(input) shouldBe List(1, 2, 3, 4, 8, 12, 11, 10, 9, 5, 6, 7)

      input = Array(
        Array(1, 2),
        Array(3, 4)
      )
      spiralOrder(input) shouldBe List(1, 2, 4, 3)
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
