package hlib.liubchenko.topinterview150.multi_dimensional_dp

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _9_maximal_square extends AnyWordSpec with Matchers {
  def maximalSquare(matrix: Array[Array[Char]]): Int = {
    val (w, h) = (matrix.head.length, matrix.length)
    val memo = Array.fill(h + 1, w + 1)(0)
    var max = 0

    for {
      i <- matrix.indices
      j <- matrix.head.indices
      if matrix(i)(j) == '1'
    } {
      memo(i + 1)(j + 1) = List(memo(i)(j), memo(i + 1)(j), memo(i)(j + 1)).min + 1
      if (memo(i + 1)(j + 1) > max) max = memo(i + 1)(j + 1)
    }

    max * max
  }

  def maximalSquare_bad(matrix: Array[Array[Char]]): Int = {
    import scala.annotation.tailrec

    val (w, h) = (matrix.head.length, matrix.length)

    @tailrec
    def check(i: Int, j: Int, level: Int): Int = {
      val checkRes = (0 to level).forall { l =>
        i + level < h &&
        j + level < w &&
        matrix(i + l)(j + level) == '1' &&
        matrix(i + level)(j + l) == '1'
      }
      if (checkRes) check(i, j, level + 1)
      else level * level
    }

    val res = for {
      i <- matrix.indices
      j <- matrix.head.indices
      if matrix(i)(j) == '1'
    } yield check(i, j, 1)

    res.maxOption.getOrElse(0)
  }

  "maximalSquare" should {
    "work as expected" in {
      val input = Array(
        Array('1', '0', '1', '0', '0'),
        Array('1', '0', '1', '1', '1'),
        Array('1', '1', '1', '1', '1'),
        Array('1', '0', '0', '1', '0')
      )
      maximalSquare(input) shouldBe 4
    }

    "" in {}
  }
}
