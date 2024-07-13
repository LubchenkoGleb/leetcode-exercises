package hlib.liubchenko.topinterview150.matrix

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _4_set_matrix_zeroes extends AnyWordSpec with Matchers {
  def setZeroes(matrix: Array[Array[Int]]): Unit = {
    val zeroElements = for {
      i <- matrix.indices
      j <- matrix(0).indices
      if matrix(i)(j) == 0
    } yield (i, j)

    val (zeroRows, zeroColumns) = zeroElements.unzip
    for {
      i <- zeroRows.distinct
      j <- matrix(i).indices
    } matrix(i)(j) = 0

    for {
      j <- zeroColumns.distinct
      i <- matrix.indices
    } matrix(i)(j) = 0
  }

  "setZeroes" should {
    "work as expected #1" in {
      val matrix = Array(
        Array(0, 1, 2, 0),
        Array(3, 4, 5, 2),
        Array(1, 3, 1, 5)
      )
      setZeroes(matrix)
      matrix.map(_.toList).toList shouldBe List(
        List(0, 0, 0, 0),
        List(0, 4, 5, 0),
        List(0, 3, 1, 0)
      )
    }

    "work as expected #2" in {
      val matrix = Array(
        Array(1, 1, 1),
        Array(1, 0, 1),
        Array(1, 1, 1)
      )
      setZeroes(matrix)
      matrix.map(_.toList).toList shouldBe List(
        List(1, 0, 1),
        List(0, 0, 0),
        List(1, 0, 1)
      )
    }
  }
}
