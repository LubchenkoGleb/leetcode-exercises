package hlib.liubchenko.topinterview150.matrix

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _1_valid_sudoku extends AnyWordSpec with Matchers {
  def isValidSudoku(board: Array[Array[Char]]): Boolean = {
    def validate(value: Array[Char]) = {
      val numbers = value.collect { case v if v.isDigit => v.toByte }
      numbers.length == numbers.filterNot(_ == 0).distinct.length
    }
    def getCol(i: Int) = board.map(_(i))
    def squareValues(pI: Int, pJ: Int) =
      for { i <- 0 until 3; j <- 0 until 3 } yield board(pI + i)(pJ + j)
    def squareIndices = for { i <- List(0, 3, 6); j <- List(0, 3, 6) } yield (i, j)

    board.forall(validate) &&
    board.indices.forall(i => validate(getCol(i))) &&
    squareIndices.forall { case (i, j) => validate(squareValues(i, j).toArray) }
  }

  "isValidSudoku" should {
    "work as expected" in {
      val input = Array(
        Array('5', '3', '.', '.', '7', '.', '.', '.', '.'),
        Array('6', '.', '.', '1', '9', '5', '.', '.', '.'),
        Array('.', '9', '8', '.', '.', '.', '.', '6', '.'),
        Array('8', '.', '.', '.', '6', '.', '.', '.', '3'),
        Array('4', '.', '.', '8', '.', '3', '.', '.', '1'),
        Array('7', '.', '.', '.', '2', '.', '.', '.', '6'),
        Array('.', '6', '.', '.', '.', '.', '2', '8', '.'),
        Array('.', '.', '.', '4', '1', '9', '.', '.', '5'),
        Array('.', '.', '.', '.', '8', '.', '.', '7', '9')
      )

      isValidSudoku(input) shouldBe true
    }
  }
}
