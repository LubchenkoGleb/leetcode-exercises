package hlib.liubchenko.visa

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class PreScreen extends AnyWordSpec with Matchers {
  private def game(bubbles: Array[Array[Int]], moves: Array[(Int, Int)]): Array[Array[Int]] = {
    val height = bubbles.length
    val width = bubbles.headOption.map(_.length).getOrElse(0)

    def shiftCol(rowIncl: Int, col: Int): Unit = {
      val r = if (rowIncl == 0) 0 else rowIncl - 1
      (r until height).foreach { i =>
        val next = if (i + 1 == height) 0 else bubbles(i + 1)(col)
        bubbles(i)(col) = next
      }
    }

    def isFit(r: Int, c: Int, checkR: Int, checkC: Int) =
      checkC >= 0 && checkC < width && checkR >= 0 && checkR < height && bubbles(r)(c) == bubbles(checkR)(checkC)

    moves.foreach { case (r, c) =>
      if (isFit(r, c, r, c - 1)) shiftCol(r + 1, c - 1)

      if (isFit(r, c, r - 1, c)) shiftCol(r - 2, c)

      shiftCol(r - 1, c)

      if (isFit(r, c, r + 1, c)) shiftCol(r, c)

      if (isFit(r, c, r, c + 1)) shiftCol(r + 1, c + 1)
    }

    bubbles
  }

  private def shiftArray(arr: Array[Int], startIndexIncl: Int): Array[Int] = {
    val s = if (startIndexIncl == 0) 0 else startIndexIncl - 1
    (s until arr.length).foreach { i =>
      val next = if (i + 1 == arr.length) 0 else arr(i + 1)
      arr(i) = next
    }
    arr
  }

  "shiftArray" should {
    "work as expected" in {
      shiftArray(Array(1, 2, 3, 4), 2).toList shouldBe List(1, 3, 4, 0)
      shiftArray(Array(1), 1).toList shouldBe List(0)
      shiftArray(Array.empty, 0).toList shouldBe List.empty
    }
  }

  "game" should {
    def input = Array(
      Array(3, 3, 2, 2, 1),
      Array(5, 4, 3, 2, 1),
      Array(3, 3, 2, 1, 1),
      Array(3, 2, 2, 2, 3),
      Array(6, 4, 2, 1, 1),
      Array(4, 1, 3, 2, 5),
      Array(2, 5, 5, 5, 3)
    )

    def printArray(array: Array[Array[Int]]) = array.map(_.mkString(" ")).mkString("\n")

    "work as expected after the first move" in {
      val actual = printArray(game(input, Array((3, 2))))
      println(actual)

      actual shouldEqual
        """3 0 0 0 1
          |5 3 0 2 1
          |3 4 0 2 1
          |3 3 2 1 3
          |6 4 3 1 1
          |4 1 3 2 5
          |2 5 5 5 3""".stripMargin
    }
  }
}
