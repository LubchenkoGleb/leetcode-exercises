package hlib.liubchenko.topinterview150.matrix

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _3_rotate_image extends AnyWordSpec with Matchers {
  def rotate(matrix: Array[Array[Int]]): Unit = ???

  "rotate" should {
    pending

    "work as expected #1" in {
      val matrix = Array(
        Array(1, 2, 3),
        Array(4, 5, 6),
        Array(7, 8, 9)
      )
      matrix.map(_.toList).toList shouldBe List(
        List(7, 4, 1),
        List(8, 5, 2),
        List(9, 6, 3)
      )
    }

    "work as expected #2" in {
      val matrix = Array(
        Array(5, 1, 9, 11),
        Array(2, 4, 8, 10),
        Array(13, 3, 6, 7),
        Array(15, 14, 12, 16)
      )
      matrix.map(_.toList).toList shouldBe List(
        List(15, 13, 2, 5),
        List(14, 3, 4, 1),
        List(12, 6, 8, 9),
        List(16, 7, 10, 11)
      )
    }
  }
}
