package hlib.liubchenko.topinterview150.matrix

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _3_rotate_image extends AnyWordSpec with Matchers {

  def rotate(matrix: Array[Array[Int]]): Unit = {
    val n = matrix.length

    def rotateLevel(l: Int): Unit = (l until n - l - 1).foreach(rotateElement(l, _))

    def rotateElement(l: Int, j: Int): Unit = {
      println(s"($l, $j) ${matrix(l)(j)}")
      val num = j - l

      val (_0i, _0j) = (l, j)
      val _0 = matrix(_0i)(_0j)

      val (_90i, _90j) = (l + num, n - l - 1)
      val _90 = matrix(_90i)(_90j)

      val (_180i, _180j) = (n - l - 1, n - l - 1 - num)
      val _180 = matrix(_180i)(_180j)

      val (_270i, _270j) = (n - l - 1 - num, l)
      val _270 = matrix(_270i)(_270j)

      matrix(_90i)(_90j) = _0
      println(matrix.map(_.mkString(" ")).mkString("\n", "\n", ""))

      matrix(_180i)(_180j) = _90
      println(matrix.map(_.mkString(" ")).mkString("\n", "\n", ""))

      matrix(_270i)(_270j) = _180
      println(matrix.map(_.mkString(" ")).mkString("\n", "\n", ""))

      matrix(_0i)(_0j) = _270
      println(matrix.map(_.mkString(" ")).mkString("\n", "\n", "\n"))
    }

    (0 until n / 2).foreach(rotateLevel)
  }

  "rotate" should {
    "work as expected #1" in {
      val matrix = Array(
        Array(1, 2, 3),
        Array(4, 5, 6),
        Array(7, 8, 9)
      )
      rotate(matrix)
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
      rotate(matrix)
      matrix.map(_.toList).toList shouldBe List(
        List(15, 13, 2, 5),
        List(14, 3, 4, 1),
        List(12, 6, 8, 9),
        List(16, 7, 10, 11)
      )
    }
  }
}
