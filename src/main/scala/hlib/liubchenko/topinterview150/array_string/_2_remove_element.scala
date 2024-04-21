package hlib.liubchenko.topinterview150.array_string

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _2_remove_element extends AnyWordSpec with Matchers {
  // println(s"i: $i, j: $j, nums: ${nums.mkString(", ")}")

  def removeElement(nums: Array[Int], `val`: Int): Int = {
    import scala.annotation.tailrec

    @tailrec
    def loop(i: Int, j: Int): Int =
      if (i > j) i
      else if (nums(j) == `val`) {
        nums(j) = 0
        loop(i, j - 1)
      } else if (nums(i) == `val`) {
        nums(i) = nums(j)
        nums(j) = 0
        loop(i + 1, j - 1)
      } else loop(i + 1, j)

    loop(0, nums.length - 1)
  }

  "removeElement" should {
    "work as expected" in {
      val a1 = Array(3, 2, 2, 3)
      removeElement(a1, 3) shouldBe 2
      a1.toList shouldBe List(2, 2, 0, 0)
      println()

      val a2 = Array(0, 1, 2, 2, 3, 0, 4, 2)
      removeElement(a2, 2) shouldBe 5
      a2.toList shouldBe List(0, 1, 4, 0, 3, 0, 0, 0)
      println()

      removeElement(Array.empty, 0) shouldBe 0
      println()

      val a4 = Array(1)
      removeElement(a4, 1) shouldBe 0
      a4.toList shouldBe List(0)
    }
  }
}
