package hlib.liubchenko.topinterview150.math

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _2_plus_one extends AnyWordSpec with Matchers {

  def plusOne(digits: Array[Int]): Array[Int] = {
    def loop(i: Int, add: Boolean): List[Int] =
      if (i < 0) Option.when(add)(1).toList
      else {
        val digitToAdd = digits(i) + (if (add) 1 else 0)
        val newAdd = digitToAdd >= 10
         loop(i - 1, newAdd) :+ (digitToAdd % 10)
      }

    loop(digits.length - 1, add = true).toArray
  }

  "plusOne" should {
    "work as expeceted" in {
      plusOne(Array(1, 2, 3)) shouldBe Array(1, 2, 4)
      plusOne(Array(4, 3, 2, 1)) shouldBe Array(4, 3, 2, 2)
      plusOne(Array(9)) shouldBe Array(1, 0)
    }
  }
}
