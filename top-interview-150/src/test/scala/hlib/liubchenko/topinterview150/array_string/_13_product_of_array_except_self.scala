package hlib.liubchenko.topinterview150.array_string

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _13_product_of_array_except_self extends AnyWordSpec with Matchers {
  def productExceptSelf(nums: Array[Int]): Array[Int] = {
    val ans = nums.scan(1)(_ * _).dropRight(1)

    var suffixProduct = 1
    nums.indices.reverse.foreach { i =>
      ans(i) *= suffixProduct
      suffixProduct *= nums(i)
    }

    ans
  }

  "productExceptSelf" should {
    "work as expected" in {
      productExceptSelf(Array(1, 2, 3, 4)) shouldBe Array(24, 12, 8, 6)
    }
  }
}
