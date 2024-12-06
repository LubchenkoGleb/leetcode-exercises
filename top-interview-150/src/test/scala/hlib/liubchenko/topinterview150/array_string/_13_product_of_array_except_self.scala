package hlib.liubchenko.topinterview150.array_string

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _13_product_of_array_except_self extends AnyWordSpec with Matchers {
  def productExceptSelf(nums: Array[Int]): Array[Int] = ???

  "productExceptSelf" should {
    "work as expected" in {
      pending

      productExceptSelf(Array(1, 2, 3, 4)) shouldBe Array(24, 12, 8, 6)
    }
  }
}
