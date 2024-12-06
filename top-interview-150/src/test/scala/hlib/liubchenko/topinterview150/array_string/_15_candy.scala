package hlib.liubchenko.topinterview150.array_string

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _15_candy extends AnyWordSpec with Matchers {
  def candy(ratings: Array[Int]): Int = ???

  "candy" should {
    "work as expected" in {
      pending

      candy(Array(1, 0, 2)) shouldBe 5
      candy(Array(1, 2, 2)) shouldBe 4
    }
  }
}
