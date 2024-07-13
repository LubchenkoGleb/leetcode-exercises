package hlib.liubchenko.topinterview150.array_string

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _9_jump_game extends AnyWordSpec with Matchers {
  def canJump(nums: Array[Int]): Boolean = {
    var maxReachableIndex = 0
    var currentIndex = 0

    while (currentIndex < nums.length && maxReachableIndex >= currentIndex) {
      maxReachableIndex = math.max(maxReachableIndex, currentIndex + nums(currentIndex))
      currentIndex += 1
    }

    maxReachableIndex >= nums.length - 1
  }

  "canJump" should {
    "work as expected" in {
      canJump(Array(2, 3, 1, 1, 4)) shouldBe true
      canJump(Array(3, 2, 1, 0, 4)) shouldBe false
    }
  }
}
