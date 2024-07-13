package hlib.liubchenko.topinterview150.hashmap

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _6_two_sum extends AnyWordSpec with Matchers {
  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    val numsMap = nums.zipWithIndex.groupMap(_._1)(_._2)
    numsMap.collectFirst {
      case (v, _) if target - v == v && numsMap(v).length == 2       => numsMap(v)
      case (v, i) if target - v != v && numsMap.contains(target - v) => numsMap(target - v) ++ i
    }.get
  }

  "twoSum" should {
    "work as expected" in {
      twoSum(Array(3, 3), 6) should contain theSameElementsAs Array(0, 1)
      twoSum(Array(2, 7, 11, 15), 9) should contain theSameElementsAs Array(0, 1)
      twoSum(Array(6, 5, 7, 8, 9, 3), 10) should contain theSameElementsAs Array(2, 5)
    }
  }
}
