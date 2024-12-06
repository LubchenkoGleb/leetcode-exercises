package hlib.liubchenko.leetcode75.has_map_set

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _1_find_the_difference_of_two_arrays extends AnyWordSpec with Matchers {
  // Not mine, Runtime 64, Memory 61 for some reason `removedAll` is faster then `diff`
  def findDifference(nums1: Array[Int], nums2: Array[Int]): List[List[Int]] = {
    val (s1, s2) = (nums1.toSet, nums2.toSet)
    List((s1 -- s2).toList, (s2 -- s1).toList)
  }

  // Mine, Runtime 14, Memory 85
  def findDifference_2(nums1: Array[Int], nums2: Array[Int]): List[List[Int]] = {
    val (nums1Set, nums2Set) = (nums1.toSet, nums2.toSet)
    def diff(set1: Set[Int], set2: Set[Int]) =
      set1.foldLeft(List.empty[Int]) { case (acc, v) => if (set2.contains(v)) acc else acc :+ v }

    List(diff(nums1Set, nums2Set), diff(nums2Set, nums1Set))
  }

  // Mine, Runtime 47, Memory 32
  def findDifference_1(nums1: Array[Int], nums2: Array[Int]): List[List[Int]] = {
    val (nums1Set, nums2Set) = (nums1.toSet, nums2.toSet)
    List(nums1Set.diff(nums2Set).toList, nums2Set.diff(nums1Set).toList)
  }

  "findDifference" should {
    "work as expected" in {
      findDifference(Array(1, 2, 3), Array(2, 4, 6)) shouldBe List(List(1, 3), List(4, 6))
      findDifference(Array(1, 2, 3, 3), Array(1, 1, 2, 2)) shouldBe List(List(3), List())
    }
  }
}
