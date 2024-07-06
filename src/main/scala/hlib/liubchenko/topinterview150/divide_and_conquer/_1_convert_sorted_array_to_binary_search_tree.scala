package hlib.liubchenko.topinterview150.divide_and_conquer

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right

  def toList: List[Int] = {
    val l = if (left == null) Nil else left.toList
    val r = if (right == null) Nil else right.toList
    value :: l ::: r
  }
}

class _1_convert_sorted_array_to_binary_search_tree extends AnyWordSpec with Matchers {
  def sortedArrayToBST(nums: Array[Int]): TreeNode = {
    if (nums.isEmpty) null
    else {
      val mid = nums.length / 2
      new TreeNode(
        nums(mid),
        sortedArrayToBST(nums.take(mid)),
        sortedArrayToBST(nums.takeRight(nums.length - 1 - mid))
      )
    }
  }

  "sortedArrayToBST" should {
    "work as expected" in {
      sortedArrayToBST(Array(-10, -3, 0, 5, 9)).toList shouldBe List.empty
    }
  }
}
