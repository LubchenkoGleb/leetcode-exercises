package hlib.liubchenko.topinterview150.binary_tree.general

import hlib.liubchenko.topinterview150.binary_tree.TreeNode
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _10_sum_root_to_leaf_numbers extends AnyWordSpec with Matchers {
  def sumNumbers(root: TreeNode): Int = {
    def loop(root: TreeNode, sum: Int): Int =
      if (root == null) 0
      else {
        val updatedSum = 10 * sum + root.value
        if (root.left == null && root.right == null) updatedSum
        else loop(root.left, updatedSum) + loop(root.right, updatedSum)
      }

    loop(root, 0)
  }

  "sumNumbers" should {
    "work as expected" in {
      sumNumbers(new TreeNode(1, new TreeNode(2), new TreeNode(3))) shouldBe 25
      sumNumbers(new TreeNode(4, new TreeNode(9, new TreeNode(5), new TreeNode(1)), new TreeNode(0))) shouldBe 1026
    }
  }
}
