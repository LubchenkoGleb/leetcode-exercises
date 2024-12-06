package hlib.liubchenko.topinterview150.binary_tree.search

import hlib.liubchenko.topinterview150.binary_tree.TreeNode
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _2_kth_smallest_element_in_a_bst extends AnyWordSpec with Matchers {
  def kthSmallest(root: TreeNode, k: Int): Int = ???

  "levelOrder" should {
    "work as expected #1" in {
      pending

      val tree = new TreeNode(3, new TreeNode(1, null, new TreeNode(2)), new TreeNode(4))
      kthSmallest(tree, 1) shouldBe 1
    }

    "work as expected #2" in {
      pending

      val tree = new TreeNode(5, new TreeNode(3, new TreeNode(2, new TreeNode(1)), new TreeNode(4)), new TreeNode(6))
      kthSmallest(tree, 3) shouldBe 3
    }
  }
}
