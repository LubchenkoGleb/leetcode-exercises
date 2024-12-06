package hlib.liubchenko.leetcode75.binary_tree_dfs

import hlib.liubchenko.leetcode75.common.TreeNode
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _2_count_good_nodes_in_binary_tree extends AnyWordSpec with Matchers {
  // My, Runtime 77, Memory 77
  def goodNodes(root: TreeNode): Int = {
    def loop(head: TreeNode, max: Int): Int = if (head == null) 0
    else {
      val newMax = math.max(max, head.value)
      val curr = if (head.value >= max) 1 else 0
      curr + loop(head.left, newMax) + loop(head.right, newMax)
    }

    loop(root, Int.MinValue)
  }

  "goodNodes" should {
    "work as expected" in {
      val tree1 = new TreeNode(
        3,
        new TreeNode(1, new TreeNode(3)),
        new TreeNode(4, new TreeNode(1), new TreeNode(5))
      )
      goodNodes(tree1) shouldBe 4

      val tree2 = new TreeNode(
        3,
        new TreeNode(3, new TreeNode(4), new TreeNode(2))
      )
      goodNodes(tree2) shouldBe 3
    }
  }
}
