package hlib.liubchenko.leetcode75.binary_tree_dfs

import hlib.liubchenko.leetcode75.common.TreeNode
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _3_Leaf_similar_trees extends AnyWordSpec with Matchers {
  // My, Runtime 9, Memory 6
  def leafSimilar(root1: TreeNode, root2: TreeNode): Boolean = {
    def leafs(head: TreeNode): List[Int] =
      if (head == null) Nil
      else if (head.left == null && head.right == null) List(head.value)
      else leafs(head.left) ::: leafs(head.right)

    leafs(root1) == leafs(root2)
  }

  "leafSimilar" should {
    "work as expected" in {
      val t1 = new TreeNode(
        3,
        new TreeNode(5, new TreeNode(6), new TreeNode(2, new TreeNode(7), new TreeNode(4))),
        new TreeNode(1, new TreeNode(9), new TreeNode(8))
      )

      val t2 = new TreeNode(
        3,
        new TreeNode(5, new TreeNode(6), new TreeNode(7)),
        new TreeNode(1, new TreeNode(4), new TreeNode(2, new TreeNode(9), new TreeNode(8)))
      )

      leafSimilar(t1, t2) shouldBe true
    }
  }
}
