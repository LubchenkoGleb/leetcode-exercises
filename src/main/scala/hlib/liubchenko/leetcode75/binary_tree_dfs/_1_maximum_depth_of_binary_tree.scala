package hlib.liubchenko.leetcode75.binary_tree_dfs

import hlib.liubchenko.leetcode75.common.TreeNode
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _1_maximum_depth_of_binary_tree extends AnyWordSpec with Matchers {
  sealed trait Solution {
    def maxDepth(root: TreeNode): Int
  }

  // Mine, Runtime 64, Memory 92
  object Solution1 extends Solution {
    def maxDepth(root: TreeNode): Int = if (root == null) 0
    else 1 + math.max(maxDepth(root.left), maxDepth(root.right))
  }

  "All solutions" should {
    "work as expected" in {
      List(Solution1).foreach { solution =>
        //@formatter:off
        val tree = new TreeNode(0,
          new TreeNode(1,
            new TreeNode(2)
          ),
          new TreeNode(3)
        )
        //@formatter:on
        solution.maxDepth(tree) shouldBe 3
      }
    }
  }
}
