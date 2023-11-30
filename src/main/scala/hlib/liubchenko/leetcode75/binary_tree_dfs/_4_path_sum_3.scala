package hlib.liubchenko.leetcode75.binary_tree_dfs

import hlib.liubchenko.leetcode75.common.TreeNode
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _4_path_sum_3 extends AnyWordSpec with Matchers {
  // My, Runtime 92, Memory 46
  def pathSum(root: TreeNode, targetSum: Int): Int = {
    def loop(head: TreeNode, acc: Long, resetAcc: Boolean): Int =
      if (head == null) 0
      else {
        val c = if (head.value + acc == targetSum) 1 else 0
        println(s"[${head.value}] $c, acc $acc, resetAcc: $resetAcc")
        val newAcc = acc + head.value
        val resetAccRes =
          if (resetAcc) loop(head.left, 0, resetAcc = true) + loop(head.right, 0, resetAcc = true) else 0
        c + resetAccRes + loop(head.left, newAcc, resetAcc = false) + loop(head.right, newAcc, resetAcc = false)
      }

    loop(root, 0, resetAcc = true)
  }

  "pathSum" should {
    "work as expected" in {
      val tree1 = new TreeNode(
        10,
        new TreeNode(
          5,
          new TreeNode(3, new TreeNode(8), new TreeNode(-2)),
          new TreeNode(2, null, new TreeNode(1))
        ),
        new TreeNode(-3, null, new TreeNode(11))
      )
      // pathSum(tree1, 8) shouldBe 4

      val tree2 = new TreeNode(
        1000000000,
        new TreeNode(
          1000000000,
          new TreeNode(294967296, new TreeNode(1000000000, new TreeNode(1000000000, new TreeNode(1000000000))))
        )
      )
      pathSum(tree2, 0) shouldBe 0
    }
  }
}
