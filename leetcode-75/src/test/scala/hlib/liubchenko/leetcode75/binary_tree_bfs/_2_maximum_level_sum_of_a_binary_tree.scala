package hlib.liubchenko.leetcode75.binary_tree_bfs

import hlib.liubchenko.leetcode75.common.TreeNode
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _2_maximum_level_sum_of_a_binary_tree extends AnyWordSpec with Matchers {
  // My, Runtime 87, Memory 87
  def maxLevelSum(root: TreeNode): Int = {
    val sum = collection.mutable.HashMap.empty[Int, Int]

    def loop(level: Int, head: TreeNode): Unit = if (head != null) {
      sum.updateWith(level)(_.orElse(Some(0)).map(_ + head.value))
      loop(level + 1, head.left)
      loop(level + 1, head.right)
    }

    loop(1, root)
    sum.maxBy(_._2)._1
  }

  "maxLevelSum" should {
    "work as expected" in {
      val tree = new TreeNode(
        1,
        new TreeNode(7, new TreeNode(7), new TreeNode(-8)),
        new TreeNode(0)
      )
      maxLevelSum(tree) shouldBe 2
    }
  }
}
