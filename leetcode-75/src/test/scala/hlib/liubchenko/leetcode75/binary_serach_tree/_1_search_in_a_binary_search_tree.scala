package hlib.liubchenko.leetcode75.binary_serach_tree

import hlib.liubchenko.leetcode75.common.TreeNode
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _1_search_in_a_binary_search_tree extends AnyWordSpec with Matchers {
  sealed trait Solution {
    def searchBST(root: TreeNode, `val`: Int): TreeNode
  }

  // Mine, Runtime 44, Memory 84
  object Solution1 extends Solution {
    import scala.annotation.tailrec

    def searchBST(root: TreeNode, `val`: Int): TreeNode = {
      @tailrec
      def loop(node: TreeNode): TreeNode =
        if (node == null) null
        else if (node.value == `val`) node
        else if (`val` < node.value) loop(node.left)
        else loop(node.right)

      loop(root)
    }
  }

}
