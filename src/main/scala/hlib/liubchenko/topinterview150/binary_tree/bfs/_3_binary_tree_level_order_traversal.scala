package hlib.liubchenko.topinterview150.binary_tree.bfs

import hlib.liubchenko.topinterview150.binary_tree.TreeNode
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _3_binary_tree_level_order_traversal extends AnyWordSpec with Matchers {
  def levelOrder(root: TreeNode): List[List[Int]] = ???

  "levelOrder" should {
    "work as expected" in {
      val tree = new TreeNode(3, new TreeNode(9), new TreeNode(20, new TreeNode(15), new TreeNode(7)))
      levelOrder(tree) shouldBe List(List(3), List(9, 20), List(15, 7))
    }
  }
}
