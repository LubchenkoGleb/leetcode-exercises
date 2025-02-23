package hlib.liubchenko.topinterview150.binary_tree.general

import hlib.liubchenko.topinterview150.binary_tree.TreeNode
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _13_count_complete_tree_nodes extends AnyWordSpec with Matchers {
  def countNodes_2(root: TreeNode): Int = {
    if (root == null) 0
    else if (root.right == null && root.left == null) 1
    else 1 + countNodes(root.left) + countNodes(root.right)
  }

  def height(tree: TreeNode, next: TreeNode => TreeNode): Int =
    if (tree == null) 0
    else 1 + height(next(tree), next)

  def countNodes(root: TreeNode): Int =
    if (root == null) 0
    else {
      val lH = height(root, _.left)
      val rH = height(root, _.right)
      if (lH == rH) math.pow(2, lH).toInt - 1
      else 1 + countNodes(root.left) + countNodes(root.right)
    }

  "countNodes" should {
    "work as expected #1" in {
      val tree = new TreeNode(1, new TreeNode(2, new TreeNode(4), new TreeNode(5)), new TreeNode(3, new TreeNode(6)))
      countNodes(tree) shouldBe 6
    }

    "work as expected #2" in {
      val tree = new TreeNode(1, new TreeNode(2, new TreeNode(4)), new TreeNode(3))
      countNodes(tree) shouldBe 4
    }

    "work as expected #3" in {
      val tree = new TreeNode(1)
      countNodes(tree) shouldBe 1
    }
  }
}
