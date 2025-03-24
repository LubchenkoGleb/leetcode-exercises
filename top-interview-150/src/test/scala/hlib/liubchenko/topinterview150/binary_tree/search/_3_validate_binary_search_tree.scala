package hlib.liubchenko.topinterview150.binary_tree.search

import hlib.liubchenko.topinterview150.binary_tree.TreeNode
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _3_validate_binary_search_tree extends AnyWordSpec with Matchers {
  def isValidBST_low_performance(root: TreeNode): Boolean = {
    def traverse(root: TreeNode): List[Int] =
      if (root == null) Nil else traverse(root.left) ++ List(root.value) ++ traverse(root.right)

    traverse(root).sliding(2).forall {
      case a :: b :: Nil => a < b
      case _             => true
    }
  }

  def isValidBST(root: TreeNode): Boolean = {
    def traverse(root: TreeNode, min: Long, max: Long): Boolean = root == null ||
      (root.value < max && root.value > min &&
        traverse(root.left, min, root.value) &&
        traverse(root.right, root.value, max))

    traverse(root, Long.MinValue, Long.MaxValue)
  }

  "isValidBST" should {
    "work as expected" in {
      isValidBST(new TreeNode(2, new TreeNode(1), new TreeNode(3))) shouldBe true
      isValidBST(new TreeNode(5, new TreeNode(1), new TreeNode(4, new TreeNode(3), new TreeNode(6)))) shouldBe false
      isValidBST(new TreeNode(5, new TreeNode(4), new TreeNode(6, new TreeNode(3), new TreeNode(7)))) shouldBe false
      isValidBST(
        new TreeNode(
          32,
          new TreeNode(26, new TreeNode(19, null, new TreeNode(27))),
          new TreeNode(47, null, new TreeNode(56))
        )
      ) shouldBe false
    }
  }
}
