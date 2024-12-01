package hlib.liubchenko.topinterview150.binary_tree.search

import hlib.liubchenko.topinterview150.binary_tree.TreeNode
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _3_validate_binary_search_tree extends AnyWordSpec with Matchers {
  def isValidBST(root: TreeNode): Boolean = ???

  "isValidBST" should {
    "work as expected" in {
      isValidBST(new TreeNode(2, new TreeNode(1), new TreeNode(3))) shouldBe true
      isValidBST(new TreeNode(5, new TreeNode(1), new TreeNode(4, new TreeNode(3), new TreeNode(6)))) shouldBe false
    }
  }
}
