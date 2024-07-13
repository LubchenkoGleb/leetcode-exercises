package hlib.liubchenko.topinterview150.binary_tree

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _1_minimum_absolute_difference_in_bst extends AnyWordSpec with Matchers {
  def getMinimumDifference(root: TreeNode): Int = {
    def loop(root: TreeNode): List[Int] =
      if (root == null) Nil
      else loop(root.left) ++ List(root.value) ++ loop(root.right)

    val list = loop(root)
    list.sliding(2).collect { case a :: b :: Nil => b - a }.min
  }

  "getMinimumDifference" should {
    "work as expected" in {
      var tree = new TreeNode(4, new TreeNode(2, new TreeNode(1), new TreeNode(3)), new TreeNode(6))
      getMinimumDifference(tree) shouldBe 1

      tree = new TreeNode(1, new TreeNode(0), new TreeNode(48, new TreeNode(12), new TreeNode(49)))
      getMinimumDifference(tree) shouldBe 1
    }
  }
}
