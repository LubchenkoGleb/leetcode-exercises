package hlib.liubchenko.topinterview150.binary_tree.bfs

import hlib.liubchenko.leetcode75.common.TreeNode
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec


class _4_binary_tree_zigzag_level_order_traversal extends AnyWordSpec with Matchers {
  def zigzagLevelOrder(root: TreeNode): List[List[Int]] = {
    import scala.collection.mutable

    val res = mutable.Map.empty[Int, List[Int]]

    def updatedValue(root: TreeNode, level: Int, curr: Option[List[Int]]) =
      if (level % 2 == 0) root.value :: curr.toList.flatten else curr.toList.flatten :+ root.value

    def loop(root: TreeNode, level: Int): Unit =
      if (root == null) ()
      else {
        res.updateWith(level)(curr => Some(updatedValue(root, level, curr)))
        loop(root.left, level + 1)
        loop(root.right, level + 1)
      }

    loop(root, 1)
    res.toList.sortBy(_._1).map(_._2)
  }

  "zigzagLevelOrder" should {
    "work as expected" in {
      val input = new TreeNode(3, new TreeNode(9), new TreeNode(20, new TreeNode(15), new TreeNode(7)))
      zigzagLevelOrder(input) shouldBe List(List(3), List(20, 9), List(15, 7))
    }
  }
}
