package hlib.liubchenko.topinterview150.binary_tree

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right

  override def toString: String =
    if (left != null && right != null) s"[$value, $left, $right]"
    else s"($value)"
}

class TreeNodeTest extends AnyWordSpec with Matchers {
  "toString" should {
    "work as expected" in {
      val tree = new TreeNode(7, new TreeNode(3), new TreeNode(15, new TreeNode(9), new TreeNode(20)))
      tree.toString shouldBe "[7, (3), [15, (9), (20)]]"
    }
  }
}
