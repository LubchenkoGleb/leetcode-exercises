package hlib.liubchenko.topinterview150.binary_tree.general

import hlib.liubchenko.topinterview150.binary_tree.TreeNode
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _4_symmetric_tree extends AnyWordSpec with Matchers {
  def isSymmetric(root: TreeNode): Boolean = {
    def compare(l: TreeNode, r: TreeNode): Boolean =
      if (l == null || r == null) l == r
      else l.value == r.value && compare(l.left, r.right) && compare(l.right, r.left)

//    def compare(l: TreeNode, r: TreeNode): Boolean = {
//      if (l != null && r != null && l.value == r.value) compare(l.left, r.right) && compare(l.right, r.left)
//      else l == r
//    }

    (root.left == null && root.right == null) || compare(root.left, root.right)
  }

}
