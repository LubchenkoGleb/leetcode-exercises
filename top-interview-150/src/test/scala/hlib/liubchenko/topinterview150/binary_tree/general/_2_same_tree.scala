package hlib.liubchenko.topinterview150.binary_tree.general

import hlib.liubchenko.topinterview150.binary_tree.TreeNode

class _2_same_tree {
  def isSameTree(p: TreeNode, q: TreeNode): Boolean =
    if (p == null || q == null) p == q
    else if (p.value == q.value) isSameTree(p.left, q.left) && isSameTree(p.right, q.right)
    else false
}
