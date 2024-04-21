package hlib.liubchenko.topinterview150.binary_tree_general

class _3_invert_binary_tree {
  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  def invertTree(root: TreeNode): TreeNode = {
    def loop(root: TreeNode): Unit = if (root != null) {
      val tmp = root.right
      root.right = root.left
      root.left = tmp
      loop(root.left)
      loop(root.right)
    }

    loop(root)
    root
  }

  def invertTree_2(root: TreeNode): TreeNode =
    if (root != null)
      new TreeNode(root.value, invertTree(root.right), invertTree(root.left))
    else null

}
