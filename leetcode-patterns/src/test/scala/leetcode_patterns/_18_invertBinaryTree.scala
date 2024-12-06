package leetcode_patterns

object _18_invertBinaryTree extends App {
  case class TreeNode(
      _value: Int = 0,
      _left: TreeNode = null,
      _right: TreeNode = null
  ) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  def invertTree(root: TreeNode): TreeNode = {
    def loop(node: TreeNode): TreeNode = {
      if (node != null)
        new TreeNode(node.value, loop(node.right), loop(node.left))
      else node
    }

    loop(root)
  }

  val tree = TreeNode(
    4,
    TreeNode(2, TreeNode(1), TreeNode(3)),
    TreeNode(7, TreeNode(6), TreeNode(9))
  )

  val newTree = invertTree(tree)
  println(s"res: $newTree")
}
