package leetcode_patterns

object _15_maximumDepthOfBinaryTree extends App {
  class TreeNode(
      _value: Int = 0,
      _left: TreeNode = null,
      _right: TreeNode = null
  ) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  def maxDepth(root: TreeNode): Int = {
    def loop(tree: TreeNode, curr: Int, max: Int): Int = {
      if (tree == null) math.max(curr, max)
      else
        math.max(
          loop(tree.left, curr + 1, max),
          loop(tree.right, curr + 1, max)
        )
    }

    loop(root, 0, 0)
  }

  def maxDepth2(root: TreeNode): Int = {
    def loop(tree: TreeNode): Int = {
      if (tree == null) 0
      else {
        val left = loop(tree.left)
        val right = loop(tree.right)

        if(left > right) left + 1
        else right + 1
      }
    }

    loop(root)
  }

  val tree = new TreeNode(
    3,
    new TreeNode(9),
    new TreeNode(20, new TreeNode(15), new TreeNode(7, new TreeNode(5)))
  )

  println(maxDepth(tree))
}
