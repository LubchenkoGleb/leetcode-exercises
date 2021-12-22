package hlib.liubchenko

object _14_leafSimilarTrees extends App {
  class TreeNode(
      _value: Int = 0,
      _left: TreeNode = null,
      _right: TreeNode = null
  ) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  def leafSimilar(root1: TreeNode, root2: TreeNode): Boolean = {
    def getSequence(tree: TreeNode, acc: List[Int]): List[Int] =
      if (tree == null) acc
      else if (tree.left == null && tree.right == null) acc :+ tree.value
      else acc ++ getSequence(tree.left, Nil) ++ getSequence(tree.right, Nil)

    getSequence(root1, Nil) == getSequence(root2, Nil)
  }

  val tree1 = new TreeNode(
    3,
    new TreeNode(
      5,
      new TreeNode(6),
      new TreeNode(2, new TreeNode(7), new TreeNode(4))
    ),
    new TreeNode(1, new TreeNode(9), new TreeNode(8))
  )

  val tree2 = new TreeNode(
    3,
    new TreeNode(5, new TreeNode(6), new TreeNode(7)),
    new TreeNode(
      1,
      new TreeNode(4),
      new TreeNode(2, new TreeNode(9), new TreeNode(8))
    )
  )

  println(leafSimilar(tree1, tree2))
}
