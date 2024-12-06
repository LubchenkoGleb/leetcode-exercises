package leetcode_patterns

import scala.annotation.tailrec

object _9_increasingOrderSearchTree extends App {
  class TreeNode(
      _value: Int = 0,
      _left: TreeNode = null,
      _right: TreeNode = null
  ) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  def increasingBST(root: TreeNode): TreeNode = {
    def loop(tree: TreeNode, acc: List[Int]): List[Int] = {
      if (tree == null) acc
      else (loop(tree.left, acc) :+ tree.value) ++ loop(tree.right, acc)
    }

    def arrayToTree(rest: List[Int]): TreeNode =
      if (rest.nonEmpty) new TreeNode(rest.head, null, arrayToTree(rest.tail))
      else null

    val res = loop(root, Nil)
    arrayToTree(res)
  }

  val tree = new TreeNode(
    5,
    new TreeNode(3, new TreeNode(2, new TreeNode(1)), new TreeNode(4)),
    new TreeNode(6, null, new TreeNode(8, new TreeNode(7), new TreeNode(9)))
  )

  println(increasingBST(tree))
}
