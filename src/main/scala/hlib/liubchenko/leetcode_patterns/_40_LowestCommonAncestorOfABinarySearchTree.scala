package hlib.liubchenko.leetcode_patterns

import scala.annotation.tailrec

object _40_LowestCommonAncestorOfABinarySearchTree extends App {
  class TreeNode(var _value: Int) {
    var value: Int = _value
    var left: TreeNode = null
    var right: TreeNode = null
  }

  @tailrec
  def lowestCommonAncestor(root: TreeNode, p: TreeNode, q: TreeNode): TreeNode =
    if (p.value < root.value && q.value < root.value) lowestCommonAncestor(root.left, p, q)
    else if (p.value > root.value && q.value > root.value) lowestCommonAncestor(root.right, p, q)
    else root

  val _0 = new TreeNode(0)
  val _2 = new TreeNode(2)
  val _3 = new TreeNode(3)
  val _4 = new TreeNode(4)
  val _5 = new TreeNode(5)
  val _6 = new TreeNode(6)
  val _7 = new TreeNode(7)
  val _8 = new TreeNode(8)
  val _9 = new TreeNode(9)

  _6.left = _2
  _6.right = _8

  _2.left = _0
  _2.right = _4

  _4.left = _3
  _4.right = _5

  _8.left = _7
  _8.right = _9

  println(lowestCommonAncestor(_6, _3, _7).value) // 6
  println(lowestCommonAncestor(_6, _3, _5).value) // 4
  println(lowestCommonAncestor(_6, _2, _5).value) // 2
}
