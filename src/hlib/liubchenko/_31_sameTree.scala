package hlib.liubchenko

object _31_sameTree extends App {
  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  def isSameTree(p: TreeNode, q: TreeNode): Boolean =
    if (p == null && q == null) true
    else if (p == null || q == null) false
    else if (p.value != q.value) false
    else isSameTree(p.left, q.left) && isSameTree(p.right, q.right)

  val t1 = new TreeNode(1, new TreeNode(2), new TreeNode(3))
  val t2 = new TreeNode(1, new TreeNode(2), new TreeNode(1))
  val t3 = new TreeNode(1, new TreeNode(1), new TreeNode(2))
  val t4 = new TreeNode(1, new TreeNode(2, new TreeNode(4)), new TreeNode(3))

  println(isSameTree(t1, t1))
  println(isSameTree(t2, t3))
  println(isSameTree(t1, t4))

}
