package leetcode_patterns

object _10_maximumDepthOfNAryTree extends App {
  class Node(var _value: Int) {
    var value: Int = _value
    var children: List[Node] = List()
  }

  def maxDepth(root: Node): Int = {
    def loop(tree: Node, cur: Int): Int = {
      if (tree == null) cur
      else if (tree.children.isEmpty) cur + 1
      else tree.children.map(c => loop(c, cur + 1)).max
    }

    loop(root, 0)
  }

  val _1 = new Node(1)
  val _2 = new Node(2)
  val _3 = new Node(3)
  val _4 = new Node(4)
  val _5 = new Node(5)
  val _6 = new Node(6)
  val _7 = new Node(7)
  val _8 = new Node(8)
  val _9 = new Node(9)
  val _10 = new Node(10)
  val _11 = new Node(11)
  val _12 = new Node(12)
  val _13 = new Node(13)
  val _14 = new Node(14)

  _1.children = List(_2, _3, _4, _5)
  _3.children = List(_6, _7)
  _7.children = List(_11)
  _11.children = List(_14)
  _4.children = List(_8)
  _8.children = List(_12)
  _5.children = List(_9, _10)
  _9.children = List(_13)

  println(maxDepth(_1))
}
