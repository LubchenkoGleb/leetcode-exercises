package hlib.liubchenko.topinterview150.binary_tree.general

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec


class Node(var _value: Int) {
  var value: Int = _value
  var left: Node = null
  var right: Node = null
  var next: Node = null
}

class _7_populating_next_right_pointers_in_each_node_2 extends AnyWordSpec with Matchers {
  import scala.annotation.tailrec
  import scala.collection.mutable

  def connect(root: Node): Node = {
    val q = mutable.Queue((root, 0))

    @tailrec
    def bfs(prev: Node, prevLevel: Int): Unit = if (q.nonEmpty) {
      val (curr, currLevel) = q.dequeue() match {
        case (next: Node, level) =>
          // println(s"${if (prev == null) "null" else prev.value} -> ${next.value}")
          q.enqueue((next.left, level + 1), (next.right, level + 1))
          (next, level + 1)
        case (null, _) => (prev, prevLevel)
      }
      if (prev != curr && prev != null && prevLevel == currLevel) prev.next = curr
      bfs(curr, currLevel)
    }

    bfs(null, 0)
    root
  }

  "connect" should {
    "work as expected" in {
      val _1 = new Node(1)
      val _2 = new Node(2)
      val _3 = new Node(3)
      val _4 = new Node(4)
      val _5 = new Node(5)
      val _7 = new Node(7)
      _1.left = _2
      _1.right = _3
      _2.left = _4
      _2.right = _5
      _3.right = _7

      connect(_1)

      _1.next shouldBe null
      _2.next.value shouldBe 3
      _3.next shouldBe null
      _4.next.value shouldBe 5
      _5.next.value shouldBe 7
      _7.next shouldBe null
    }
  }
}
