package hlib.liubchenko.topinterview150.graph.general

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _3_clone_graph extends AnyWordSpec with Matchers {
  class Node(var _value: Int) {
    var value: Int = _value
    var neighbors: List[Node] = List()
  }

  def cloneGraph(graph: Node): Node = ???

  "cloneGraph" should {
    "work as expected" in {
      pending

      val n1 = new Node(1)
      val n2 = new Node(2)
      val n3 = new Node(3)
      val n4 = new Node(4)
      n1.neighbors = List(n2, n4)
      n2.neighbors = List(n1, n3)
      n3.neighbors = List(n2, n4)
      n4.neighbors = List(n1, n3)

      cloneGraph(n1)
    }
  }
}
