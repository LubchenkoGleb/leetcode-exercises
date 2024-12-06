package hlib.liubchenko.topinterview150.linked_list

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ListNode(_x: Int = 0, _next: ListNode = null) {
  var next: ListNode = _next
  var x: Int = _x

  def toList: List[Int] = ListNode.toList(this)
}
object ListNode {
  def toList(node: ListNode): List[Int] = if (node == null) Nil else node.x :: toList(node.next)

  def apply(elements: Int*): ListNode =
    if (elements.isEmpty) null else new ListNode(elements.head, apply(elements.tail: _*))
}
class ListNodeTest extends AnyWordSpec with Matchers {
  "toList and apply" should {
    "work as expected" in {
      ListNode(2, 4, 3).toList shouldBe List(2, 4, 3)
    }
  }
}
