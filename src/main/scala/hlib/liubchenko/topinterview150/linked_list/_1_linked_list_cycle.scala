package hlib.liubchenko.topinterview150.linked_list

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _1_linked_list_cycle extends AnyWordSpec with Matchers {

  // Definition for singly-linked list.
  class ListNode(var _x: Int = 0) {
    var next: ListNode = null
    var x: Int = _x
  }

  def hasCycle(head: ListNode): Boolean = {
    import scala.annotation.tailrec

    @tailrec
    def loop(slow: ListNode, fast: ListNode): Boolean = {
      if (slow == null || fast == null || fast.next == null) false
      else if (slow == fast) true
      else loop(slow.next, fast.next.next)
    }

    if (head == null) false else loop(head, head.next)
  }

  "hasCycle" should {
    "work as expected" in {
      val l3 = new ListNode(3)
      val l2 = new ListNode(2)
      val l0 = new ListNode(0)
      val l4 = new ListNode(4)
      l3.next = l2
      l2.next = l0
      l0.next = l4
      l4.next = l2

      hasCycle(l3) shouldEqual true
    }
  }
}
