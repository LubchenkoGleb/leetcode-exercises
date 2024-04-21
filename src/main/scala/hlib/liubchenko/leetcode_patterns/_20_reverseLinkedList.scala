package hlib.liubchenko.leetcode_patterns

import scala.annotation.tailrec

object _20_reverseLinkedList extends App {
  case class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int = _x
  }

  def reverseList(head: ListNode): ListNode = {
    @tailrec
    def loop(rest: ListNode, reversed: ListNode): ListNode =
      if (rest == null) null
      else if (rest != null && rest.next == null) new ListNode(rest.x, reversed)
      else loop(rest.next, new ListNode(rest.x, reversed))

    loop(head, null)
  }

  val list = ListNode(1, ListNode(2, ListNode(3, ListNode(4, ListNode(5)))))
  println(reverseList(list))
}
