package hlib.liubchenko.leetcode75.common

import scala.annotation.tailrec

class ListNode(_x: Int = 0, _next: ListNode = null) {
  var next: ListNode = _next
  var x: Int = _x

  override def toString: String = {
    @tailrec
    def loop(head: ListNode, acc: String): String =
      if (head != null) loop(head.next, if (acc.isEmpty) head.x.toString else s"$acc ${head.x}") else acc
    loop(this, "")
  }
}
object ListNode {
  def apply(values: Int*): ListNode = {
    @tailrec
    def loop(head: ListNode, tail: ListNode, values: Seq[Int]): ListNode =
      if (values.isEmpty) head
      else {
        val newV = new ListNode(values.head)
        if (head == null) loop(newV, newV, values.tail)
        else { tail.next = newV; loop(head, newV, values.tail) }
      }

    loop(null, null, values)
  }
}