package hlib.liubchenko.leetcode75.linked_list

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
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

class _2_odd_even_linked_list extends AnyWordSpec with Matchers {
  // My, Runtime 100, Memory 90
  def oddEvenList(head: ListNode): ListNode = {
    import scala.annotation.tailrec

    @tailrec
    def loop(isOdd: Boolean, head: ListNode, oddTail: ListNode, evenTail: ListNode, evenHead: ListNode): Unit = {
      println(s"after: isOdd: $isOdd, head: $head, oddTail: $oddTail, evenTail: $evenTail, evenHead: $evenHead")
      if (head == null) oddTail.next = evenHead
      else {
        val next = head.next
        head.next = null

        if (isOdd) {
          if (oddTail != null) oddTail.next = head
          loop(isOdd = false, next, head, evenTail, evenHead)
        } else {
          val newEvenHead = if (evenHead == null) head else { evenTail.next = head; evenHead }
          loop(isOdd = true, next, oddTail, head, newEvenHead)
        }
      }
    }

    if (head != null && head.next != null) loop(isOdd = true, head, null, null, null)
    head
  }

  "oddEvenList" should {
    "work as expected" in {
//      val list1 = ListNode(1, 2, 3, 4, 5)
//      list1.toString shouldBe "1 2 3 4 5"
//      oddEvenList(list1).toString shouldBe "1 3 5 2 4"

      val list2 = ListNode(2, 1, 3, 5, 6, 4, 7)
      list2.toString shouldBe "2 1 3 5 6 4 7"
      oddEvenList(list2).toString shouldBe "2 3 6 7 1 5 4"
    }
  }
}
