package hlib.liubchenko.topinterview150.divide_and_conquer

import hlib.liubchenko.topinterview150.linked_list.ListNode
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _2_sort_list extends AnyWordSpec with Matchers {
  import scala.annotation.tailrec

  def merge(l1: ListNode, l2: ListNode): ListNode =
    if (l1 == null) l2
    else if (l2 == null) l1
    else if (l1.x > l2.x) { l2.next = merge(l1, l2.next); l2 }
    else { l1.next = merge(l1.next, l2); l1 }

  def findMid(l: ListNode): ListNode = {
    @tailrec
    def loop(slow: ListNode, fast: ListNode): ListNode =
      if (fast == null || fast.next == null) slow else loop(slow.next, fast.next.next)

    if (l == null) l else loop(l, l.next)
  }

  def sortList(head: ListNode): ListNode = if (head == null || head.next == null) head
  else {
    val mid = findMid(head)
    val rHead = mid.next
    mid.next = null

    val lSorted = sortList(head)
    val rSorted = sortList(rHead)
    merge(lSorted, rSorted)
  }

  "merge" should {
    "work as expected" in {
      val l1 = new ListNode(1, new ListNode(4, new ListNode(7, new ListNode(9))))
      val l2 = new ListNode(2, new ListNode(3, new ListNode(5)))
      merge(l1, l2).toList shouldBe List(1, 2, 3, 4, 5, 7, 9)
    }
  }

  "findMid" should {
    "work as expected" in {
      val l1 = new ListNode(1, new ListNode(4, new ListNode(7, new ListNode(9, new ListNode(11)))))
      findMid(l1).x shouldBe 7
      val l2 = new ListNode(1, new ListNode(4, new ListNode(7, new ListNode(9))))
      findMid(l2).x shouldBe 4
    }
  }

  "sortList" should {
    "work as expected" in {
      val l = new ListNode(4, new ListNode(2, new ListNode(1, new ListNode(3))))
      sortList(l).toList shouldBe List(1, 2, 3, 4)
    }
  }

}
