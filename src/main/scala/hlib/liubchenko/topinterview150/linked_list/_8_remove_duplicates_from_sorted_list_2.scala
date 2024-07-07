package hlib.liubchenko.topinterview150.linked_list

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _8_remove_duplicates_from_sorted_list_2 extends AnyWordSpec with Matchers {
  def deleteDuplicates_2(head: ListNode): ListNode = {
    import scala.annotation.tailrec

    @tailrec
    def loop(pointer: ListNode, isPrevUnique: Boolean, prev: ListNode, curr: ListNode): Unit = {
      if (curr == null) if (isPrevUnique) pointer.next = prev else ()
      else if (prev.x == curr.x) loop(pointer, isPrevUnique = false, curr, curr.next)
      else {
        if (isPrevUnique) {
          pointer.next = prev; prev.next = null; loop(pointer.next, isPrevUnique = true, curr, curr.next)
        } else loop(pointer, isPrevUnique = true, curr, curr.next)
      }
    }

    if (head == null || head.next == null) head
    else {
      val tmpNode = new ListNode(Int.MinValue)
      loop(tmpNode, head.x != head.next.x, head, head.next)
      tmpNode.next
    }
  }

  def deleteDuplicates(head: ListNode): ListNode = {
    def loop(head: ListNode, previousValue: Int): ListNode = {
      if (head == null) null
      else if (previousValue == head.x) loop(head.next, previousValue)
      else if (head.next != null && head.x == head.next.x) loop(head.next, head.x)
      else new ListNode(head.x, loop(head.next, head.x))
    }

    loop(head, Int.MinValue)
  }

  "deleteDuplicates" should {
    "work as expected" in {
      deleteDuplicates(ListNode(1, 1, 1, 2, 3, 3, 4)).toList shouldBe List(2, 4)
      deleteDuplicates(ListNode(1, 2, 3, 3, 4, 4, 5)).toList shouldBe List(1, 2, 5)
      deleteDuplicates(ListNode()) shouldBe null
      deleteDuplicates(ListNode(1, 1)) shouldBe null
      deleteDuplicates(ListNode(1, 2, 2)).toList shouldBe List(1)
    }
  }
}
