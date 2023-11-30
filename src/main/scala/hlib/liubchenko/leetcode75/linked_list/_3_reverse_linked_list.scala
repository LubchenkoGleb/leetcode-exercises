package hlib.liubchenko.leetcode75.linked_list

import hlib.liubchenko.leetcode75.common.ListNode
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _3_reverse_linked_list extends AnyWordSpec with Matchers {
  import scala.annotation.tailrec

  // My, Runtime 57, Memory 9
  def reverseList(head: ListNode): ListNode = {
    @tailrec
    def loop(curr: ListNode, reversedList: ListNode): ListNode =
      if (curr == null) reversedList
      else {
        val next = curr.next
        curr.next = reversedList
        loop(next, curr)
      }

    loop(head, null)
  }

  "reverseList" should {
    "work as expected" in {
      reverseList(ListNode(1, 2, 3, 4, 5)).toString shouldBe "5 4 3 2 1"
      reverseList(new ListNode(1)).toString shouldBe "1"
      reverseList(null) shouldBe null
    }
  }
}
