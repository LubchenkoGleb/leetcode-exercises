package hlib.liubchenko.topinterview150.linked_list

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ListNode(_x: Int = 0, _next: ListNode = null) {
  var next: ListNode = _next
  var x: Int = _x
}

class _2_add_two_numbers extends AnyWordSpec with Matchers {
  def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {
    def sumAndRem(sum: Int) = {
      val newRem = if (sum > 9) 1 else 0
      val newSum = if (sum > 9) sum - 10 else sum
      (newSum, newRem)
    }

    def loop(l1Head: ListNode, l2Head: ListNode, rem: Int): ListNode = {
      if (l1Head == null && l2Head == null) if(rem != 0) new ListNode(rem) else null
      else if (l1Head == null) {
        val (newSum, newRem) = sumAndRem(l2Head.x + rem)
        new ListNode(_x = newSum, _next = loop(null, l2Head.next, newRem))
      } else if (l2Head == null) {
        val (newSum, newRem) = sumAndRem(l1Head.x + rem)
        new ListNode(_x = newSum, _next = loop(l1Head.next, null, newRem))
      } else {
        val (newSum, newRem) = sumAndRem(l1Head.x + l2Head.x + rem)
        new ListNode(_x = newSum, _next = loop(l1Head.next, l2Head.next, newRem))
      }
    }
    loop(l1, l2, 0)
  }

  def toList(node: ListNode): List[Int] = if (node == null) Nil else node.x :: toList(node.next)

  "addTwoNumbers" should {
    "work as expected" in {
      var l1 = new ListNode(2, new ListNode(4, new ListNode(3)))
      toList(l1) shouldBe List(2, 4, 3)
      var l2 = new ListNode(5, new ListNode(6, new ListNode(4)))
      toList(addTwoNumbers(l1, l2)) shouldBe List(7, 0, 8)

      l1 = new ListNode(2, new ListNode(4, new ListNode(5)))
      l2 = new ListNode(5, new ListNode(6, new ListNode(4)))
      toList(addTwoNumbers(l1, l2)) shouldBe List(7, 0, 0, 1)
    }
  }
}
