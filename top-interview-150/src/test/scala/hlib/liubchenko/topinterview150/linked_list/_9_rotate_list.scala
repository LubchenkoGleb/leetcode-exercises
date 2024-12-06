package hlib.liubchenko.topinterview150.linked_list

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _9_rotate_list extends AnyWordSpec with Matchers {
  def rotateRight(head: ListNode, k: Int): ListNode = {
    import scala.annotation.tailrec

    def length(rem: ListNode): Int =
      if (rem == null) 0 else 1 + length(rem.next)

    @tailrec
    def takeN(n: Int, rem: ListNode): ListNode =
      if (n == 1) rem else takeN(n - 1, rem.next)

    if (k == 0) return head

    val l = length(head)
    if (l == 0) return head

    val shift = k % l
    if (shift == 0) return head

    val currHead = head
    val newLast = takeN(l - shift, head)
    val newHead = newLast.next
    newLast.next = null

    takeN(shift, newHead).next = currHead

    newHead

  }

  "rotateRight" should {
    "work as expected" in {
      var input = ListNode(1, 2, 3, 4, 5)
      rotateRight(input, 2).toList shouldBe List(4, 5, 1, 2, 3)

      input = ListNode(0, 1, 2)
      rotateRight(input, 4).toList shouldBe List(2, 0, 1)
    }
  }

}
