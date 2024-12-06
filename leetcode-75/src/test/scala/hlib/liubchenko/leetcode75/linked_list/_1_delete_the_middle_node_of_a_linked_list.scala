package hlib.liubchenko.leetcode75.linked_list

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.annotation.tailrec

class _1_delete_the_middle_node_of_a_linked_list extends AnyWordSpec with Matchers {
  // Mine, Runtime 64, Memory 71
  class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int = _x

    override def toString: String = values(this, "")
  }

  @tailrec
  private def count(n: ListNode, c: Int): Int = if (n == null) c else count(n.next, c + 1)

  @tailrec
  private def get(n: ListNode, i: Int, currI: Int): ListNode =
    if (n == null || i == currI) n else get(n.next, i, currI + 1)

  def deleteMiddle(head: ListNode): ListNode = {
    val c = count(head, 0) / 2
    val m = get(head, c - 1, 0)
    if (m != null && m.next != null) { m.next = m.next.next; head }
    else m
  }

  @tailrec
  private def values(n: ListNode, acc: String): String =
    if (n == null) acc
    else values(n.next, if (acc == "") s"${n.x}" else s"$acc, ${n.x}")

  "deleteMiddle" should {
    def build(values: List[Int]) =
      values.reverse.map(new ListNode(_)).reduce { (a, b) => b.next = a; b }

    "work as expected" in {
      val n = build((0 to 7).toList)
      count(n, 0) shouldBe 8
      n.toString shouldBe (0 to 7).mkString(", ")
      get(n, 3, 0).toString shouldBe build((3 to 7).toList).toString
      deleteMiddle(n).toString shouldBe build(List(0, 1, 2, 3, 5, 6, 7)).toString
      deleteMiddle(new ListNode(1)) shouldBe null
    }
  }
}
