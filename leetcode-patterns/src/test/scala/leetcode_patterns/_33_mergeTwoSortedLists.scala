package leetcode_patterns

object _33_mergeTwoSortedLists extends App {
  case class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int = _x
  }

  def mergeTwoLists(list1: ListNode, list2: ListNode): ListNode = {
    if (list1 == null && list2 == null) null
    else if (list1 != null && list2 != null) {
      if (list1.x < list2.x) new ListNode(list1.x, mergeTwoLists(list1.next, list2))
      else new ListNode(list2.x, mergeTwoLists(list1, list2.next))
    } else if (list1 != null) list1
    else list2
  }

  val l1 = new ListNode(1, new ListNode(2, new ListNode(4)))
  val l2 = new ListNode(1, new ListNode(3, new ListNode(4)))
  println(mergeTwoLists(l1, l2))
  println(mergeTwoLists(null, null))
  println(mergeTwoLists(null, new ListNode(0)))

}
