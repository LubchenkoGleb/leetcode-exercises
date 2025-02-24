package hlib.liubchenko.topinterview150.linked_list

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _3_merge_two_sorted_lists extends AnyWordSpec with Matchers {
  def mergeTwoLists(list1: ListNode, list2: ListNode): ListNode =
    if (list1 == null) list2
    else if (list2 == null) list1
    else if (list1.x < list2.x) { list1.next = mergeTwoLists(list1.next, list2); list1 }
    else { list2.next = mergeTwoLists(list1, list2.next); list2 }
}
