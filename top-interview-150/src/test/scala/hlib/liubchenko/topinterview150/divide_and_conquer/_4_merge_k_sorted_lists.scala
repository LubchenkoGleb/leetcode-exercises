package hlib.liubchenko.topinterview150.divide_and_conquer

import hlib.liubchenko.topinterview150.linked_list.ListNode

class _4_merge_k_sorted_lists {
  def mergeKLists(lists: Array[ListNode]): ListNode = {
    def merge(a: ListNode, b: ListNode): ListNode =
      if (a == null) b
      else if (b == null) a
      else if (a.x > b.x) { b.next = merge(a, b.next); b }
      else { a.next = merge(a.next, b); a }

    lists.fold(null)(merge)
  }
}
