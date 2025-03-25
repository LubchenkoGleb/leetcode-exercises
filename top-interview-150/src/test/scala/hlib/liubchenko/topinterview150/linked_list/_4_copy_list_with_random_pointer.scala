package hlib.liubchenko.topinterview150.linked_list

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _4_copy_list_with_random_pointer extends AnyWordSpec with Matchers {

  class Node(var _value: Int) {
    var value: Int = _value
    var next: Node = null
    var random: Node = null
  }

  def copyRandomList(head: Node): Node = {
    import scala.annotation.tailrec
    import scala.collection.mutable

    val mapping: mutable.Map[Node, Node] = mutable.Map.empty

    @tailrec
    def initMapping(head: Node): Unit = if (head != null) {
      val newHead = mapping.getOrElseUpdate(head, new Node(head.value))
      if (head.next != null) newHead.next = mapping.getOrElseUpdate(head.next, new Node(head.next.value))
      if (head.random != null) newHead.random = mapping.getOrElseUpdate(head.random, new Node(head.random.value))

      initMapping(head.next)
    }

    initMapping(head)
    mapping.getOrElse(head, null)
  }

  def copyRandomList_v1(head: Node): Node = {
    import scala.annotation.tailrec
    import scala.collection.mutable

    val oldToNewMapping: mutable.Map[Node, Node] = mutable.Map.empty

    @tailrec
    def initMapping(head: Node): Unit = if (head != null) {
      oldToNewMapping.put(head, new Node(head.value))
      initMapping(head.next)
    }

    def initLinks(head: Node): Unit = if (head != null) {
      val newHead = oldToNewMapping(head)
      if (head.next != null) newHead.next = oldToNewMapping(head.next)
      if (head.random != null) newHead.random = oldToNewMapping(head.random)
      initLinks(head.next)
    }

    initMapping(head)
    initLinks(head)
    oldToNewMapping(head)
  }

  "copyRandomList" should {
    "work as expected" in {
      (0 until 100000).map(_ => new Node(0).hashCode()).toSet.size shouldBe 100000

    }
  }
}
