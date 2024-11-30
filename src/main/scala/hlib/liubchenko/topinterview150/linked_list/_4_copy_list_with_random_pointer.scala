package hlib.liubchenko.topinterview150.linked_list

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _4_copy_list_with_random_pointer extends AnyWordSpec with Matchers {

  class Node(var _value: Int) {
    var value: Int = _value
    var next: Node = null
    var random: Node = null
  }

  def copyRandomList(head: Node): Node = ???

  "copyRandomList" should {
    "work as expected" in pending
  }
}
