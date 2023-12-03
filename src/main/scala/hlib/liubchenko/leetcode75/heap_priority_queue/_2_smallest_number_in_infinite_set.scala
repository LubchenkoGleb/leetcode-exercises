package hlib.liubchenko.leetcode75.heap_priority_queue

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

// My, Runtime 100, Memory 33
class SmallestInfiniteSet() {
  import scala.collection.mutable

  private val s = mutable.SortedSet(1)

  def popSmallest(): Int = {
    val v = s.head
    s.remove(v)
    if (s.isEmpty) s.add(v + 1)
    // println(s"After pop: ${s.mkString(", ")}")
    v
  }

  def addBack(num: Int) = {
    if (num < s.last) s.add(num)
    // println(s"After add: ${s.mkString(", ")}")
  }
}

class _2_smallest_number_in_infinite_set extends AnyWordSpec with Matchers {
  "SmallestInfiniteSet" should {
    "work as expected with test case 1" in {
      val s = new SmallestInfiniteSet()
      s.popSmallest() shouldBe 1
      s.popSmallest() shouldBe 2
      s.popSmallest() shouldBe 3
      s.addBack(1)
      s.popSmallest() shouldBe 1
      s.popSmallest() shouldBe 4
      s.popSmallest() shouldBe 5
    }

    "work as expected with test case 2" in {
      val s = new SmallestInfiniteSet()
      s.popSmallest() shouldBe 1
      s.addBack(608)
      s.popSmallest() shouldBe 2
      s.addBack(4)
      s.popSmallest() shouldBe 3
    }
  }
}
