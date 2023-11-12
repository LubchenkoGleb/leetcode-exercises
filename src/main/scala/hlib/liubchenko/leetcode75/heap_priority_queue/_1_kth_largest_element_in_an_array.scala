package hlib.liubchenko.leetcode75.heap_priority_queue

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _1_kth_largest_element_in_an_array extends AnyWordSpec with Matchers {
  sealed trait Solution {
    def findKthLargest(nums: Array[Int], k: Int): Int
  }
  // Mine, Runtime 28, Memory 94
  object Solution1 extends Solution {
    import collection.mutable

    def findKthLargest(nums: Array[Int], k: Int): Int = {
      val q = mutable.PriorityQueue.empty[Int](Ordering[Int].reverse)
      nums.foreach { i =>
        q.enqueue(i)
        if (q.size > k) q.dequeue()
      // println(s"i[$i], ${q.clone().dequeueAll}")
      }
      q.head
    }
  }

  // Mine, Runtime 45, Memory 95
  object Solution2 extends Solution {
    import collection.mutable

    def findKthLargest(nums: Array[Int], k: Int): Int = {
      val q = mutable.PriorityQueue.empty[Int](Ordering[Int].reverse)
      nums.foreach { i =>
        if (q.size < k) q.enqueue(i)
        else if (i > q.head) q.enqueue(i)

        if (q.size > k) q.dequeue()
      }
      q.head
    }
  }

  object Solution3 extends Solution {
    import collection.mutable

    def findKthLargest(nums: Array[Int], k: Int): Int =
      mutable.PriorityQueue.from(nums).dequeueAll(k - 1)
  }

  "All solutions" should {
    "work" in {
      List(Solution1, Solution2).map { solution =>
        println(solution)
        // 1, 2, 3, 4, 5, 6
        solution.findKthLargest(Array(3, 2, 1, 5, 6, 4), 2) shouldBe 5
        // 1, 2, 2, 3, 3, 4, 5, 5, 6
        solution.findKthLargest(Array(3, 2, 3, 1, 2, 4, 5, 5, 6), 4) shouldBe 4
      }
    }
  }
}
