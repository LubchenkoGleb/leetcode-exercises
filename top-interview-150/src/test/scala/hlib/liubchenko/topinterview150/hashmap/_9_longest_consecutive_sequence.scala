package hlib.liubchenko.topinterview150.hashmap

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _9_longest_consecutive_sequence extends AnyWordSpec with Matchers {
  import scala.annotation.tailrec

  def longestConsecutive(nums: Array[Int]): Int = {
    val set = nums.toSet

    @tailrec
    def check(element: Int, acc: Int = 1): Int = if (set.contains(element + 1)) check(element + 1, acc + 1) else acc

    // Shorter FP Solution but worth performance
    // set.filterNot(n => set.contains(n - 1)).map(check(_)).maxOption.getOrElse(0)

    var max = 0
    set.foreach { n => if (!set.contains(n - 1)) max = math.max(max, check(n)) }
    max
  }

  "longestConsecutive" should {
    "work as expected" in {
      longestConsecutive(Array(100, 4, 200, 1, 3, 2, 5)) shouldBe 5
    }

  }
}
