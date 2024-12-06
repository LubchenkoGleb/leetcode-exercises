package hlib.liubchenko.topinterview150.intervals

import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.wordspec.AnyWordSpec

class _1_summary_ranges extends AnyWordSpec with Matchers {
  def summaryRanges(nums: Array[Int]): List[String] = {
    def loop(intervalStart: Int, currIndx: Int): List[String] = {
      def createValue = if (intervalStart == nums(currIndx - 1)) intervalStart.toString
      else s"$intervalStart->${nums(currIndx - 1)}"

      if (currIndx == nums.length) List(createValue)
      else if (nums(currIndx - 1) + 1 == nums(currIndx)) loop(intervalStart, currIndx + 1)
      else createValue :: loop(nums(currIndx), currIndx + 1)
    }

    if (nums.isEmpty) List.empty else loop(nums(0), 1)
  }

  "summaryRanges" should {
    "work as expected" in {
      Table(
        ("input", "expected"),
        (Array(0, 1, 2, 4, 5, 7), List("0->2", "4->5", "7")),
        (Array(0, 2, 3, 4, 6, 8, 9), List("0", "2->4", "6", "8->9"))
      ) { (input, expected) =>
        summaryRanges(input) shouldBe expected
      }
    }
  }
}
