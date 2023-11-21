package hlib.liubchenko.leetcode75.intervals

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _1_non_overlapping_intervals extends AnyWordSpec with Matchers {
  // not mine, Runtime 87, memory 25
  def eraseOverlapIntervals(intervals: Array[Array[Int]]): Int = {
    var (result, previousLast) = (0, Integer.MIN_VALUE)

    for (a <- intervals.sortBy(_(1))) {
      if (a.head < previousLast) result += 1
      else previousLast = a.last
      println(s"a[${a.mkString(", ")}] result: $result, previousLast: $previousLast")
    }

    result
  }

  // memory limit
  def eraseOverlapIntervals_2(intervals: Array[Array[Int]]): Int = {
    val acc = scala.collection.mutable.SortedMap.empty[Int, Int]

    intervals.map { case Array(f, t) => (f, t) }.sortBy(_._1).foreach { case (f, t) =>
      acc.updateWith(t) { currMb =>
        val curr = currMb.getOrElse(0)
        val maxBefore = acc.rangeTo(f).values.maxOption.getOrElse(0)
        val res = math.max(curr, maxBefore + 1)
        // println(s"interval[$f, $t] curr: $curr, maxBefore: $maxBefore, res: $res")
        Some(res)
      }
    // println(s"after update: $acc")
    }

    val longest = acc.values.maxOption.getOrElse(0)
    // println(s"acc: $acc")
    // println(s"longest: $longest")
    intervals.length - longest
  }

  import scala.collection.immutable.SortedMap

  // memory limit
  def eraseOverlapIntervals_1(intervals: Array[Array[Int]]): Int = {
    val iMap = intervals.map { case Array(f, t) => (f, t) }.groupMap(_._1)(_._2).to(SortedMap)

    def length(i: Int, acc: Int): Int = iMap
      .iteratorFrom(i)
      .flatMap(_._2)
      .toSet
      .map { next => length(next, acc + 1) }
      .maxOption
      .getOrElse(acc)

    intervals.length - intervals.map { case Array(_, t) => length(t, 1) }.max
  }

  "eraseOverlapIntervals" should {
    "work as expected" in {
      eraseOverlapIntervals(Array(Array(1, 3), Array(2, 3), Array(3, 4), Array(1, 2))) shouldBe 1
//      eraseOverlapIntervals(Array(Array(1, 2), Array(2, 3))) shouldBe 0
//      eraseOverlapIntervals(Array(Array(1, 100), Array(11, 22), Array(1, 11), Array(2, 12))) shouldBe 2
//      eraseOverlapIntervals(Array(Array(0, 1), Array(3, 4), Array(1, 2))) shouldBe 0
//      eraseOverlapIntervals(Array(Array(1, 2), Array(2, 3), Array(3, 4), Array(-100, -2), Array(5, 7))) shouldBe 0
//      eraseOverlapIntervals(
//        Array(
//          Array(-52, 31),
//          Array(-73, -26),
//          Array(82, 97),
//          Array(-65, -11),
//          Array(-62, -49),
//          Array(95, 99),
//          Array(58, 95),
//          Array(-31, 49),
//          Array(66, 98),
//          Array(-63, 2),
//          Array(30, 47),
//          Array(-40, -26)
//        )
//      ) shouldBe 7
    }
  }
}
