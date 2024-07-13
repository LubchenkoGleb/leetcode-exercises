package hlib.liubchenko.topinterview150.intervals

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _3_insert_interval extends AnyWordSpec with Matchers {
  def insert(intervals: Array[Array[Int]], newInterval: Array[Int]): Array[Array[Int]] = {
    if (intervals.isEmpty) return Array(newInterval)

    val (lastBefore, firstAfter) = findRangesToModify(intervals, newInterval)
    println(s"lastBefore: $lastBefore, firstAfter: $firstAfter")

    def mergeLeft(left: Array[Int], mid: Array[Int]) =
      if (left(1) + 1 < mid(0)) Array(left, mid) else Array(Array(left(0), mid(1)))

    def mergeRight(mid: Array[Int], right: Array[Int]) =
      if (mid(1) < right(0) - 1) Array(mid, right) else Array(Array(mid(0), right(1)))

    def merge(left: Array[Int], mid: Array[Int], right: Array[Int]) =
      if (left(1) + 1 < mid(0) && mid(1) < right(0) - 1) Array(left, mid, right)
      else if (left(1) + 1 >= mid(0) && mid(1) >= right(0) - 1) Array(Array(left(0), right(1)))
      else if (left(1) >= mid(0)) mergeLeft(left, mid) :+ right
      else left +: mergeRight(mid, right)

    if (firstAfter == 0) mergeLeft(newInterval, intervals(0)) ++ intervals.drop(1)
    else if (lastBefore == intervals.length - 1) intervals.dropRight(1) ++ mergeRight(intervals.last, newInterval)
    else {
      val mid = merge(intervals(lastBefore), newInterval, intervals(firstAfter))
      intervals.take(lastBefore) ++ mid ++ intervals.drop(firstAfter + 1)
    }
  }

  private def findRangesToModify(intervals: Array[Array[Int]], newInterval: Array[Int]): (Int, Int) = {
    // last index that starts before the new interval
    var lastBefore = 0
    var condition = true
    var i = 0
    while (condition && i < intervals.length) {
      if (intervals(i)(0) <= newInterval(0)) lastBefore = i
      else condition = false
      i += 1
    }

    // first index that ends after the new interval
    var firstAfter = intervals.length - 1
    condition = true
    i = intervals.length - 1
    while (condition && i > -1) {
      if (newInterval(1) <= intervals(i)(1)) firstAfter = i
      else condition = false
      i -= 1
    }

    (lastBefore, firstAfter)
  }

  "insert" should {
    "work as expected" in {
      val input = Array(Array(3, 3), Array(5, 7), Array(13, 15), Array(17, 19))

      def test(toInsert: Array[Int])(expected: (Int, Int)*) =
        insert(input, toInsert).map(_.toList).toList shouldBe expected.map { case (s, e) => List(s, e) }.toList

//      val doNotIntersectToInsert = Array(9, 11)
//      findRangesToModify(input, doNotIntersectToInsert) shouldBe (1, 2)
//      test(doNotIntersectToInsert)((3, 3), (5, 7), (9, 11), (13, 15), (17, 19))
//
//      val intersectBothToInsert = Array(6, 14)
//      findRangesToModify(input, intersectBothToInsert) shouldBe (1, 2)
//      test(intersectBothToInsert)((3, 3), (5, 15), (17, 19))
//
//      val leftIntersect = Array(6, 11)
//      findRangesToModify(input, leftIntersect) shouldBe (1, 2)
//      test(leftIntersect)((3, 3), (5, 11), (13, 15), (17, 19))
//
//      val rightIntersect = Array(9, 14)
//      findRangesToModify(input, rightIntersect) shouldBe (1, 2)
//      test(Array(9, 14))((3, 3), (5, 7), (9, 15), (17, 19))
//
//      val merge1 = Array(7, 13)
//      findRangesToModify(input, merge1) shouldBe (1, 2)
//      test(merge1)((3, 3), (5, 15), (17, 19))
//
//      val merge2 = Array(8, 12)
//      findRangesToModify(input, merge2) shouldBe (1, 2)
//      test(merge2)((3, 3), (5, 15), (17, 19))
//
//      val appendLeft = Array(0, 1)
//      findRangesToModify(input, appendLeft) shouldBe (0, 0)
//      test(appendLeft)((0, 1), (3, 3), (5, 7), (13, 15), (17, 19))
//
//      val appendMergeLeft = Array(0, 2)
//      findRangesToModify(input, appendMergeLeft) shouldBe (0, 0)
//      test(appendMergeLeft)((0, 3), (5, 7), (13, 15), (17, 19))
//
//      val appendRight = Array(21, 22)
//      findRangesToModify(input, appendRight) shouldBe (3, 3)
//      test(appendRight)((3, 3), (5, 7), (13, 15), (17, 19), (21, 22))
//
//      val appendMergeRight = Array(20, 21)
//      findRangesToModify(input, appendMergeRight) shouldBe (3, 3)
//      test(appendMergeRight)((3, 3), (5, 7), (13, 15), (17, 21))

      insert(Array(Array(1, 5)), Array(2, 3)).map(_.toList).toList shouldBe List(List(1, 5))
    }
  }
}
