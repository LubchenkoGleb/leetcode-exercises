package hlib.liubchenko.topinterview150.intervals

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.mutable

class _2_merge_intervals extends AnyWordSpec with Matchers {
  def merge(intervals: Array[Array[Int]]): Array[Array[Int]] = {
    intervals
      .sortBy(_.head)
      .foldLeft(mutable.Stack.empty[Array[Int]]) {
        case (acc, interval) if acc.isEmpty => acc.push(interval)
        case (acc, interval @ Array(from, to)) =>
          val Array(accFrom, accTo) = acc.head
          if (accFrom <= from && from <= accTo) {
            acc.pop()
            acc.push(Array(accFrom, math.max(accTo, to)))
          } else acc.push(interval)
      }
      .toArray
  }

  "merge" should {
    "work as expected" in {
      merge(Array(Array(1, 3), Array(2, 6), Array(8, 10), Array(15, 18))).map(_.toList).toList should
        contain theSameElementsAs List(List(1, 6), List(8, 10), List(15, 18))
    }
  }
}
