package hlib.liubchenko.topinterview150.multi_dimensional_dp

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec


class _1_triangle extends AnyWordSpec with Matchers {
  def minimumTotal(triangle: List[List[Int]]): Int = {
    import scala.annotation.tailrec

    def bestPrev(i: Int, prev: List[Int]): Int =
      if (i == 0) prev.head
      else if (i == prev.length) prev.last
      else math.min(prev(i - 1), prev(i))

    @tailrec
    def loop(curr: List[List[Int]], prev: List[Int]): Int =
      if (curr.isEmpty) prev.min
      else {
        val newCurr = curr.head.zipWithIndex.map { case (v, i) => bestPrev(i, prev) + v }
        loop(curr.tail, newCurr)
      }

    loop(triangle.tail, triangle.head)
  }

  "minimumTotal" should {
    "work as expected" in {
      minimumTotal(List(List(2), List(3, 4), List(6, 5, 7), List(4, 1, 8, 3))) shouldBe 11
    }
  }
}
