package hlib.liubchenko.hackerrank

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.annotation.tailrec

class _4_minimumBribes extends AnyWordSpec with Matchers {
  def minimumBribes(q: Array[Int]): Int = {
    // Write your code here
    def swap(i: Int, j: Int): Unit = { val tmp = q(i); q(i) = q(j); q(j) = tmp }

    @tailrec
    def loop(i: Int, acc: Int): Int = {
      if (acc == -1) acc
      else if (i == 0) acc
      else if (i + 1 == q(i)) loop(i - 1, acc)
      else if (i + 1 == q(i - 1)) {
        swap(i, i - 1); loop(i - 1, acc + 1)
      } else if (i + 1 == q(i - 2)) {
        swap(i - 1, i - 2); swap(i, i - 1); loop(i - 1, acc + 2)
      } else -1
    }

    val res = loop(q.length - 1, 0)
    //    if (res == -1) println("Too chaotic") else println(res)
    res
  }

  "minimumBribes" should {
    "work as expected" in {
      minimumBribes(Array(1, 2, 5, 3, 7, 8, 6, 4)) shouldEqual 7
      // 1, 2, 5, 3, 7, 8, 6, 4
      // 1, 2, 5, 3, 7, 6, 4, 8 - 2
      // 1, 2, 5, 3, 6, 4, 7, 8 - 4
      // 1, 2, 5, 3, 4, 6, 7, 8 - 5
      // 1, 2, 3, 4, 5, 6, 7, 8 - 7
    }
  }
}
