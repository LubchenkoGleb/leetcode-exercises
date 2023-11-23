package hlib.liubchenko.leetcode75.two_pointers

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _3_container_with_most_water extends AnyWordSpec with Matchers {

  // not my idea but my implementation, Runtime 88, memory 29
  def maxArea(height: Array[Int]): Int = {
    import scala.annotation.tailrec

    def vol(i: Int, j: Int) = math.min(height(i), height(j)) * (j - i)

    @tailrec
    def loop(l: Int, r: Int, volMax: Int): Int = if (l < r) {
      val nMax = math.max(volMax, vol(l, r))
      if (height(l) < height(r)) loop(l + 1, r, nMax) else loop(l, r - 1, nMax)
    } else volMax

    loop(0, height.length - 1, 0)
  }

  def maxArea_2(height: Array[Int]): Int = {
    def vol(i: Int, j: Int) = math.min(height(i), height(j)) * (j - i)
    var (l, r, max) = (0, 1, vol(0, 1))

    // println(s"before: l:$l, r:$r, max:$max")
    for (i <- (r + 1) until height.length) {
      val lVol = vol(l + 1, r)
      if (lVol > max) { l += 1; max = lVol }
      val rVol = vol(l, i)
      if (rVol >= max) { r += 1; max = rVol }
      println(s"i[$i] after: lVol: $lVol, rVol: $rVol, l:$l, r:$r, max:$max")
    }
    max
  }

  // Mine, correct but memory limit
  def maxArea_1(height: Array[Int]): Int = {
    var max = 0
    for (i <- height.indices) {
      for (j <- i + 1 until height.length) {
        val v = math.min(height(i), height(j)) * (j - i)
        if (v > max) max = v
      }
    }
    max
  }

  "maxArea" should {
    "work as expected" in {
      maxArea(Array(1, 8, 6, 2, 5, 4, 8, 3, 7)) shouldBe 49
      maxArea(Array(1, 2, 4, 3)) shouldBe 4
      maxArea(Array(1, 1)) shouldBe 1
    }
  }
}
