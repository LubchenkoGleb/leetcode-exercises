package hlib.liubchenko.topinterview150.array_string

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _11_h_Index extends AnyWordSpec with Matchers {
  def hIndex(citations: Array[Int]): Int = {
    citations
      .sorted(Ordering[Int].reverse)
      .zipWithIndex
      .collectFirst { case (c, i) if c <= i => i }
      .getOrElse(citations.length)
  }

  "hIndex" should {
    "work as expected" in {
      hIndex(Array(6, 5, 3, 1, 0)) shouldBe 3
      hIndex(Array(1, 3, 1)) shouldBe 1
      hIndex(Array(1)) shouldBe 1
    }
  }
}
