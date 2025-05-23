package hlib.liubchenko.adventofcode._2024.day1

import hlib.liubchenko.adventofcode._2024.Utils
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.io.Source

class Challenge2 extends AnyWordSpec with Matchers {
  def findSimilarityScore(lines: List[String]): Long = {
    val (l1, l2) = lines.map(_.split("[ \\t]+")).map { case Array(a, b) => a.toInt -> b.toInt }.unzip
    val l2Map = l2.groupMapReduce(n => n)(_ => 1)(_ + _)
    l1.map(n => l2Map.get(n).map(n * _).getOrElse(0)).sum
  }

  "Day #1 Challenge #2" should {
    "work as expected #1" in {
      findSimilarityScore(
        List(
          "3   4",
          "4   3",
          "2   5",
          "1   3",
          "3   9",
          "3   3"
        )
      ) shouldBe 31
    }

    "work as expected #2" in {
      val input = Utils.readInputFile(1)
      findSimilarityScore(input) shouldBe 18805872
    }
  }
}
