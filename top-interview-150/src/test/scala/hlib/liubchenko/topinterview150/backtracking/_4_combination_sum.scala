package hlib.liubchenko.topinterview150.backtracking

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _4_combination_sum extends AnyWordSpec with Matchers {
  def combinationSum(candidates: Array[Int], target: Int): List[List[Int]] = {
    def loop(acc: List[Int], candidates: List[Int], target: Int): List[List[Int]] =
      if (target == 0) List(acc)
      else if (target < 0) Nil
      else {
        val maxAllowedElement = acc.lastOption.getOrElse(0)
        candidates.filter(_ >= maxAllowedElement).flatMap(i => loop(acc :+ i, candidates, target - i))
      }

    loop(Nil, candidates.sorted.toList, target)
  }

  "combinationSum" should {
    "work as expected" in {
      combinationSum(Array(2, 3, 6, 7), 7) shouldBe List(List(2, 2, 3), List(7))
    }
  }
}
