package hlib.liubchenko.topinterview150.backtracking

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _2_combinations extends AnyWordSpec with Matchers {
  def combine(n: Int, k: Int): List[List[Int]] = {
    import scala.collection.mutable

    def generateCombination(start: Int, elementsToAdd: Int, combination: mutable.ListBuffer[Int]): List[List[Int]] =
      if (elementsToAdd == 0) List(combination.toList)
      else
        (start to n - elementsToAdd + 1).flatMap { i =>
          combination.append(i)
          val res = generateCombination(i + 1, elementsToAdd - 1, combination)
          combination.dropRightInPlace(1)
          res
        }.toList

    generateCombination(1, k, mutable.ListBuffer.empty[Int])
  }

  "combine" should {
    "work as expected" in {
      println(combine(4, 3))
    }
  }
}
