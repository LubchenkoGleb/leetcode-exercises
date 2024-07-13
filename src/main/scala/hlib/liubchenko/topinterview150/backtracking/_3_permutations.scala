package hlib.liubchenko.topinterview150.backtracking

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _3_permutations extends AnyWordSpec with Matchers {
  def permute(nums: Array[Int]): List[List[Int]] = {
    def loop(rem: Set[Int]): Set[List[Int]] =
      if (rem.size == 1) Set(rem.toList)
      else rem.flatMap(v => loop(rem - v).map(v :: _))

    loop(nums.toSet).toList
  }

  "permute" should {
    "work as expected" in {
      permute(Array(1, 2, 3, 4)) shouldBe List(
        List(1, 2, 3, 4),
        List(3, 1, 4, 2),
        List(2, 4, 1, 3),
        List(4, 1, 2, 3),
        List(3, 1, 2, 4),
        List(1, 3, 2, 4),
        List(1, 4, 3, 2),
        List(3, 2, 4, 1),
        List(2, 3, 4, 1),
        List(2, 3, 1, 4),
        List(1, 2, 4, 3),
        List(4, 3, 1, 2),
        List(2, 4, 3, 1),
        List(3, 4, 2, 1),
        List(4, 2, 3, 1),
        List(2, 1, 3, 4),
        List(1, 4, 2, 3),
        List(4, 1, 3, 2),
        List(4, 2, 1, 3),
        List(2, 1, 4, 3),
        List(3, 2, 1, 4),
        List(1, 3, 4, 2),
        List(4, 3, 2, 1),
        List(3, 4, 1, 2)
      )

    }
  }
}
