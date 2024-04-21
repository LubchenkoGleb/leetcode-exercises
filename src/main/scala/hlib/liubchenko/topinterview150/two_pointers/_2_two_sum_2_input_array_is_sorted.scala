package hlib.liubchenko.topinterview150.two_pointers

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _2_two_sum_2_input_array_is_sorted extends AnyWordSpec with Matchers {
  // println(s"1st Loop - i: $i, j: $j, nums($i): ${numbers(i)}, nums($j): ${numbers(j)}, ")
  // println(s"2nd Loop - i: $i, j: $j, nums($i): ${numbers(i)}, nums($j): ${numbers(j)}, ")

  def twoSum_2(numbers: Array[Int], target: Int): Array[Int] = {
    var (i, j) = (0, 1)
    val res = Array(0, 1)

    def sum = numbers(i) + numbers(j)
    def update(): Unit = { res(0) = i; res(1) = j }

    while (i < numbers.length - 1 && sum < target) {
      while (j < numbers.length - 1 && sum < target) {
        j += 1
        update()
      }
      if (sum != target) {
        i += 1; j = i + 1
        update()
      }
    }

    res.map(_ + 1)
  }

  def twoSum(numbers: Array[Int], target: Int): Array[Int] = {
    import scala.annotation.tailrec

    @tailrec
    def loop(s: Int, e: Int): Array[Int] = {
      val sum = numbers(s) + numbers(e)
      if (sum > target) loop(s, e - 1)
      else if (sum < target) loop(s + 1, e)
      else Array(s + 1, e + 1)
    }

    loop(0, numbers.length - 1)
  }

  "twoSum" should {
    "work as expected" in {
      twoSum(Array(1, 2, 3, 5, 8), 8).toList shouldEqual List(3, 4)
      twoSum(Array(2, 7, 11, 15), 9).toList shouldEqual List(1, 2)
      twoSum(Array(2, 3, 4), 6).toList shouldEqual List(1, 3)
      twoSum(Array(5, 25, 75), 100).toList shouldEqual List(2, 3)
    }
  }
}
