package hlib.liubchenko.leetcode75.two_pointers

object _1_move_zeros extends App {
  // Runtime 21, Memory 79
  def moveZeroes(nums: Array[Int]): Unit = {
    var nonZeroCount = 0
    nums.indices.foreach { i =>
      if (nums(i) != 0) {
        if (i != nonZeroCount) {
          nums(nonZeroCount) = nums(i)
          nums(i) = 0
        }
        nonZeroCount += 1
      }
    }
  }

  // Mine Runtime 63, Memory 79
  def moveZeroes_1(nums: Array[Int]): Unit = {
    var zeroCount = 0
    (0 until (nums.length - zeroCount)).foreach { i =>
      if (nums(i) == 0) zeroCount += 1
      else if (zeroCount != 0) {
        nums(i - zeroCount) = nums(i)
        nums(i) = 0
      }
    }
  }

  // val array = Array(0, 1, 0, 3, 12)
//  val array = Array(0, 1, 0, 0, 5, 3, 0, 12)
  val array = Array(1)
  moveZeroes(array)
  println(array.mkString(", "))
}
