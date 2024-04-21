package hlib.liubchenko.leetcode_patterns

object _23_moveZeros extends App {
  def moveZeroes(nums: Array[Int]): Unit = {
    var zerosInARow = 0
    var i = 0
    var prevZero = false

    while (i < nums.length) {
      if (nums(i) == 0) {
        zerosInARow += 1
        prevZero = true
      } else if (prevZero) {
        nums(i - zerosInARow) = nums(i)
        nums(i) = 0
      }
      if (nums(i) != 0) prevZero = false
      i += 1
    }
  }

  def moveZeroes2(nums: Array[Int]): Unit = {
    var lastNonZero = 0
    nums.indices.foreach { i =>
      if (nums(i) != 0) {
        val tmp = nums(lastNonZero)
        nums(lastNonZero) = nums(i)
        nums(i) = tmp
        lastNonZero += 1
      }
    }
  }

  val input = Array(0, 1, 0, 3, 12)
  moveZeroes2(input)
  println(input.toList)
}
