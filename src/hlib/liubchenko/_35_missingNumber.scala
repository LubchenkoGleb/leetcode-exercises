package hlib.liubchenko

object _35_missingNumber extends App {
  def missingNumber(nums: Array[Int]): Int = {
    val ordered = Array.fill(nums.length + 1)(-1)

    nums.indices.foreach { i =>
      ordered(nums(i)) = 1
    }

    ordered.indices.find(i => ordered(i) == -1).getOrElse(nums.length)
  }

  def missingNumber2(nums: Array[Int]): Int = {
    val n = nums.length
    var sum = (n * (n + 1)) / 2

    nums.foreach(sum -= _)

    sum
  }

  println(missingNumber(Array(3, 0, 1)))  // 2
  println(missingNumber(Array(3, 2, 0))) // 1
  println(missingNumber(Array(9, 6, 4, 2, 3, 5, 7, 0, 1)))  // 8
  println(missingNumber(Array(9, 6, 4, 2, 3, 5, 8, 0, 1)))  // 7
  println(missingNumber(Array(6, 9, 4, 2, 3, 5, 8, 0, 1)))  // 7
}
