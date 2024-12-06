package leetcode_patterns

object _26_findAllNumbersDisappearedInAnArray extends App {
  def findDisappearedNumbers(nums: Array[Int]): List[Int] = {
    val set = nums.toSet
    (1 to nums.length).filter(i => !set.contains(i)).toList
  }

  def findDisappearedNumbers2(nums: Array[Int]): List[Int] = {
    nums.indices.foreach { i =>
      val nI = nums(i)
      if (nI > 0) {
        val nV = nums(nI - 1)
        if (nV > 0) nums(nI - 1) = -nV
      } else {
        val nV = nums(-nI - 1)
        if (nV > 0) nums(-nI - 1) = -nV
      }
    }

    nums.indices.filter(nums(_) > 0).map(_ + 1).toList
  }

  println(findDisappearedNumbers2(Array(4, 3, 2, 7, 8, 2, 3, 1)))
}
