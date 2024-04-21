package hlib.liubchenko.topinterview150.array_string

class _5_majority_element {
  def majorityElement(nums: Array[Int]): Int = nums
    .foldLeft((nums(0), 0)) { case ((accValue, accCount), element) =>
      if (accCount == 0) (element, 1)
      else if (accValue == element) (accValue, accCount + 1)
      else (accValue, accCount - 1)
    }
    ._1
}
