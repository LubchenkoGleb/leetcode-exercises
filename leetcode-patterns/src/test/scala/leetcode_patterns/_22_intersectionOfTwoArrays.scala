package leetcode_patterns

object _22_intersectionOfTwoArrays extends App {
  def withScalaCheatsIntersection(
      nums1: Array[Int],
      nums2: Array[Int]
  ): Array[Int] = {
    nums1.toSet.intersect(nums2.toSet).toArray
  }

  def withScalaCheatsIntersectio2(
      nums1: Array[Int],
      nums2: Array[Int]
  ): Array[Int] = {
    nums1.intersect(nums2).distinct
  }

  def intersection(
      nums1: Array[Int],
      nums2: Array[Int]
  ): Array[Int] = {
    val set1 = nums1.toSet
    val set2 = nums2.toSet
    val res =
      if (set2.size < set1.size) set2.filter(set1.contains)
      else set1.filter(set2.contains)
    res.toArray
  }

  println(withScalaCheatsIntersection(Array(1, 2, 2, 1), Array(2, 2)).toList)
}
