package hlib.liubchenko.leetcode_patterns

object _8_uniqueNumberOfOccurrences extends App {
  def uniqueOccurrences(arr: Array[Int]): Boolean = {
    val grouped = arr.groupBy(identity).view.mapValues(_.length)
    grouped.keys.size == grouped.values.toSet.size
  }
}
