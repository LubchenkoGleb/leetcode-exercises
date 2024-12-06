package leetcode_patterns

object _4_maximum69Number extends App {
  def maximum69Number (num: Int): Int = {
    num.toString.replaceFirst("6", "9").toInt
  }
}
