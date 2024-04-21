package hlib.liubchenko.leetcode_patterns

object _3_splitAStringInBalancedStrings extends App {
  def balancedStringSplit(s: String): Int = {
    s.foldLeft((0, 0, 0)) { case ((lCount, rCount, res), next) =>
      val newLCount = if(next == 'L') lCount + 1 else lCount
      val newRCount = if(next == 'R') rCount + 1 else rCount

      if(newLCount == newRCount) (0, 0, res + 1)
      else (newLCount, newRCount, res)
    }._3
  }

  List(
    "RLRRLLRLRL",
    "RLLLLRRRLR",
    "LLLLRRRR",
    "RLRRRLLRLL"
  ).map(balancedStringSplit).foreach(println)
}
