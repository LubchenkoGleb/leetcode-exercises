package hlib.liubchenko.leetcode75.dp_multidimentional

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _2_longest_common_subsequence extends AnyWordSpec with Matchers {

  // Not my, Runtime 86, Memory 52
  def longestCommonSubsequence(text1: String, text2: String): Int = {
    if (text1.equals(text2)) return text1.length

    val dp = Array.ofDim[Int](text1.length, text2.length)
    def dpValue(i: Int, j: Int) = if (i >= 0 && j >= 0) dp(i)(j) else 0

    for (i <- text1.indices; j <- text2.indices)
      if (text1(i) == text2(j)) dp(i)(j) = dpValue(i - 1, j - 1) + 1
      else dp(i)(j) = Math.max(dpValue(i, j - 1), dpValue(i - 1, j))

    dp.last.last
  }

  def longestCommonSubsequence_5(text1: String, text2: String): Int = {

    if (text1.equals(text2)) return text1.length

    val dp = Array.ofDim[Int](text1.length + 1, text2.length + 1)

    for (i <- 0 until text1.length) {
      for (j <- 0 until text2.length) {
        if (text1(i) == text2(j)) dp(i + 1)(j + 1) = dp(i)(j) + 1
        else dp(i + 1)(j + 1) = Math.max(dp(i + 1)(j), dp(i)(j + 1))

        println(s"i: $i, j: $j, arr1(i): ${text1(i)}, arr2(j): ${text2(j)}")
        println(dp.map(_.mkString(",")).mkString("\n") + "\n")
      }
    }

    dp.last.last
  }

  def longestCommonSubsequence_4(text1: String, text2: String): Int = {

    if (text1.equals(text2)) return text1.length

    val dp = Array.ofDim[Int](text1.length + 1, text2.length + 1)

    for (i <- dp.length - 2 to 0 by -1) {
      for (j <- dp.head.length - 2 to 0 by -1) {
        //if they are equal, go diagonally plus 1
        if (text1(i) == text2(j)) dp(i)(j) = dp(i + 1)(j + 1) + 1
        //if not equal, pick the max of left or under
        else dp(i)(j) = Math.max(dp(i)(j + 1), dp(i + 1)(j))

        println(s"i: $i, j: $j, arr1(i): ${text1(i)}, arr2(j): ${text2(j)}")
        println(dp.map(_.mkString(",")).mkString("\n") + "\n")
      }
    }

    dp.head.head
  }

  // Runtime 8, Memory 56
  def longestCommonSubsequence_3(text1: String, text2: String): Int = {
    val text2LettersIndexes = text2.filter(text1.contains(_)).zipWithIndex.groupMap(_._1)(_._2)

    val text2MatchingLettersCount = text2LettersIndexes.valuesIterator.map(_.length).sum
    val res = new Array[Int](text2MatchingLettersCount)

    text1.flatMap(text2LettersIndexes.get).foreach { t2Indexes =>
//      t2Indexes.map(t2I => t2I -> (1 + res.take(t2I).maxOption.getOrElse(0))).foreach { case (i, v) => res(i) = v }
      t2Indexes
        .map { t2I => t2I -> (1 + (0 until t2I).map(res(_)).maxOption.getOrElse(0)) }
        .foreach { case (i, v) => res(i) = v }
//      println(s"res: ${res.mkString(", ")}")
    }

    res.maxOption.getOrElse(0)
  }

  // Memory limit
  def longestCommonSubsequence_2(text1: String, text2: String): Int = {
    val text2LettersIndexes = text2.filter(text1.contains(_)).zipWithIndex.groupMap(_._1)(_._2)

    val text2MatchingLettersCount = text2LettersIndexes.valuesIterator.map(_.length).sum
    val res = new Array[Int](text2MatchingLettersCount)

    text1.flatMap(text2LettersIndexes.get).foreach { t2Indexes =>
      t2Indexes
        .map { t2I => t2I -> (1 + (0 until t2I).map(res(_)).maxOption.getOrElse(0)) }
        .foreach { case (i, v) => res(i) = v }
    }

    res.maxOption.getOrElse(0)
  }

  // doesn't work
  def longestCommonSubsequence_1(text1: String, text2: String): Int = {
    def loop(s1: String, s2: String): Int = {
      println(s"s1: $s1, s2: $s2")
      if (s1.isEmpty || s2.isEmpty) 0
      else if (s1.head == s2.head) 1 + loop(s1.tail, s2.tail)
      else {
        val s1HeadInS2Index = s2.indexOf(s1.head)
        val s2HeadInS1Index = s1.indexOf(s2.head)

        if (s1HeadInS2Index == -1) loop(s1.tail, s2)
        else if (s2HeadInS1Index == -1) loop(s1, s2.tail)
        else if (s2.length - s1HeadInS2Index >= s1.length - s2HeadInS1Index) loop(s1, s2.drop(s1HeadInS2Index))
        else loop(s1.drop(s2HeadInS1Index), s2)
      }
    }

    loop(text1, text2)
  }

  "longestCommonSubsequence" should {
    "work as expected" in {
//      longestCommonSubsequence("abcde", "ace") shouldBe 3 // ace
//      longestCommonSubsequence("abcde", "afce") shouldBe 3 // ace
//      longestCommonSubsequence("abcdef", "afce") shouldBe 3 // ace
//      longestCommonSubsequence("abc", "abc") shouldBe 3
//      longestCommonSubsequence("abc", "def") shouldBe 0
//      longestCommonSubsequence("bsbininm", "jmjkbkjkv") shouldBe 1 // b
//      longestCommonSubsequence("oxcpqrsvwf", "shmtulqrypy") shouldBe 2 // qr
      longestCommonSubsequence("afbca", "abcfa") shouldBe 4 // abca
//      longestCommonSubsequence("abcba", "abcbcba") shouldBe 5 // abca

    }
  }
}
