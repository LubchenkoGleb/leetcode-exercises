package hlib.liubchenko.leetcode75.array_string

import scala.annotation.tailrec

object _1_merge_strings_alternately extends App {

  // Runtime 93, Memory 40
  def mergeAlternately(word1: String, word2: String): String = {
    val sb = new scala.collection.mutable.StringBuilder()

    val firstIterator = word1.iterator
    val secondIterator = word2.iterator

    while (firstIterator.hasNext || secondIterator.hasNext) {
      if (firstIterator.hasNext) sb.append(firstIterator.next())
      if (secondIterator.hasNext) sb.append(secondIterator.next())
    }

    sb.result()
  }

  // Runtime 45, Memory 25
  def mergeAlternately_5(word1: String, word2: String): String = {
    word1.zipAll(word2, "", "").flatMap { case (x, y) => Seq(x, y) }.mkString
  }

  // Runtime 17, Memory 10
  def mergeAlternately_4(word1: String, word2: String): String = {
    word1.zipAll(word2, "", "").map { case (x, y) => s"$x$y" }.mkString
  }

  // Mine, Runtime 93, Memory 40
  def mergeAlternately_3(word1: String, word2: String): String = {
    val builder = new StringBuilder()

    (0 until math.max(word1.length, word2.length)).foreach { i =>
      if (i < word1.length) builder.append(word1.charAt(i))
      if (i < word2.length) builder.append(word2.charAt(i))
    }

    builder.result()
  }

  // Mine, Runtime 58, Memory 49
  def mergeAlternately_2(word1: String, word2: String): String = {
    val builder = new StringBuilder()

    @tailrec
    def loop(w1: String, w2: String): String = if (w1.isEmpty && w2.isEmpty) builder.mkString
    else {
      w1.headOption.foreach(builder.append)
      loop(w2, w1.tail)
    }

    loop(word1, word2)
  }

  // Mine, Runtime 58, Memory 6
  def mergeAlternately_1(word1: String, word2: String): String = {
    @tailrec
    def loop(w1: String, w2: String, acc: List[Char]): List[Char] = if (w1.isEmpty && w2.isEmpty) acc
    else loop(w2, w1.tail, acc ++ w1.headOption)

    loop(word1, word2, Nil).mkString
  }

  assert(mergeAlternately("abc", "pqr") == "apbqcr")
  assert(mergeAlternately("ab", "pqrs") == "apbqrs")
}
