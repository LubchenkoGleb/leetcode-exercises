package hlib.liubchenko.leetcode_patterns

object _13_reverseString extends App {
  def reverseString(s: Array[Char]): Unit = {
    (0 until s.length / 2).foreach { i =>
      val tmp = s(s.length - i - 1)
      s(s.length - i - 1) = s(i)
      s(i) = tmp
    }
  }

//  val input = "hello".toCharArray
  val input = Array(
    "A",
    " ",
    "m",
    "a",
    "n",
    ",",
    " ",
    "a",
    " ",
    "p",
    "l",
    "a",
    "n",
    ",",
    " ",
    "a",
    " ",
    "c",
    "a",
    "n",
    "a",
    "l",
    ":",
    " ",
    "P",
    "a",
    "n",
    "a",
    "m",
    "a"
  ).map(_.charAt(0))
  println(input.mkString(""))

  reverseString(input)

  println(input.mkString(""))
}
