package hlib.liubchenko.leetcode75.stack

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.mutable

class _3_decode_string extends AnyWordSpec with Matchers {
  def decodeString(s: String): String = {
    val stack = mutable.Queue.empty[Char]
    for (c <- s) if (c == ']') {
      val letters = stack.dequeueWhile(_ != '[').mkString
      stack.dequeue()
      val digit = stack.dequeueWhile(_.isDigit).mkString.toInt
      stack.enqueueAll(letters * digit)
    } else stack.enqueue(c)
    stack.reverse.mkString
  }

  def decodeString_2(s: String): String = {
    val stack = mutable.Stack.empty[String]
    for (c <- s) {
      if (stack.isEmpty || c == '[') stack.push(c.toString)
      else if (c.isLetter) { if (stack.head.head.isLetter) stack.push(stack.pop() + c) else stack.push(c.toString) }
      else if (c.isDigit) { if (stack.head.head.isDigit) stack.push(stack.pop() + c) else stack.push(c.toString) }
      else {
        val letters = stack.popWhile(_ != "]").reverse.mkString
        val digit = stack.pop().toInt
        stack.push(letters * digit)
      }
    }
    stack.reverse.mkString
  }

  "decodeString" should {
    "work as expected" in {
      decodeString("3[a]2[bc]") shouldBe "aaabcbc"
      decodeString("3[a2[c]]") shouldBe "accaccacc"
      decodeString("2[abc]3[cd]ef") shouldBe "abcabccdcdcdef"
    }
  }
}
