package hlib.liubchenko.leetcode75.stack

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _1_removing_stars_from_a_string extends AnyWordSpec with Matchers {

  // Mine, Runtime 62, Memory 75
  def removeStars(s: String): String = {
    import scala.collection.mutable

    val stack = mutable.Stack.empty[Char]
    s.foreach {
      case '*' => stack.pop()
      case v   => stack.push(v)
    }
    stack.reverse.mkString
  }

  "removeStars" should {
    "work as expected" in {
      removeStars("leet**cod*e") shouldBe "lecoe"
      removeStars("erase*****") shouldBe ""
    }
  }
}
