package hlib.liubchenko.topinterview150.backtracking

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _6_generate_parentheses extends AnyWordSpec with Matchers {
  def generateParenthesis(n: Int): List[String] = ???

  "generateParenthesis" should {
    "work as expected" in {
      generateParenthesis(3) shouldBe List("((()))", "(()())", "(())()", "()(())", "()()()")
      generateParenthesis(1) shouldBe List("()")
    }
  }
}
