package hlib.liubchenko.topinterview150.backtracking

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _6_generate_parentheses extends AnyWordSpec with Matchers {
  def generateParenthesis(n: Int): List[String] = {
    def when(cond: => Boolean, value: => List[String]) = if (cond) value else Nil

    def loop(lRem: Int, rRem: Int, acc: String): List[String] =
      when(lRem == 0 && rRem == 0, List(acc)) ++
        when(lRem > 0, loop(lRem - 1, rRem, acc + '(')) ++
        when(lRem < rRem, loop(lRem, rRem - 1, acc + ')'))

    loop(n, n, "")
  }

  "generateParenthesis" should {
    "work as expected" in {
      generateParenthesis(1) shouldBe List("()")
      generateParenthesis(2) should contain theSameElementsAs List("()()", "(())")
      generateParenthesis(3) should contain theSameElementsAs List("((()))", "(()())", "(())()", "()(())", "()()()")
      generateParenthesis(4).sorted should contain theSameElementsAs List(
        "(((())))",
        "((()()))",
        "((())())",
        "((()))()",
        "(()(()))",
        "(()()())",
        "(()())()",
        "(())(())",
        "(())()()",
        "()((()))",
        "()(()())",
        "()(())()",
        "()()(())",
        "()()()()"
      ).sorted

      generateParenthesis(5).sorted should contain theSameElementsAs List(
        "((((()))))",
        "(((()())))",
        "(((())()))",
        "(((()))())",
        "(((())))()",
        "((()(())))",
        "((()()()))",
        "((()())())",
        "((()()))()",
        "((())(()))",
        "((())()())",
        "((())())()",
        "((()))(())",
        "((()))()()",
        "(()((())))",
        "(()(()()))",
        "(()(())())",
        "(()(()))()",
        "(()()(()))",
        "(()()()())",
        "(()()())()",
        "(()())(())",
        "(()())()()",
        "(())((()))",
        "(())(()())",
        "(())(())()",
        "(())()(())",
        "(())()()()",
        "()(((())))",
        "()((()()))",
        "()((())())",
        "()((()))()",
        "()(()(()))",
        "()(()()())",
        "()(()())()",
        "()(())(())",
        "()(())()()",
        "()()((()))",
        "()()(()())",
        "()()(())()",
        "()()()(())",
        "()()()()()"
      ).sorted
    }
  }
}
