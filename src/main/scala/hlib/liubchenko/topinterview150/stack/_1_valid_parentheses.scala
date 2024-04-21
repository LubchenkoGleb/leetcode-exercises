package hlib.liubchenko.topinterview150.stack

import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.wordspec.AnyWordSpec

class _1_valid_parentheses extends AnyWordSpec with Matchers {
  def isValid(s: String): Boolean = {
    import scala.annotation.tailrec
    import scala.collection.mutable

    val stack = mutable.Stack.empty[Char]
    def isOpenBracket(i: Int) = s(i) == '(' || s(i) == '{' || s(i) == '['
    def isValidCloseBracket(open: Char, close: Char): Boolean = (open, close) match {
      case ('(', ')') | ('{', '}') | ('[', ']') => true
      case _                                    => false
    }

    @tailrec
    def loop(i: Int): Boolean =
      if (i == s.length) stack.isEmpty
      else if (isOpenBracket(i)) { stack.push(s(i)); loop(i + 1) }
      else if (stack.nonEmpty && isValidCloseBracket(stack.pop(), s(i))) loop(i + 1)
      else false

    loop(0)
  }

  "isValid" should {
    "work as expected" in {
      Table(
        ("input", "expected"),
        ("()", true),
        ("()[]{}", true),
        ("(]", false),
        ("[", false),
        ("]", false)
      ) { (input, expected) =>
        isValid(input) shouldBe expected
      }
    }
  }
}
