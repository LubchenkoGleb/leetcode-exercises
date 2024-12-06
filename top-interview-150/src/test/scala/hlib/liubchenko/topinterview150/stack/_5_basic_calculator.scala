package hlib.liubchenko.topinterview150.stack

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _5_basic_calculator extends AnyWordSpec with Matchers {
  def calculate(s: String): Int = {
    import scala.collection.mutable

    val stack = mutable.Stack.empty[String]

    def solveSimple(expr: collection.Seq[String]) = expr
      .sliding(2, 2)
      .map {
        case d :: "+" :: Nil => d.toInt
        case d :: "-" :: Nil => -d.toInt
        case d :: Nil        => d.toInt
      }
      .sum

    s.foreach {
      case c if Set('+', '-', '(').contains(c) => stack.push(c.toString)
      case c if c.isDigit =>
        if (stack.nonEmpty && stack.head.toIntOption.isDefined) { stack.push(stack.pop() + c) }
        else stack.push(c.toString)
      case ' ' => ()
      case ')' =>
        val simple = stack.popWhile(_ != "(")
        stack.pop()
        val res = solveSimple(simple)
        stack.push(res.toString)
    }

    solveSimple(stack.popAll())
  }

  "calculate" should {
    "work as expected" in {
      calculate("1 + 1") shouldBe 2
      calculate("(1+(4+5+2)-3)+(6+8)") shouldBe 23
      calculate("2147483647") shouldBe 2147483647
    }
  }
}
