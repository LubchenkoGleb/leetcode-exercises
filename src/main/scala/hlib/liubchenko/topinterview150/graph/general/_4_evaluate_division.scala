package hlib.liubchenko.topinterview150.graph.general

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _4_evaluate_division extends AnyWordSpec with Matchers {
  def calcEquation(equations: List[List[String]], values: Array[Double], queries: List[List[String]]): Array[Double] =
    ???

  "calcEquation" should {
    "work as expected #1" in {
      calcEquation(
        equations = List(
          List("a", "b"),
          List("b", "c")
        ),
        values = Array(2.0, 3.0),
        queries = List(
          List("a", "c"),
          List("b", "a"),
          List("a", "e"),
          List("a", "a"),
          List("x", "x")
        )
      ) shouldBe Array(6.00000, 0.50000, -1.00000, 1.00000, -1.00000)
    }

    "work as expected #2" in {
      calcEquation(
        equations = List(
          List("a", "b"),
          List("b", "c"),
          List("bc", "cd")
        ),
        values = Array(1.5, 2.5, 5.0),
        queries = List(
          List("a", "c"),
          List("c", "b"),
          List("bc", "cd"),
          List("cd", "bc")
        )
      ) shouldBe Array(3.75000, 0.40000, 5.00000, 0.20000)
    }

    "work as expected #3" in {
      calcEquation(
        equations = List(
          List("a", "b")
        ),
        values = Array(0.5),
        queries = List(
          List("a", "b"),
          List("b", "a"),
          List("a", "c"),
          List("x", "y")
        )
      ) shouldBe Array(0.50000, 2.00000, -1.00000, -1.00000)
    }
  }
}
