package hlib.liubchenko.leetcode75.backtracking

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _1_letter_combinations_of_a_phone_number extends AnyWordSpec with Matchers {
  private val symbols = Map(
    '2' -> List('a', 'b', 'c'),
    '3' -> List('d', 'e', 'f'),
    '4' -> List('g', 'h', 'i'),
    '5' -> List('j', 'k', 'l'),
    '6' -> List('m', 'n', 'o'),
    '7' -> List('p', 'q', 'r', 's'),
    '8' -> List('t', 'u', 'v'),
    '9' -> List('w', 'x', 'y', 'z')
  )

  sealed trait Solution {
    def letterCombinations(digits: String): List[String]
  }

  // My, Runtime 47, Memory 50
  object Solution1 extends Solution {
    def letterCombinations(digits: String): List[String] = {
      val acc = collection.mutable.ListBuffer("").filter(_ => digits.nonEmpty)
      for (letters <- digits.map(symbols).toList)
        acc.flatMapInPlace(accV => letters.map(accV + _))
      acc.toList
    }
  }

  // My, Runtime 42, Memory 13
  object Solution2 extends Solution {
    override def letterCombinations(digits: String): List[String] =
      digits.map(symbols).foldLeft(List("").filter(_ != digits)) { case (acc, next) =>
        for { accV <- acc; n <- next } yield accV + n
      }
  }

  // Not my, Runtime 76, Memory 23
  object Solution3 extends Solution {
    def letterCombinations(digits: String): List[String] = {
      def loop(digits: String): List[String] =
        if (digits.nonEmpty) for {
          previous <- loop(digits.tail)
          char <- symbols.getOrElse(digits.head, Nil)
        } yield char +: previous
        else List("")

      if (digits.isEmpty) List.empty else loop(digits)
    }

  }

  "letterCombinations" should {
    "work as expected" in {
      List(Solution1, Solution2, Solution3).foreach { solution =>
        solution.letterCombinations("23") should contain theSameElementsAs List(
          "ad",
          "ae",
          "af",
          "bd",
          "be",
          "bf",
          "cd",
          "ce",
          "cf"
        )

        solution.letterCombinations("") shouldBe List.empty
      }
    }
  }
}
