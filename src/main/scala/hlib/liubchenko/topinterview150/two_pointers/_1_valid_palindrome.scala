package hlib.liubchenko.topinterview150.two_pointers

import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.immutable._
import scala.collection.mutable._


class _1_valid_palindrome extends AnyWordSpec with Matchers {
  // println(s"i: $i, j: $j, s(i): ${s(i)}, s(j): ${s(j)}")
  List("a")

  def isPalindrome(s: String): Boolean = {
    import scala.annotation.tailrec

    @tailrec
    def loop(i: Int, j: Int): Boolean =
      if (i >= j) true
      else if (!s(i).isLetterOrDigit) loop(i + 1, j)
      else if (!s(j).isLetterOrDigit) loop(i, j - 1)
      else if (s(i).toLower == s(j).toLower) loop(i + 1, j - 1)
      else false

    if (s.isEmpty) true
    else loop(0, s.length - 1)
  }

  "isPalindrome" should {
    "work as expected" in {
      val table = Table(
        ("input", "expected"),
        ("A man, a plan, a canal: Panama", true),
        ("race a car", false),
        (" ", true),
        ("", true)
      )
      forAll(table) { (input, expected) =>
        isPalindrome(input) shouldBe expected
        println()
      }
    }
  }
}
