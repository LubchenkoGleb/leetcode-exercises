package hlib.liubchenko.hackerrank

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

// https://www.hackerrank.com/test/crlnp8rgs12/questions/a2b68fq8p7b
class _3_challenge_palindrome_index extends AnyWordSpec with Matchers {
  def palindromeIndex(s: String): Int = {
    def isPalindrom(sI: Int, eI: Int) =
      (0 to ((eI - sI) / 2)).forall { i => s(sI + i) == s(eI - i) }

    val n = s.length
    (0 to (n / 2))
      .collectFirst {
        case i if s(i) != s(n - 1 - i) =>
          val skipFirst = isPalindrom(i + 1, n - 1 - i)
          val skipLast = isPalindrom(i, n - 2 - i)
          if (skipFirst) i else if (skipLast) n - 1 - i else -1
      }
      .getOrElse(-1)
  }

  "palindromeIndex" should {
    "work as expected" in {

      // hgy gsv lf(w)c wns wtuhmyaljkqlqjjqlqkjlaymhutw snw cfl vsg ygh
      palindromeIndex("hgygsvlfwcwnswtuhmyaljkqlqjjqlqkjlaymhutwsnwcflvsgygh") shouldEqual 8
      palindromeIndex("kjowoemiduaaxasnqghxbxkiccikxbxhgqnsaxaaudimeowojk") shouldEqual -1 // already palindrome
    }
  }
}
