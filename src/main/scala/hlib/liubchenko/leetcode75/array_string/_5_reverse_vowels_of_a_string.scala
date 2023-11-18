package hlib.liubchenko.leetcode75.array_string

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _5_reverse_vowels_of_a_string extends AnyWordSpec with Matchers {
  sealed trait Solution {
    def reverseVowels(s: String): String
  }

  // Mine, Runtime 31, Memory 10
  object Solution1 extends Solution {
    private val vowels = Set('a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U')

    def reverseVowels(s: String): String = {
      val chars = s.toCharArray

      def loop(f: Int, b: Int): Unit = {
        val nF = (f to b).find { i => vowels.contains(chars(i)) }
        val nB = (b to f by -1).find { i => vowels.contains(chars(i)) }
        nF.zip(nB).filter { case (f, b) => f <= b }.foreach { case (f, b) =>
          val tmp = chars(f)
          chars(f) = chars(b)
          chars(b) = tmp
          //swap
          loop(f + 1, b - 1)
        }
      }

      loop(0, s.length - 1)
      chars.mkString
    }
  }

  // Not mine, Runtime 46, Memory 19
  object Solution2 extends Solution {
    import scala.collection.mutable

    private val vowels = Set('a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U')

    override def reverseVowels(s: String): String = {
      val stack = mutable.Stack.empty[Char]
      for (c <- s if vowels.contains(c)) stack.push(c)
      s.toCharArray.map(c => if (vowels.contains(c)) stack.pop() else c).mkString
    }
  }

  "reverseVowels" should {
    "work" in {
      List(Solution1, Solution2).foreach { solution =>
        solution.reverseVowels("hello") shouldBe "holle"
        solution.reverseVowels("leetcode") shouldBe "leotcede"
        solution.reverseVowels("aA") shouldBe "Aa"
      }
    }
  }

}
