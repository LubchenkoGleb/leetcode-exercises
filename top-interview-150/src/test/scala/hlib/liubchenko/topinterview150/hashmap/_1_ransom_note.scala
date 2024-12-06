package hlib.liubchenko.topinterview150.hashmap

import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.wordspec.AnyWordSpec

class _1_ransom_note extends AnyWordSpec with Matchers {
  def canConstruct(ransomNote: String, magazine: String): Boolean = {
    def countLetters(s: String) = s.groupMapReduce(identity)(_ => 1)(_ + _)
    val (requiredLetters, availableLetters) = (countLetters(ransomNote), countLetters(magazine))

    requiredLetters.forall { case (letter, count) =>
      availableLetters.get(letter).exists(_ >= count)
    }
  }

  "canConstruct" should {
    "work as expected" in {
      Table(
        ("ransomNote", "magazine", "expected"),
        ("a", "b", false)
      ) { (ransomNote, magazine, expected) =>
        canConstruct(ransomNote, magazine) shouldBe expected
      }
    }
  }
}
