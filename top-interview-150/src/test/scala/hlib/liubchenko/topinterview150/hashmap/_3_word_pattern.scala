package hlib.liubchenko.topinterview150.hashmap

import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.wordspec.AnyWordSpec

class _3_word_pattern extends AnyWordSpec with Matchers {
  def wordPattern(pattern: String, s: String): Boolean = {
    import scala.collection.mutable
    val (patternToSMapping, sToPatternMapping) = (mutable.Map.empty[Char, String], mutable.Map.empty[String, Char])

    val words = s.split(" ")
    if (words.length != pattern.length) false
    else
      pattern.zip(words).forall { case (c, word) =>
        patternToSMapping.getOrElseUpdate(c, word) == word && sToPatternMapping.getOrElseUpdate(word, c) == c
      }
  }

  "wordPattern" should {
    "work as expected" in {
      Table(
        ("patter", "s", "expected"),
        ("abba", "dog cat cat dog", true),
        ("abba", "dog cat cat fish", false),
        ("aaaa", "dog cat cat dog", false),
        ("abba", "dog dog dog dog", false)
      ) { (pattern, s, expected) =>
        wordPattern(pattern, s) shouldBe expected
      }
    }
  }
}
