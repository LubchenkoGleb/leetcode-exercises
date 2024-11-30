package hlib.liubchenko.topinterview150.array_string

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _24_text_justification extends AnyWordSpec with Matchers {
  def fullJustify(words: Array[String], maxWidth: Int): List[String] = ???

  "fullJustify" should {
    "work as expected" in {
      fullJustify(Array("This", "is", "an", "example", "of", "text", "justification."), 16) shouldBe List(
        "This    is    an",
        "example  of text",
        "justification.  "
      )
      fullJustify(Array("What", "must", "be", "acknowledgment", "shall", "be"), 16) shouldBe List(
        "What   must   be",
        "acknowledgment  ",
        "shall be        "
      )
    }
  }
}
