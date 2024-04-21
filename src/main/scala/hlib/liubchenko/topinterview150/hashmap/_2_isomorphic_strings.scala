package hlib.liubchenko.topinterview150.hashmap

import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.wordspec.AnyWordSpec

class _2_isomorphic_strings extends AnyWordSpec with Matchers {
  def isIsomorphic(s: String, t: String): Boolean = {
    import scala.collection.mutable
    val (sToTMapping, tToSMapping) = (mutable.Map.empty[Char, Char], mutable.Map.empty[Char, Char])

    if (s.length != t.length) false
    else
      s.indices.forall { i =>
        val (sC, tC) = (s(i), t(i))
        sToTMapping.getOrElseUpdate(sC, tC) == tC && tToSMapping.getOrElseUpdate(tC, sC) == sC
      }
  }

  "isIsomorphic" should {
    "work as expected" in {
      Table(
        ("s", "t", "expected"),
        ("egg", "add", true),
        ("foo", "bar", false),
        ("paper", "title", true),
        ("bbbaaaba", "aaabbbba", false),
        ("badc", "baba", false)
      ) { (s, t, expected) =>
        isIsomorphic(s, t) shouldBe expected
      }
    }
  }
}
