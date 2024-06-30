package hlib.liubchenko.topinterview150.hashmap

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _5_group_anagrams extends AnyWordSpec with Matchers {
  def groupAnagrams(strs: Array[String]): List[List[String]] = {
    strs.groupBy(_.sorted).values.map(_.toList).toList
  }

  "groupAnagrams" should {
    "work as expected" in {
      groupAnagrams(Array("eat", "tea", "tan", "ate", "nat", "bat")).map(_.toSet).toSet shouldBe Set(
        Set("eat", "tea", "ate"),
        Set("bat"),
        Set("tan", "nat")
      )
    }
  }
}
