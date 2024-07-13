package hlib.liubchenko.topinterview150.array_string

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _12_insert_delete_get_random_O1 extends AnyWordSpec with Matchers {
  class RandomizedSet() {
    import scala.collection.mutable
    import scala.collection.mutable.ArrayBuffer
    private val dataSet = new ArrayBuffer[Int]()
    private val mapping = mutable.HashMap.empty[Int, Int]

    def insert(`val`: Int): Boolean = {
      if (mapping.contains(`val`)) false
      else {
        mapping += (`val` -> dataSet.length)
        dataSet += `val`
        true
      }
    }

    def remove(`val`: Int): Boolean = {
      if (mapping.contains(`val`)) {
        val index = mapping(`val`)

        dataSet(index) = dataSet.last
        mapping(dataSet(index)) = index

        dataSet.dropRightInPlace(1)
        mapping.remove(`val`)

        true
      } else false
    }

    def getRandom(): Int = {
      import scala.util.Random
      dataSet(new Random().nextInt(dataSet.size))
    }
  }

  "RandomizedSet" should {
    "work as expected #1" in {
      val set = new RandomizedSet()
      set.insert(1) shouldBe true
      set.remove(2) shouldBe false
      set.insert(2) shouldBe true
      set.getRandom shouldBe 2
      set.remove(1) shouldBe true
      set.insert(2) shouldBe false
      set.getRandom shouldBe 2
      // ["RandomizedSet",  "insert", "remove", "insert", "getRandom",  "remove", "insert", "getRandom"]
      //  [[],              [1],      [2],      [2],      [],           [1],      [2],      []]
      //  Output
      //  [null,            true,     false,    true,     2,            true,     false,    2]

    }

    "work as expected #2" in {
      val set = new RandomizedSet()
      set.remove(0) shouldBe false
      set.remove(0) shouldBe false
      set.insert(0) shouldBe true
      set.getRandom shouldBe 0
      set.remove(0) shouldBe true
      set.insert(0) shouldBe true
      // ["RandomizedSet",  "remove", "remove", "insert", "getRandom",  "remove", "insert"]
      // [[],               [0],      [0],      [0],      [],           [0],      [0]]
      // Output
      // [null,             false,    false,    true,     0,            true,     true]
    }
  }
}
