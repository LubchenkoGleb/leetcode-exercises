package hlib.liubchenko.adventofcode._2024.day11

import hlib.liubchenko.adventofcode._2024.Utils
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Challenge2 extends AnyWordSpec with Matchers {
  def method(stonesStr: String, blinks: Int): Long = {
    val stones = collection.mutable.ListBuffer.from(stonesStr.split(" ").map(_.toLong))

    val uniqueNumbers = collection.mutable.HashSet.empty[Long]

    def loop(blinks: Int, stone: Long): Long = {
      uniqueNumbers.add(stone)
      if (blinks == 0) 1
      else if (stone == 0) loop(blinks - 1, 1)
      else if (stone.toString.length % 2 == 0) {
        val (s1, s2) = stone.toString.splitAt(stone.toString.length / 2)
        loop(blinks - 1, s1.toLong) + loop(blinks - 1, s2.toLong)
      } else loop(blinks - 1, stone * 2024)
    }

    val res = stones.map(loop(blinks, _)).sum
    println(s"Unique numbers: ${uniqueNumbers.size}")
    res
  }

  "Day #11 Challenge #2" should {
    "work as expected #1" in {
      method("125 17", 6) shouldBe 22
    }

    "work as expected #2" in {
      val input = Utils.readInputFileAsString(11)
      method(input, 35) shouldBe 220999
    }
  }
}
