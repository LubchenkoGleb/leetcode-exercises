package hlib.liubchenko.adventofcode._2024.day9

import hlib.liubchenko.adventofcode._2024.Utils
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Challenge2 extends AnyWordSpec with Matchers {
  def compactSpace(input: String): Long = {
    val transformed = input
      .map(_.asDigit)
      .sliding(2, 2)
      .zipWithIndex
      .flatMap { case (block, i) => List.fill(block.head)(i.toString) ++ block.tail.flatMap(List.fill(_)(".")) }
      .toArray

    var end = transformed.length - 1

    def findBlockStart: Int = {
      var i = end
      while (i > 0 && transformed(i) == transformed(end)) i -= 1
      i + 1
    }

    def findEmptySpace(size: Int) = {
      var (i, space) = (0, 0)
      while (i < end && space != size) {
        if (transformed(i) == ".") space += 1
        else space = 0
        i += 1
      }
      if (space == size) Some(i - size) else None
    }

    while (end > 1) {
      if (transformed(end) == ".") end -= 1
      else {
        val blockStart = findBlockStart
        findEmptySpace(end - blockStart + 1).foreach { emptySpaceStart =>
          (0 to end - blockStart).foreach { i =>
            transformed(emptySpaceStart + i) = transformed(blockStart + i)
            transformed(blockStart + i) = "."
          }
        }

        end = blockStart - 1
      }

    }

    transformed.zipWithIndex.collect { case (id, i) if id != "." => id.toLong * i }.sum
  }

  "Day #9 Challenge #2" should {
    "work as expected #1" in {
      // 00...111...2...333.44.5555.6666.777.888899
      // 0099.111...2...333.44.5555.6666.777.8888..
      // 0099.1117772...333.44.5555.6666.....8888..
      // 0099.111777244.333....5555.6666.....8888..
      // 00992111777.44.333....5555.6666.....8888..
      compactSpace("2333133121414131402") shouldBe 2858
    }

    "work as expected #2" in {
      val input = Utils.readInputFileAsString(9)
      compactSpace(input) shouldBe 6415163624282L
    }
  }
}
