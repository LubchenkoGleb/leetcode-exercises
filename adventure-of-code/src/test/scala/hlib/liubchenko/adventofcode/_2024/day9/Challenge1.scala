package hlib.liubchenko.adventofcode._2024.day9

import hlib.liubchenko.adventofcode._2024.Utils
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Challenge1 extends AnyWordSpec with Matchers {
  def compactSpace(input: String): Long = {
    val transformed = input
      .map(_.asDigit)
      .sliding(2, 2)
      .zipWithIndex
      .flatMap { case (block, i) => List.fill(block.head)(i.toString) ++ block.tail.flatMap(List.fill(_)(".")) }
      .toArray

    var (start, end) = (0, transformed.length - 1)
    while (start < end) {
      if (transformed(start) != ".") start += 1
      else if (transformed(end) == ".") end -= 1
      else {
        transformed(start) = transformed(end)
        transformed(end) = "."
        start += 1; end -= 1
      }
    }

    transformed.takeWhile(_ != ".").zipWithIndex.map { case (id, i) => id.toLong * i }.sum
  }

  "Day #9 Challenge #1" should {
    "work as expected #1" in {
      // 00...111...2...333.44.5555.6666.777.888899

      // 00...111...2...333.44.5555.6666.777.888899
      // 009..111...2...333.44.5555.6666.777.88889.
      // 0099.111...2...333.44.5555.6666.777.8888..
      // 00998111...2...333.44.5555.6666.777.888...
      // 009981118..2...333.44.5555.6666.777.88....
      // 0099811188.2...333.44.5555.6666.777.8.....
      // 009981118882...333.44.5555.6666.777.......
      // 0099811188827..333.44.5555.6666.77........
      // 00998111888277.333.44.5555.6666.7.........
      // 009981118882777333.44.5555.6666...........
      // 009981118882777333644.5555.666............
      // 00998111888277733364465555.66.............
      // 0099811188827773336446555566..............
      compactSpace("2333133121414131402") shouldBe 1928
    }

    "work as expected #2" in {
      val input = Utils.readInputFileAsString(9)
      compactSpace(input) shouldBe 6385338159127L
    }
  }
}
