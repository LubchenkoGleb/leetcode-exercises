package hlib.liubchenko.adventofcode._2024

import scala.io.Source
import scala.util.Using

object Utils {
  def readInputFile(day: Int): List[String] = Using(Source.fromFile(inputFilePath(day))) { _.getLines().toList }.get
  def readInputFileAsString(day: Int): String = readInputFile(day).mkString

  private def inputFilePath(day: Int): String = {
    s"/Users/macbookpro/Projects/leetcode-exercises/adventure-of-code/src/test/scala/hlib/liubchenko/_2024/day$day/input.txt"
  }
}
