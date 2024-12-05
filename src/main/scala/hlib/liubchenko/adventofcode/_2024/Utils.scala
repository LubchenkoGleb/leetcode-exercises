package hlib.liubchenko.adventofcode._2024

import scala.io.Source
import scala.util.Using

object Utils {
  def readInputFile(filePath: String): List[String] = Using(Source.fromFile(filePath)) { _.getLines().toList }.get
  def readInputFileAsString(filePath: String): String = readInputFile(filePath).mkString
}
