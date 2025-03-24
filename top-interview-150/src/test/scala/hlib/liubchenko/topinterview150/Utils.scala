package hlib.liubchenko.topinterview150

import scala.io.Source
import scala.util.Using

object Utils {
  def readInputFile(name: String): List[String] = Using(Source.fromResource(name)) { _.getLines().toList }.get
}
