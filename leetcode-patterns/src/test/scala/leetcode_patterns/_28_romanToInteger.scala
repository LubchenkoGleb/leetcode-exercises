package leetcode_patterns

import scala.collection.mutable.ArrayBuffer

object _28_romanToInteger extends App {
  def romanToInt(s: String): Int = {
    var i = 0
    val buff = new ArrayBuffer[String]()

    def ifEx: String => Boolean = {
      case "IV" | "IX" | "XL" | "XC" | "CD" | "CM" => true
      case _                                       => false
    }

    while (i < s.length) {
      if (i + 1 < s.length && ifEx(s.substring(i, i + 2))) {
        buff.addOne(s.substring(i, i + 2))
        i += 2
      } else {
        buff.addOne(s.substring(i, i + 1))
        i += 1
      }
    }

    buff.map {
      case "IV" => 4
      case "IX" => 9
      case "XL" => 40
      case "XC" => 90
      case "CD" => 400
      case "CM" => 900
      case "I"  => 1
      case "V"  => 5
      case "X"  => 10
      case "L"  => 50
      case "C"  => 100
      case "D"  => 500
      case "M"  => 1000
    }.sum
  }

  def romanToInt2(s: String): Int = {
    val map = Map(
      'I' -> 1,
      'V' -> 5,
      'X' -> 10,
      'L' -> 50,
      'C' -> 100,
      'D' -> 500,
      'M' -> 1000
    )
    s.sliding(2).filter(_.length == 2).foldLeft(0) { (acc, str) =>
      val a = map(str(0))
      if (a < map(str(1))) acc - a
      else acc + a
    } + map(s.last)
  }

  println(romanToInt2("MCMXCIV")) // M-CM-XC-IV
  println(romanToInt2("I")) // M-CM-XC-IV
  println(romanToInt2("II")) // M-CM-XC-IV
}
