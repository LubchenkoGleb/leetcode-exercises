package hlib.liubchenko.topinterview150.array_string

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _17_roman_to_integer extends AnyWordSpec with Matchers {
  def romanToInt_v1(s: String): Int = {

    def matchSimple(c: Char) = c match {
      case 'I' => 1
      case 'V' => 5
      case 'X' => 10
      case 'L' => 50
      case 'C' => 100
      case 'D' => 500
      case 'M' => 1000
    }

    var i = 0
    var acc = 0
    while (i < s.length - 1) {

      val sI = matchSimple(s(i))
      val sNext = if (i + 1 > s.length) 0 else matchSimple(s(i + 1))

      if (sNext > sI) { i += 2; acc += sNext - sI }
      else { acc += sI; i += 1 }
    }

    acc
  }

  def romanToInt(s: String): Int = {

    def matchSimple(c: Char) = c match {
      case 'I' => 1
      case 'V' => 5
      case 'X' => 10
      case 'L' => 50
      case 'C' => 100
      case 'D' => 500
      case 'M' => 1000
    }

    s.sliding(2)
      .filter(_.length == 2)
      .map(_.map(matchSimple))
      .map { s => s(0) -> s(1) }
      .foldLeft(0) { case (acc, (_1, _2)) =>
        if (_1 >= _2) acc + _1
        else acc - _1
      } + s.lastOption.map(matchSimple).getOrElse(0)
  }

  "romanToInt" should {
    "work as expected" in {
      romanToInt("MCMXCIV") shouldBe 1994
    }
  }
}
