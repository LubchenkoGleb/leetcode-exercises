package hlib.liubchenko.topinterview150.hashmap

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.annotation.tailrec

class _7_happy_number extends AnyWordSpec with Matchers {
  def isHappy(n: Int): Boolean = {
    import scala.annotation.tailrec

    val maxRecLevel = 10

    @tailrec
    def loop(n: Int, recLevel: Int): Boolean =
      if (n == 1) true
      else if (recLevel > maxRecLevel) false
      else {
        val sum = n.toString.map(_.asDigit).map(i => i * i).sum
        loop(sum, recLevel + 1)
      }

    loop(n, 0)
  }

  "isHappy" should {
    "work as expected" in {
      isHappy(19) shouldBe true
      isHappy(2) shouldBe false
      // 2
      // 4
      // 16
      // 1 + 36 = 37
      // 9 + 49 = 58
      // 25 + 64 = 89
      // 64 + 81 = 145
      // 1 + 16 + 25 = 42
      // 16 + 4 = 20

    }
  }
}
