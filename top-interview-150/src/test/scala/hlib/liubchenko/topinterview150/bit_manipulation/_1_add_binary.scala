package hlib.liubchenko.topinterview150.bit_manipulation

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _1_add_binary extends AnyWordSpec with Matchers {
  def addBinary(a: String, b: String): String = {
    val buff = new StringBuilder()
    val length = math.max(a.length, b.length)
    var rem = '0'

    (length - 1 to 0 by -1).foreach { i =>
      val aV =
        if (a.length > b.length) a(i)
        else if (i - (b.length - a.length) >= 0) a(i - (b.length - a.length))
        else '0'
      val bV =
        if (b.length > a.length) b(i)
        else if (i - (a.length - b.length) >= 0) b(i - (a.length - b.length))
        else '0'

      val res = (aV, bV, rem) match {
        case ('0', '0', '1') | ('0', '1', '0') | ('1', '0', '0') => rem = '0'; '1'
        case ('0', '0', '0')                                     => '0'
        case ('1', '0', '1') | ('0', '1', '1') | ('1', '1', '0') => rem = '1'; '0'
        case ('1', '1', '1')                                     => rem = '1'; '1'
      }

      buff.append(res)
    }

    if(rem == '1') buff.append('1')

    buff.reverse.result()
  }

  "addBinary" should {
    "work as expected" in {
      addBinary("010101", "1101") shouldBe "100010"
    }
  }
}
