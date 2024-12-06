package hlib.liubchenko.topinterview150.bit_manipulation

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _2_reverse_bits extends AnyWordSpec with Matchers {
  // you need treat n as an unsigned value
  def reverseBits(x: Int): Int = {
    var num = x

    num = num >>> 16 | num << 16
    num = (num & 0xff00ff00) >>> 8 | (num & 0x00ff00ff) << 8
    num = (num & 0xf0f0f0f0) >>> 4 | (num & 0x0f0f0f0f) << 4
    num = (num & 0xcccccccc) >>> 2 | (num & 0x33333333) << 2 // hex 'c'= 12 = 1100, hex '3' = 0011
    num = (num & 0xaaaaaaaa) >>> 1 | (num & 0x55555555) << 1 // hex 'a'= 10 = 1010, hex '5' = 0101

    num
  }

  def printBinaryStr(value: Int): Unit = {
    val binaryStr = value.toBinaryString
    val fixedLength = if (binaryStr.length < 32) "0" * (32 - binaryStr.length) + binaryStr else binaryStr
    println(fixedLength.sliding(4, 4).mkString("|"))
  }

  "reverseBits" should {
    "work as expected" in {
      val input = Integer.parseUnsignedInt("00000010100101000001111010011100", 2)
      val expected = Integer.parseUnsignedInt("00111001011110000010100101000000", 2)
      reverseBits(input) shouldBe expected
    }
  }
}
