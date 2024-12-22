package hlib.liubchenko.adventofcode._2024.day22

import hlib.liubchenko.adventofcode._2024.Utils
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.annotation.tailrec

class Challenge1 extends AnyWordSpec with Matchers {
  def secretNumbers(number: List[String]): Long =
    number.map(_.toLong).map(secretNumber(_, 2000)).sum

  def secretNumber(number: Long, n: Int): Long = {
    //1. Calculate the result of multiplying the secret number by 64. Then, mix this result into the secret number. Finally, prune the secret number.
    //2. Calculate the result of dividing the secret number by 32. Round the result down to the nearest integer.
    //   Then, mix this result into the secret number. Finally, prune the secret number.
    //3. Calculate the result of multiplying the secret number by 2048. Then, mix this result into the secret number. Finally, prune the secret number.

    //a. To mix a value into the secret number, calculate the bitwise XOR of the given value and the secret number.
    //   Then, the secret number becomes the result of that operation.
    //   (If the secret number is 42 and you were to mix 15 into the secret number, the secret number would become 37.)
    //b. To prune the secret number, calculate the value of the secret number modulo 16777216.
    //   Then, the secret number becomes the result of that operation.
    //   (If the secret number is 100000000 and you were to prune the secret number, the secret number would become 16113920.)

//    @tailrec
//    def loop(num: Long, i: Int): Long = if (i == 0) num
//    else {
//      val t1 = num ^ (num * 64) % 16777216
//      val t2 = t1 ^ (t1 / 32) % 16777216
//      val t3 = t2 ^ (t2 * 2048) % 16777216
//      loop(t3, i - 1)
//    }
//    loop(number, n)

    (0 until n).foldLeft(number) { case (num, _) =>
      val t1 = num ^ (num * 64) % 16777216
      val t2 = t1 ^ (t1 / 32) % 16777216
      t2 ^ (t2 * 2048) % 16777216
    }
  }

  "Day #22 Challenge #1" should {
    "work as expected #1" in {
      val input = List(
        "1", // 8685429
        "10", // 4700978
        "100", // 15273692
        "2024" // 8667524
      )
      secretNumbers(input) shouldBe 37327623
    }

    "work as expected #2" in {
      val input = Utils.readInputFile(22)
      secretNumbers(input) shouldBe 20401393616L
    }
  }

  "secretNumber" should {
    "work as expected" in {
      secretNumber(123, 1) shouldBe 15887950
      secretNumber(123, 2) shouldBe 16495136
      secretNumber(123, 3) shouldBe 527345
      secretNumber(123, 4) shouldBe 704524
      secretNumber(123, 5) shouldBe 1553684
      secretNumber(123, 6) shouldBe 12683156
      secretNumber(123, 7) shouldBe 11100544
      secretNumber(123, 8) shouldBe 12249484
      secretNumber(123, 9) shouldBe 7753432
      secretNumber(123, 10) shouldBe 5908254
    }
  }
}
