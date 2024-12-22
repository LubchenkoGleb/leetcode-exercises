package hlib.liubchenko.adventofcode._2024.day22

import hlib.liubchenko.adventofcode._2024.Utils
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Challenge2 extends AnyWordSpec with Matchers {
  def secretNumbers(number: List[String]): Long = {
    val acc = collection.mutable.Map.empty[(Long, Long, Long, Long), Long]

    for {
      seller <- number.map(_.toLong)
      (pair, price) <- timeToSell(seller, 2000)
    } acc.updateWith(pair) { _.map(_ + price).orElse(Some(price)) }

    acc.values.max
  }

  def timeToSell(number: Long, n: Int): Map[(Long, Long, Long, Long), Long] = (0L until n)
    .scan(number) { (num, _) =>
      val t1 = num ^ (num * 64) % 16777216
      val t2 = t1 ^ (t1 / 32) % 16777216
      t2 ^ (t2 * 2048) % 16777216
    }
    .map(_ % 10)
    .sliding(5)
    .map { case IndexedSeq(_1, _2, _3, _4, _5) =>
      (_5 - _4, _4 - _3, _3 - _2, _2 - _1) -> _5
    }
    .toSeq
    .groupMapReduce(_._1)(_._2)((a, _) => a)

  "Day #22 Challenge #2" should {
    "work as expected #1" in {
      val input = List(
        "1",
        "2",
        "3",
        "2024"
      )
      secretNumbers(input) shouldBe 23
    }

    "work as expected #2" in {
      val input = Utils.readInputFile(22)
      secretNumbers(input) shouldBe 2272
    }
  }
}
