package hlib.liubchenko.adventofcode._2024.day5

import hlib.liubchenko.adventofcode._2024.Utils
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Challenge2 extends AnyWordSpec with Matchers {
  def pageOrdering(input: List[String]): Int = {
    val (rules, pageNumbers) = input.span(_.nonEmpty)

    val parsedRules = rules
      .map(_.split('|'))
      .map { a => println(a.toList); a }
      .map { case Array(a, b) => (a.toInt, b.toInt) }
      .toSet

    def orderPages(row: Array[Int]) = row.sortWith { (a, b) => parsedRules.contains((a, b)) }

    def validate(line: Array[Int]): Boolean =
      line.sliding(2).forall { case Array(a, b) => parsedRules.contains((a, b)) }

    pageNumbers
      .filter(_.nonEmpty)
      .map(_.split(',').map(_.toInt))
      .filterNot(validate)
      .map(orderPages)
      .map { line => line(line.length / 2) }
      .sum
  }

  "Day #5 Challenge #2 pageOrdering" should {
    "work as expected #1" in {
      pageOrdering("""47|53
                     |97|13
                     |97|61
                     |97|47
                     |75|29
                     |61|13
                     |75|53
                     |29|13
                     |97|29
                     |53|29
                     |61|53
                     |97|53
                     |61|29
                     |47|13
                     |75|47
                     |97|75
                     |47|61
                     |75|61
                     |47|29
                     |75|13
                     |53|13
                     |
                     |75,47,61,53,29
                     |97,61,53,29,13
                     |75,29,13
                     |75,97,47,61,53
                     |61,13,29
                     |97,13,75,29,47""".stripMargin.split("\n").toList) shouldBe 123
    }

    "Day #5 Challenge #2" in {
      val input = Utils.readInputFile(5)
      pageOrdering(input) shouldBe 5093
    }
  }
}
