package hlib.liubchenko.adventofcode._2024.day4

import hlib.liubchenko.adventofcode._2024.Utils
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Challenge1 extends AnyWordSpec with Matchers {
  def pageOrdering(input: List[String]): Int = {
    val (rules, pageNumbers) = input.span(_.nonEmpty)
    val numbers = rules
      .map(_.split('|'))
      .map { case Array(a, b) => (a.toInt, b.toInt) }
    val rulesMap = numbers
      .groupMap(_._1)(_._2)
      .view
      .mapValues(_.toSet)
      .toMap
    val orderedNumbersMap = numbers
      .flatMap { case (a, b) => List(a, b) }
      .distinct
      .sortWith { (a, b) =>
        rulesMap.get(a).exists(_.contains(b))
      }
      .zipWithIndex
      .toMap

    def validate(line: Array[Int]): Boolean = {
      val res = line.sliding(2).forall { case Array(a, b) =>
        if(orderedNumbersMap.contains(a) && orderedNumbersMap.contains(b)) orderedNumbersMap(a) < orderedNumbersMap(b)
        else true
      }
      res
    }

    pageNumbers
      .filter(_.nonEmpty)
      .map(_.split(',').map(_.toInt))
      .filter(validate)
      .map { line => line(line.length / 2) }
      .sum
  }

  "fixMemory" should {
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
          |97,13,75,29,47""".stripMargin.split("\n").toList) shouldBe 143
    }

    "work as expected #2" in {
      val input = Utils.readInputFile("src/main/scala/hlib/liubchenko/adventofcode/_2024/day4/input.txt")
      pageOrdering(input) shouldBe 6051
    }
  }
}
