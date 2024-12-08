package hlib.liubchenko.adventofcode._2024.day5

import hlib.liubchenko.adventofcode._2024.Utils
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Challenge2 extends AnyWordSpec with Matchers {
  def pageOrdering(input: List[String]): Int = {
    val (rules, pageNumbers) = input.span(_.nonEmpty)
    val rulesMap = rules
      .map(_.split('|'))
      .map { case Array(a, b) => (a.toInt, b.toInt) }
      .groupMap(_._1)(_._2)
      .view
      .mapValues(_.toSet)
      .toMap

    def validate(line: Array[Int]): Boolean = line.tails
      .filter { _.length >= 2 }
      .forall { findWrongElement(_).isEmpty }

    def findWrongElement(line: Array[Int]): Option[Int] = {
      val (pageToValidate, nextPages) = (line.head, line.tail)
      rulesMap.get(pageToValidate) match {
        case Some(allowedPages) => nextPages.find(p => !allowedPages.contains(p))
        case None               => Some(pageToValidate)
      }
    }

    pageNumbers
      .filter(_.nonEmpty)
      .map(_.split(',').map(_.toInt))
      .filterNot(validate)
      .map { line =>
        var i = 0

        def replace(wrongElementValue: Int): Unit = {
          val wrongElementIndex = line.indexOf(wrongElementValue)
          line(wrongElementIndex) = line(i)
          line(i) = wrongElementValue
        }

        while (i <= line.length - 1) {
          findWrongElement(line.drop(i)) match {
            case Some(wrongElementValue) if wrongElementValue != line(i) => replace(wrongElementValue)
            case Some(wrongElementValue) =>
              line
                .drop(i + 1)
                .find { rulesMap.get(_).toSet.flatten.contains(wrongElementValue) } match {
                case Some(value) => replace(value)
                case None        => i += 1
              }
            case None => i += 1
          }

        }

        line(line.length / 2)
      }
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
      val input = Utils.readInputFile(4)
      pageOrdering(input) shouldBe 5093
    }
  }
}
