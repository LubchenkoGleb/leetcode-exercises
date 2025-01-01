package hlib.liubchenko.adventofcode._2024.day24

import hlib.liubchenko.adventofcode._2024.Utils
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.mutable
import scala.language.implicitConversions

class Challenge2 extends AnyWordSpec with Matchers {
  def method(lines: List[String], bitsCount: Int, opp: (Long, Long) => Long): String = {
    val (predefinedWiresLines, rulesLines) = lines.span(_.nonEmpty)

    val initialWires = predefinedWiresLines
      .map(_.split(": "))
      .map { case Array(name, value) => name -> toBool(value.toInt) }
      .toMap

    val ruleRegex = "([a-z0-9]{3}) (XOR|OR|AND) ([a-z0-9]{3}) -> ([a-z0-9]{3})".r
    val rulesMap = mutable.Map.from(rulesLines.tail.map { case ruleRegex(a, op, b, c) => c -> (a, op, b) }.toMap)

    def check(rulesMap: mutable.Map[String, (String, String, String)]): Int = {
      val wires = collection.mutable.Map.from(initialWires)

      def loop(cName: String): Boolean = {
        def calcC = {
          val (aName, op, bName) = rulesMap(cName)
          val (a, b) = (loop(aName), loop(bName))
          op match {
            case "AND" => a && b
            case "OR" => a || b
            case "XOR" => a ^ b
          }
        }

        wires.getOrElseUpdate(cName, calcC)
      }

      rulesMap.keys.foreach(loop)

      val expected = opp(getNumber(wires, "x"), getNumber(wires, "y")).toBinaryString
      val actual = getNumber(wires, "z").toBinaryString
      val length = math.max(expected.length, actual.length)

      val expectedF = expected.reverse.padTo(length, ' ').reverse
      val actualF = actual.reverse.padTo(length, ' ').reverse

      val zipped = expectedF.zip(actualF)
      val res = zipped.takeWhile { case (a, b) => a == b }
      res.length
    }

    var bitsMatches = check(rulesMap)
    val fixedRules = mutable.Set.empty[String]
    val iterator = rulesMap.toList.combinations(2)

    while (bitsMatches != bitsCount && iterator.hasNext) {
      val (aK, aV) :: (bK, bV) :: Nil = iterator.next()

      rulesMap.update(aK, bV)
      rulesMap.update(bK, aV)

      lazy val checkRes = check(rulesMap)
      if (fixedRules.intersect(Set(aK, bK)).isEmpty && checkRes > bitsMatches) {
        fixedRules.add(aK)
        fixedRules.add(bK)
        bitsMatches = checkRes
      } else {
        rulesMap.update(aK, aV)
        rulesMap.update(bK, bV)
      }
    }

    fixedRules.toList.sorted.mkString(",")
  }

  def getNumber(wires: mutable.Map[String, Boolean], prefix: String): Long = {
    val res = wires.view
      .filterKeys(_.startsWith(prefix))
      .toList
      .sortBy(_._1)(Ordering[String].reverse)
      .map(_._2)
      .map(if (_) 1 else 0)
      .mkString

    java.lang.Long.parseLong(res, 2)
  }

  private implicit def toBool(int: Int): Boolean = if (int == 0) false else true

  "Day #24 Challenge #2" should {
    "work as expected #1" in {
      val input = List(
        "x00: 0",
        "x01: 1",
        "x02: 0",
        "x03: 1",
        "x04: 0",
        "x05: 1",
        "y00: 0",
        "y01: 0",
        "y02: 1",
        "y03: 1",
        "y04: 0",
        "y05: 1",
        "",
        "x00 AND y00 -> z05",
        "x01 AND y01 -> z02",
        "x02 AND y02 -> z01",
        "x03 AND y03 -> z03",
        "x04 AND y04 -> z04",
        "x05 AND y05 -> z00"
      )
      method(input, 6, _ & _) shouldBe "z00,z01,z02,z05"
    }

    "work as expected #2" in {
//        List(1,2,3,4).combinations(2).foreach(println)

      val input = Utils.readInputFile(24)
      method(input, 4, _ + _) shouldBe 0
    }
  }
}
