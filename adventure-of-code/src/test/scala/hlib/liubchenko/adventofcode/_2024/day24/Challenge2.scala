package hlib.liubchenko.adventofcode._2024.day24

import hlib.liubchenko.adventofcode._2024.Utils
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.language.implicitConversions

class Challenge2 extends AnyWordSpec with Matchers {
  implicit class StringOps(key: String) {
    def isX: Boolean = key.startsWith("x")
    def isY: Boolean = key.startsWith("y")
    def isZ: Boolean = key.startsWith("z")
    def isInput: Boolean = isX || isY
  }

  def initRules(lines: List[String]) = {
    val (predefinedWiresLines, rulesLines) = lines.span(_.nonEmpty)

    val wires = collection.mutable.Map.empty[String, Boolean]
    predefinedWiresLines.foreach { str =>
      val Array(name, value) = str.split(": ")
      wires.put(name, value.toInt)
    }

    val ruleRegex = "([a-z0-9]{3}) (XOR|OR|AND) ([a-z0-9]{3}) -> ([a-z0-9]{3})".r
    rulesLines.tail.map { case ruleRegex(a, op, b, c) =>
      (a, op, b, c)
    }
  }

  def method(lines: List[String]): String = {
    val rulesMap = initRules(lines)

    // The value of the output is `x XOR y XOR c`, the next carry bit `cN = (x AND y) OR ((x XOR y) AND c)`
    // but we just care about the fact that:
    // 1. If the output of a gate is z, then the operation has to be XOR unless it is the last bit.
    // 2. If the output of a gate is not z and the inputs are not x, y then it has to be AND / OR, but not XOR.
    // 3. If you have a XOR gate with inputs x, y, there must be another XOR gate with this gate as an input.
    //    Search through all gates for an XOR-gate with this gate as an input; if it does not exist, your (original) XOR gate is faulty.
    // 4. if you have an AND-gate, there must be an OR-gate with this gate as an input.
    //    If that gate doesn't exist, the original AND gate is faulty.

    val wrong1 = rulesMap.filter { case (_, op, _, res) => res.isZ && op != "XOR" && res != "z45" }

    val wrong2 = rulesMap.filter { case (l, op, r, res) => !res.isZ && !l.isInput && !r.isInput && op == "XOR" }

    val wrong3 = rulesMap.filter { case (l, op, r, res) =>
      (l.isInput || r.isInput) && op == "XOR" &&
        !rulesMap.exists { case (l2, op2, r2, _) => (l2 == res || r2 == res) && op2 == "XOR" } &&
        !l.contains("00")
    }
    println(wrong3.mkString(", "))

    val wrong4 = rulesMap.filter { case (l, op, r, res) =>
      ((l.isInput || r.isInput) && op == "AND") &&
        !rulesMap.exists { case (l2, op2, r2, _) => (l2 == res || r2 == res) && op2 == "OR" } &&
        !l.contains("00")
    }
    println(wrong4.mkString(", "))

    (wrong1 ++ wrong2 ++ wrong3 ++ wrong4).map(_._4).distinct.sorted.mkString(",")
  }

  private implicit def toBool(int: Int): Boolean = if (int == 0) false else true

  "Day #24 Challenge #1" should {

    "work as expected #2" in {
      val input = Utils.readInputFile(24)
      method(input) shouldBe "djg,dsd,hjm,mcq,sbg,z12,z19,z37"
    }

  }
}
