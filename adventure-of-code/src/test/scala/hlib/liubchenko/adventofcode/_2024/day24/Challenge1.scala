package hlib.liubchenko.adventofcode._2024.day24

import hlib.liubchenko.adventofcode._2024.Utils
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.language.implicitConversions

class Challenge1 extends AnyWordSpec with Matchers {
  def method(lines: List[String]): Long = {
    val (predefinedWiresLines, rulesLines) = lines.span(_.nonEmpty)

    val wires = collection.mutable.Map.empty[String, Boolean]
    predefinedWiresLines.foreach { str =>
      val Array(name, value) = str.split(": ")
      wires.put(name, value.toInt)
    }

    val ruleRegex = "([a-z0-9]{3}) (XOR|OR|AND) ([a-z0-9]{3}) -> ([a-z0-9]{3})".r
    val rulesMap = rulesLines.tail.map { case ruleRegex(a, op, b, c) =>
      c -> (a, op, b)
    }.toMap

    def loop(cName: String): Boolean = {
      def calcC = {
        val (aName, op, bName) = rulesMap(cName)
        val (a, b) = (loop(aName), loop(bName))
        op match {
          case "AND" => a && b
          case "OR"  => a || b
          case "XOR" => a ^ b
        }
      }
      wires.getOrElseUpdate(cName, calcC)
    }

    rulesMap.keys.foreach(loop)

    val res = wires.view
      .filterKeys(_.startsWith("z"))
      .toList
      .sortBy(_._1)(Ordering[String].reverse)
      .map(_._2)
      .map(if (_) 1 else 0)
      .mkString

    java.lang.Long.parseLong(res, 2)
  }

  private implicit def toBool(int: Int): Boolean = if (int == 0) false else true

  "Day #24 Challenge #1" should {
    "work as expected #1" in {
      val input = List(
        "x00: 1",
        "x01: 0",
        "x02: 1",
        "x03: 1",
        "x04: 0",
        "y00: 1",
        "y01: 1",
        "y02: 1",
        "y03: 1",
        "y04: 1",
        "",
        "ntg XOR fgs -> mjb",
        "y02 OR x01 -> tnw",
        "kwq OR kpj -> z05",
        "x00 OR x03 -> fst",
        "tgd XOR rvg -> z01",
        "vdt OR tnw -> bfw",
        "bfw AND frj -> z10",
        "ffh OR nrd -> bqk",
        "y00 AND y03 -> djm",
        "y03 OR y00 -> psh",
        "bqk OR frj -> z08",
        "tnw OR fst -> frj",
        "gnj AND tgd -> z11",
        "bfw XOR mjb -> z00",
        "x03 OR x00 -> vdt",
        "gnj AND wpb -> z02",
        "x04 AND y00 -> kjc",
        "djm OR pbm -> qhw",
        "nrd AND vdt -> hwm",
        "kjc AND fst -> rvg",
        "y04 OR y02 -> fgs",
        "y01 AND x02 -> pbm",
        "ntg OR kjc -> kwq",
        "psh XOR fgs -> tgd",
        "qhw XOR tgd -> z09",
        "pbm OR djm -> kpj",
        "x03 XOR y03 -> ffh",
        "x00 XOR y04 -> ntg",
        "bfw OR bqk -> z06",
        "nrd XOR fgs -> wpb",
        "frj XOR qhw -> z04",
        "bqk OR frj -> z07",
        "y03 OR x01 -> nrd",
        "hwm AND bqk -> z03",
        "tgd XOR rvg -> z12",
        "tnw OR pbm -> gnj"
      )
      method(input) shouldBe 2024
    }

    "work as expected #2" in {
      val input = Utils.readInputFile(24)
      method(input) shouldBe 64755511006320L
    }
  }
}
