package hlib.liubchenko.adventofcode._2024.day17

import hlib.liubchenko.adventofcode._2024.Utils
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Challenge2 extends AnyWordSpec with Matchers {
  def runProgram(input: String): String = {
    val (regA, regB, regC, ops) = input.split("\n") match {
      case Array(regAStr, regBStr, regCStr, _, instructionsStr) =>
        def parseReg(regStr: String) = regStr.split(" ")(2).toLong

        val ops = instructionsStr.split(" ")(1).split(",").map(_.toInt)
        (parseReg(regAStr), parseReg(regBStr), parseReg(regCStr), ops)
    }

    runProgram(regA, regB, regC, ops)._4
  }

  def runProgram(_regA: Long, _regB: Long, _regC: Long, ops: Array[Int]) = {
    var (regA, regB, regC) = (_regA, _regB, _regC)
    val output = collection.mutable.ListBuffer.empty[Long]
    var pointer = 0

    while (pointer < ops.length - 1) {
      val (instruction, operand) = (ops(pointer), ops(pointer + 1))
      val operandValue = operand match {
        case 4 => regA
        case 5 => regB
        case 6 => regC
        case v => v
      }

      instruction match {
        case 0 => regA = regA / math.pow(2, operandValue).toInt
        case 1 => regB = regB ^ operandValue
        case 2 => regB = operandValue % 8
        case 3 => if (regA != 0) pointer = operandValue.toInt else pointer += 2
        case 4 => regB = regB ^ regC
        case 5 => output.addOne(operandValue % 8)
        case 6 => regB = regA / math.pow(2, operandValue).toInt
        case _ => regC = regA / math.pow(2, operandValue).toInt
      }

      if (instruction != 3) pointer += 2
    }

    (regA, regB, regC, output.mkString(","))
  }

  // helper method for manual solving
  def solve(expected: Int, baseA: Long) = {
    println(s"Original number: $baseA (Binary: ${baseA.toBinaryString})")

    println("\nEnumerating all numbers with 3 prepended bits:")
    val candidates = for (suffix <- 0 until 8) yield { // Loop through all 3-bit prefixes (0 to 7)
      val result = (baseA << 3) | suffix
      println(f"Suffix: ${suffix.toBinaryString}%3s → Result: $result (Binary: ${result.toBinaryString})")
      result
    }

    def testA(regA: Long): Boolean = {
      var regB = 0L
      var regC = 0L
      regB = regA % 8
      regB = regB ^ 2
      regC = regA / math.pow(2, regB).toInt
      regB = regB ^ regC
      regB = regB ^ 7
      val res = regB % 8
      println(s"regA: $regA, res=$res")
      res == expected
    }

    val res = candidates.filter(testA)

    val msg = if (res.isEmpty) "Solution is not found" else s"Solution is ${res.sorted.mkString(", ")}"
    println(msg)
  }

  "Day #17 Challenge #2" should {
    "find value" in {
      pending

      // 2,4| 1,2| 7,5| 0,3| 4,7| 1,7| 5,5| 3,0|
      // 2,4 - regB = regA % 8
      // 1,2 - regB = regB xor 2
      // 7,5 - regC = regA / (2 ^ regB)
      // 0,3 - regA = regA / 8
      // 4,7 - regB = regB xor regC
      // 1,7 - regB = regB xor 7 ???
      // 5,5 - out regB % 8
      // 3,0 - jum to beginning if regA != 0


      // exp 0: solution 5
      // exp 3: solution 43
      // exp 5: solution 346
      // exp 5: solution 2770
      // exp 7: solution 22163
      // exp 1: solution 177309
      // exp 7: solution 1418472
      // exp 4: solution 11347777
      // exp 3: solution 90782219
      // exp 0: solution 726257756
      // exp 5: solution 5810062048
      // exp 7: solution 46480496387
      // exp 2: solution 371843971102
      // exp 1: solution 2974751768816
      // exp 4: solution 23798014150529
      // exp 2: solution 190384113204239
      solve(2, 23798014150529L)
    }

    "work as expected with input file" in {
      val input =
        """Register A: 190384113204239
          |Register B: 0
          |Register C: 0
          |
          |Program: 2,4,1,2,7,5,0,3,4,7,1,7,5,5,3,0""".stripMargin
      runProgram(input) shouldBe "2,4,1,2,7,5,0,3,4,7,1,7,5,5,3,0"
    }
  }
}
