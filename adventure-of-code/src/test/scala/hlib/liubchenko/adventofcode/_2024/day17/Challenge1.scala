package hlib.liubchenko.adventofcode._2024.day17

import hlib.liubchenko.adventofcode._2024.Utils
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Challenge1 extends AnyWordSpec with Matchers {
  def runProgram(input: String): (Long, Long, Long, String) = {
    var (regA, regB, regC, ops, pointer) = input.split("\n") match {
      case Array(regAStr, regBStr, regCStr, _, instructionsStr) =>
        def parseReg(regStr: String) = regStr.split(" ")(2).toLong
        val ops = instructionsStr.split(" ")(1).split(",").map(_.toInt)
        (parseReg(regAStr), parseReg(regBStr), parseReg(regCStr), ops, 0)
    }
    val output = collection.mutable.ListBuffer.empty[Long]

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

  "Day #17 Challenge #1" should {
    "work as expected #1" in {
      val input = """Register A: 729
                    |Register B: 0
                    |Register C: 0
                    |
                    |Program: 0,1,5,4,3,0""".stripMargin
      runProgram(input) shouldBe (0, 0, 0, "4,6,3,5,6,3,5,2,1,0")
    }

    "work as expected #2" in {
      val input = """Register A: 0
                    |Register B: 0
                    |Register C: 9
                    |
                    |Program: 2,6""".stripMargin
      runProgram(input) shouldBe (0, 1, 9, "")
    }

    "work as expected #3" in {
      val input = """Register A: 10
                    |Register B: 0
                    |Register C: 0
                    |
                    |Program: 5,0,5,1,5,4""".stripMargin
      runProgram(input) shouldBe (10, 0, 0, "0,1,2")
    }

    "work as expected #4" in {
      val input = """Register A: 2024
                    |Register B: 0
                    |Register C: 0
                    |
                    |Program: 0,1,5,4,3,0""".stripMargin
      runProgram(input) shouldBe (0, 0, 0, "4,2,5,6,7,7,7,7,3,1,0")
    }

    "work as expected #5" in {
      val input = """Register A: 0
                    |Register B: 29
                    |Register C: 0
                    |
                    |Program: 1,7""".stripMargin
      runProgram(input) shouldBe (0, 26, 0, "")
    }

    "work as expected #6" in {
      val input = """Register A: 0
                    |Register B: 2024
                    |Register C: 43690
                    |
                    |Program: 4,0""".stripMargin
      runProgram(input) shouldBe (0, 44354, 43690, "")
    }

    "work as expected with input file" in {
      val input = Utils.readInputFile(17).mkString("\n")
      runProgram(input) shouldBe (0, 4, 0, "7,1,3,7,5,1,0,3,4")
    }
  }
}
