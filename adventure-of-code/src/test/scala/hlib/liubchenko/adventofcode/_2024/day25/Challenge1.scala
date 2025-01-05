package hlib.liubchenko.adventofcode._2024.day25

import hlib.liubchenko.adventofcode._2024.Utils
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Challenge1 extends AnyWordSpec with Matchers {
  def method(lines: List[String]): Int = {
    val looks :: keys :: Nil = lines
      .grouped(8)
      .map(_.dropRight(1).transpose)
      .map { d => d.head.head -> d.map(_.count(_ == '#')) }
      .toList
      .groupMap(_._1)(_._2)
      .values
      .toList

    var count = 0
    for {
      l <- looks
      k <- keys
      // _ = println(s"l: ${l.mkString(",")}, k: ${k.mkString(",")}")
      if l.zip(k).forall { case (lP, kP) => lP + kP <= 7 }
    } count += 1

    count
  }

  "Day #25 Challenge #1" should {
    "work as expected #1" in {
      val input = """#####
                    |.####
                    |.####
                    |.####
                    |.#.#.
                    |.#...
                    |.....
                    |
                    |#####
                    |##.##
                    |.#.##
                    |...##
                    |...#.
                    |...#.
                    |.....
                    |
                    |.....
                    |#....
                    |#....
                    |#...#
                    |#.#.#
                    |#.###
                    |#####
                    |
                    |.....
                    |.....
                    |#.#..
                    |###..
                    |###.#
                    |###.#
                    |#####
                    |
                    |.....
                    |.....
                    |.....
                    |#....
                    |#.#..
                    |#.#.#
                    |#####""".stripMargin
      method(input.split("\n").toList) shouldBe 3
    }

    "work as expected #2" in {
      val input = Utils.readInputFile(25)
      method(input) shouldBe 0
    }
  }
}
