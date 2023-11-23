package hlib.liubchenko.leetcode75.monotonic_stack

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.mutable

class _1_daily_temperatures extends AnyWordSpec with Matchers {
  // Mine, Runtime 36, Memory 100
  def dailyTemperatures(temperatures: Array[Int]): Array[Int] = {
    val stack = mutable.Stack(0)

    val test = for {
      i <- 1 to 5
      j <- 5 to 10
    } yield i
    println(test)

    for {
      i <- (1 until temperatures.length)
      j <- stack.popWhile(temperatures(i) > temperatures(_))
      _ = temperatures(j) = i - j
    } stack.push(i)

    stack.foreach(i => temperatures(i) = 0)

    temperatures
  }

  // Mine, Runtime 36, Memory 100
  def dailyTemperatures_2(temperatures: Array[Int]): Array[Int] = {
    val stack = mutable.Stack(0)

    for (i <- 1 until temperatures.length) {
      for (j <- stack.popWhile(temperatures(i) > temperatures(_))) { temperatures(j) = i - j }
      stack.push(i)
    }
    stack.foreach(i => temperatures(i) = 0)

    temperatures
  }

  def dailyTemperatures_1(temperatures: Array[Int]): Array[Int] = {
    temperatures.indices.foreach { i =>
      temperatures(i) =
        Iterator.range(i, temperatures.length).find(j => temperatures(j) > temperatures(i)).fold(0)(_ - i)
    }
    temperatures
  }

  "dailyTemperatures" should {
    "work as expected" in {
      dailyTemperatures(Array(73, 74, 75, 71, 69, 72, 76, 73)) shouldBe Array(1, 1, 4, 2, 1, 1, 0, 0)
    }
  }
}

//              69`
//           71 71
// 73 74 75  75 75
//
//    i=1
