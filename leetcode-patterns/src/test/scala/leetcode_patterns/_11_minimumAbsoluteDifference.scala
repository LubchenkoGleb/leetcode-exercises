package leetcode_patterns

import java.time.LocalDateTime
import scala.io.Source

object _11_minimumAbsoluteDifference extends App {
  def minimumAbsDifference(arr: Array[Int]): List[List[Int]] = {
    arr.sorted
      .sliding(2)
      .foldLeft((Int.MaxValue, List.empty[Array[Int]])) {
        case ((currDiff, pairsAcc), arr @ Array(x, y)) =>
          val minDiff = math.abs(x - y)
          if (minDiff < currDiff) (minDiff, List(arr))
          else if (minDiff == currDiff) (minDiff, pairsAcc :+ arr)
          else (currDiff, pairsAcc)
      }
      ._2
      .map(_.toList.sorted)
      .sortBy(_.head)
  }

  def minimumAbsDifferenceMemoryOpt(arr: Array[Int]): List[List[Int]] = {
    val minDiff = arr
      .combinations(2)
      .foldLeft(Int.MaxValue) { case (currDiff, Array(x, y)) =>
        val minDiff = math.abs(x - y)
        if (minDiff < currDiff) minDiff
        else currDiff
      }

    arr
      .combinations(2)
      .collect {
        case Array(x, y) if math.abs(x - y) == minDiff =>
          if (x < y) List(x, y) else List(y, x)
      }
      .toList
      .sortBy(_.head)
  }

  def minimumAbsDifferenceMemoryOptEvenWithoutPatternMatch(
      arr: Array[Int]
  ): List[List[Int]] = {
    val minDiff = arr
      .combinations(2)
      .foldLeft(Int.MaxValue) { case (currDiff, arr) =>
        val minDiff = math.abs(arr(0) - arr(1))
        if (minDiff < currDiff) minDiff
        else currDiff
      }

    arr
      .combinations(2)
      .collect {
        case arr if math.abs(arr(0) - arr(1)) == minDiff =>
          if (arr(0) < arr(1)) List(arr(0), arr(1)) else List(arr(1), arr(0))
      }
      .toList
      .sortBy(_.head)
  }

  def minimumAbsDifferenceSliding(
      arr: Array[Int]
  ): List[List[Int]] = {
    val sortedArray = arr.sorted

    val minDiff = sortedArray
      .sliding(2)
      .foldLeft(Int.MaxValue) { case (currDiff, arr) =>
        val minDiff = math.abs(arr(0) - arr(1))
        if (minDiff < currDiff) minDiff
        else currDiff
      }

    sortedArray
      .sliding(2)
      .collect {
        case arr if math.abs(arr(0) - arr(1)) == minDiff =>
          if (arr(0) < arr(1)) List(arr(0), arr(1)) else List(arr(1), arr(0))
      }
      .toList
      .sortBy(_.head)
  }

//  val input = Array(3, 8, -10, 23, 19, -4, -14, 27)
//  val input = Array(4, 2, 1, 3)
  val input = {
    val s = Source.fromFile("src/resources/_11_input.txt")
    val lines = s
      .getLines()
      .flatMap(_.split(","))
      .map(_.toInt)
      .toArray
    s.close()
    lines
  }

  println(s"Before: ${LocalDateTime.now()}")
  println(minimumAbsDifferenceSliding(input))
  println(s"After: ${LocalDateTime.now()}")
}
