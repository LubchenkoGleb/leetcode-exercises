package hlib.liubchenko

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object _39_happyNumber extends App {

  def next(i: Int) = {
    var (rest, acc) = (i, 0)
    while (rest > 0) {
      val d = rest % 10
      acc += d * d
      rest /= 10
    }
    acc
  }

  def isHappy(n: Int): Boolean = {
    var rest = n
    val seen = new mutable.HashSet[Int]()
    while (rest != 1 && !seen.contains(rest)) {
      seen.add(rest)
      rest = next(rest)
    }

    rest == 1
  }

  def isHappyMagicHardcode(n: Int): Boolean = {
    var rest = n
    while (rest != 1 && rest != 4) {
      rest = next(rest)
    }
    rest == 1
  }

  println(isHappyMagicHardcode(18))
  println(isHappyMagicHardcode(19))

  def printAll(n: Int): Unit = {
    val acc = new ListBuffer[Int]()
    val seen = new mutable.HashSet[Int]()
    var rest = n
    while (rest != 1 && !seen.contains(rest)) {
      seen.add(rest)
      acc.addOne(rest)
      rest = next(rest)
    }
    println(acc)
  }

  printAll(16)
  printAll(17)
  printAll(99999)
}
