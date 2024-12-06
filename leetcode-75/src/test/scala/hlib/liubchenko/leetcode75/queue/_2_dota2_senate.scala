package hlib.liubchenko.leetcode75.queue

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.mutable

class _2_dota2_senate extends AnyWordSpec with Matchers {
  // Not my idea, Runtime 55, memory 11
  def predictPartyVictory(senate: String): String = {
    val (radQ, dirQ) = (mutable.Queue.empty[Int], mutable.Queue.empty[Int])
    senate.zipWithIndex.foreach {
      case ('R', i) => radQ.enqueue(i)
      case ('D', i) => dirQ.enqueue(i)
    }
    var nextI = senate.length
    while (radQ.nonEmpty && dirQ.nonEmpty) {
      if (radQ.dequeue() < dirQ.dequeue()) radQ.enqueue(nextI)
      else dirQ.enqueue(nextI)
      nextI += 1
    }
    if (radQ.isEmpty) "Dire" else "Radiant"
  }

  // My, Runtime 88, memory 33
  def predictPartyVictory_2(senate: String): String = {
    var (rRemain, dRemain) = (senate.count(_ == 'R'), senate.count(_ == 'D'))
    val remainQ = mutable.Queue.from(senate)
    val banQ = mutable.Queue.empty[Char]

    while (rRemain != 0 && dRemain != 0) {
      val c = remainQ.dequeue
      if (banQ.isEmpty || banQ.head != c) { banQ.enqueue(if (c == 'R') 'D' else 'R'); remainQ.enqueue(c) }
      else { banQ.dequeue(); if (c == 'R') rRemain -= 1 else dRemain -= 1 }
    }

    if (rRemain == 0) "Dire" else "Radiant"
  }

  "predictPartyVictory" should {
    "work as expected" in {
      predictPartyVictory("RD") shouldBe "Radiant"
      predictPartyVictory("RDD") shouldBe "Dire"
      predictPartyVictory("DDRRR") shouldBe "Dire"
    }
  }
}
