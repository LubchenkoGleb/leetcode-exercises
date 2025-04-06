package hlib.liubchenko.topinterview150.array_string

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _14_gas_station extends AnyWordSpec with Matchers {
  def canCompleteCircuit(gas: Array[Int], cost: Array[Int]): Int = {

  }

  "canCompleteCircuit" should {
    "work as expected" in {
      canCompleteCircuit(Array(1, 2, 3, 4, 5), Array(3, 4, 5, 1, 2)) shouldBe 3
      canCompleteCircuit(Array(2, 3, 4), Array(3, 4, 3)) shouldBe -1
    }
  }
}
