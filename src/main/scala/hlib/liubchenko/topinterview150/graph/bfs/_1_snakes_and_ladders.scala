package hlib.liubchenko.topinterview150.graph.bfs

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _1_snakes_and_ladders extends AnyWordSpec with Matchers {
  def snakesAndLadders(board: Array[Array[Int]]): Int = ???

  "snakesAndLadders" should {
    "work as expected #1" in {
      snakesAndLadders(
        Array(
          Array(-1, -1, -1, -1, -1, -1),
          Array(-1, -1, -1, -1, -1, -1),
          Array(-1, -1, -1, -1, -1, -1),
          Array(-1, 35, -1, -1, 13, -1),
          Array(-1, -1, -1, -1, -1, -1),
          Array(-1, 15, -1, -1, -1, -1)
        )
      ) shouldBe 4
    }

    "work as expected #2" in {
      snakesAndLadders(
        Array(
          Array(-1, -1),
          Array(-1, 3)
        )
      ) shouldBe 1
    }
  }
}
