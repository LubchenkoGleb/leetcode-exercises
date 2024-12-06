package hlib.liubchenko.topinterview150.graph.bfs

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _2_minimum_genetic_mutation extends AnyWordSpec with Matchers {
  def minMutation(startGene: String, endGene: String, bank: Array[String]): Int = ???

  "minMutation" should {
    "work as expected" in {
      pending

      minMutation(
        startGene = "AACCGGTT",
        endGene = "AACCGGTA",
        bank = Array("AACCGGTA")
      ) shouldBe 1
      minMutation(
        startGene = "AACCGGTT",
        endGene = "AAACGGTA",
        bank = Array("AACCGGTA", "AACCGCTA", "AAACGGTA")
      ) shouldBe 2
    }
  }
}
