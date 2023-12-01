package hlib.liubchenko.leetcode75.graph_dfs

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _2_number_of_provinces extends AnyWordSpec with Matchers {
  // My, Runtime 62, Memory 100
  def findCircleNum(isConnected: Array[Array[Int]]): Int = {
    val visited = new Array[Boolean](isConnected.length)

    def travel(node: Int): Unit = {
      visited(node) = true
      isConnected.indices.collect { case i if isConnected(node)(i) == 1 && !visited(i) => i }.foreach(travel)
    }

    def loop(acc: Int): Int = {
      val notVisited = visited.indices.find(!visited(_))
      notVisited.fold(acc) { i =>
        travel(i)
        loop(acc + 1)
      }
    }

    loop(0)
  }

  "findCircleNum" should {
    "work as expected" in {
      val matrix1 = Array(Array(1, 1, 0), Array(1, 1, 0), Array(0, 0, 1))
      findCircleNum(matrix1) shouldBe 2
      val matrix2 = Array(Array(1, 0, 0), Array(0, 1, 0), Array(0, 0, 1))
      findCircleNum(matrix2) shouldBe 3
    }
  }
}
