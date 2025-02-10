package hlib.liubchenko.topinterview150.graph.bfs

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _1_snakes_and_ladders extends AnyWordSpec with Matchers {
  def snakesAndLadders(board: Array[Array[Int]]): Int = {
    // 1. iterate over the board
    // 2. for each move update all possible moves

    val n = board.length

    val distances = Array.fill(n, n)(Int.MaxValue)
    distances(n - 1)(0) = 0

    def getPosition(pos: Int) = {
      val invertedPos = (n * n - 1) - (pos - 1)
      val i = invertedPos / n
      val j = if (i % 2 == n % 2) invertedPos % n else n - 1 - invertedPos % n
      (i, j)
    }

    var pos = 1
    while (pos != n * n) {
      val (currI, currJ) = getPosition(pos)

      def update(movesUsed: Int)(next: Int): Boolean = {
        val (nextI, nextJ) = getPosition(next)
        val before = distances(nextI)(nextJ)
        distances(nextI)(nextJ) = math.min(distances(currI)(currJ) + movesUsed, distances(nextI)(nextJ))
        before != distances(nextI)(nextJ)
      }

      (1 to 6).map(_ + pos).filter(_ <= n * n).foreach { update(1) }
      val cheat = board(currI)(currJ)
      val isUsed = if (cheat != -1) update(0)(cheat) else false

      println(s"$pos($currI, $currJ)")
      val distancesStr = distances
        .map(_.map(v => if (v == Int.MaxValue) "." else v.toString).mkString(" "))
        .mkString("", "\n", "\n")
      println(distancesStr)

      if (cheat != -1 && cheat < pos && isUsed) pos = cheat
      else pos += 1
    }

    val (finalI, finalJ) = getPosition(n * n)
    distances(finalI)(finalJ)
  }

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

    "work as expected #3" in {
      snakesAndLadders(
        Array(
          Array(-1, -1, -1),
          Array(-1, 9, 8),
          Array(-1, 8, 9)
        )
      ) shouldBe 1
    }

    "work as expected #4" in {
      snakesAndLadders(
        Array(
          Array(-1, 4, -1),
          Array(6, 2, 6),
          Array(-1, 3, -1)
        )
      ) shouldBe 2
    }

    "work as expected #5" in {
      snakesAndLadders(
        Array(
          Array(1,1,-1),
          Array(1,1,1),
          Array(-1,1,1)
        )
      ) shouldBe -1
    }

  }
}
