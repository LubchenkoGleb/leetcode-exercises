package hlib.liubchenko.topinterview150.graph.bfs

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _1_snakes_and_ladders extends AnyWordSpec with Matchers {
  import scala.annotation.tailrec

  def snakesAndLadders(board: Array[Array[Int]]): Int = {
    val n = board.length

    val distances = Array.fill(n, n)(Int.MaxValue)
    distances(n - 1)(0) = 0

    def getPosition(pos: Int) = {
      val invertedPos = (n * n - 1) - (pos - 1)
      val i = invertedPos / n
      val j = if (i % 2 == n % 2) invertedPos % n else n - 1 - invertedPos % n
      (i, j)
    }

    @tailrec
    def loop(moves: List[Int]): Unit = if (moves.nonEmpty) {
      val pos :: tail = moves
      val (currI, currJ) = getPosition(pos)

      val newMoves = (1 to 6).filter(_ + pos <= n * n).flatMap { shift =>
        var next = pos + shift
        var (nextI, nextJ) = getPosition(next)

        val cheatMaybe = board(nextI)(nextJ)
        if (cheatMaybe != -1) {
          val (cheatI, cheatJ) = getPosition(cheatMaybe)
          next = cheatMaybe; nextI = cheatI; nextJ = cheatJ
        }

        val before = distances(nextI)(nextJ)
        distances(nextI)(nextJ) = math.min(distances(currI)(currJ) + 1, distances(nextI)(nextJ))
        if (before != distances(nextI)(nextJ)) Some(next) else None
      }

      loop(tail ++ newMoves)
    }

    loop(List(1))

    val (finalI, finalJ) = getPosition(n * n)
    val res = distances(finalI)(finalJ)
    if (res == Int.MaxValue) -1 else res
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
          Array(1, 1, -1),
          Array(1, 1, 1),
          Array(-1, 1, 1)
        )
      ) shouldBe -1
    }

    "work as expected #6" in {
      val input = Array(
        Array(-1, 1, 2, -1),
        Array(2, 13, 15, -1),
        Array(-1, 10, -1, -1),
        Array(-1, 6, 2, 8)
      )
      snakesAndLadders(input) shouldBe 2
    }
  }
}
