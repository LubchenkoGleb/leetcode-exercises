package hlib.liubchenko.topinterview150.graph.bfs

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _1_snakes_and_ladders extends AnyWordSpec with Matchers {
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

    var pos = 1
    var isLoop = false
    while (pos != n * n && !isLoop) {
      val (currI, currJ) = getPosition(pos)

      def update(moves: Int): (Boolean, Int) = {
        var next = pos + moves
        var (nextI, nextJ) = getPosition(next)
        if (board(nextI)(nextJ) != -1) {
          distances(nextI)(nextJ) = math.min(distances(currI)(currJ) + 1, distances(nextI)(nextJ))
          next = board(nextI)(nextJ)
          val nextCord = getPosition(next)
          nextI = nextCord._1; nextJ = nextCord._2
        }

        val before = distances(nextI)(nextJ)
        distances(nextI)(nextJ) = math.min(distances(currI)(currJ) + 1, distances(nextI)(nextJ))
        val updated = before != distances(nextI)(nextJ)

        updated -> next
      }

      val nextMoves = (1 to 6)
        .filter(_ + pos <= n * n)
        .map(update)

      val nextMove = nextMoves.collect { case (updated, newPos) if updated || newPos > pos => newPos }.minOption

      nextMove.minOption match {
        case Some(value) => pos = value
        case None        => isLoop = true
      }

      println(s"$pos($currI, $currJ)")
      val distancesStr = distances
        .map(_.map(v => if (v == Int.MaxValue) "." else v.toString).mkString(" "))
        .mkString("", "\n", "\n")
      println(distancesStr)
    }

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
