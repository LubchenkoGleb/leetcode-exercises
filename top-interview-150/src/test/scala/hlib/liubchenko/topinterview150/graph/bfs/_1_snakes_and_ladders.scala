package hlib.liubchenko.topinterview150.graph.bfs

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.annotation.tailrec
import scala.collection.mutable

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

    val stack = mutable.Stack(1)

    @tailrec
    def loop(): Unit = if (stack.nonEmpty) {
      val pos = stack.pop()
      val (currI, currJ) = getPosition(pos)

      (1 to 6).filter(_ + pos <= n * n).foreach { shift =>
        val next = pos + shift
        val (nextI, nextJ) = getPosition(next)

        def update(_next: Int) = {
          val (_nextI, _nextJ) = getPosition(_next)
          val before = distances(_nextI)(_nextJ)
          distances(_nextI)(_nextJ) = math.min(distances(currI)(currJ) + 1, distances(_nextI)(_nextJ))
          if (before != distances(_nextI)(_nextJ)) stack.push(_next)
        }

        if (board(nextI)(nextJ) != -1) update(board(nextI)(nextJ))
        else update(next)
      }

      println(s"$pos($currI, $currJ)")
      val distancesStr = distances
        .map(_.map(v => if (v == Int.MaxValue) "." else v.toString).mkString(" "))
        .mkString("", "\n", "\n")
      println(distancesStr)

      loop()
    }

    loop()

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
