package hlib.liubchenko.leetcode75.stack

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _2_asteroid_collision extends AnyWordSpec with Matchers {
  // positive >, negative <
  def asteroidCollision(asteroids: Array[Int]): Array[Int] = {
    import scala.annotation.tailrec
    import scala.collection.mutable

    val stack = mutable.Stack.empty[Int]

    @tailrec
    def loop(i: Int): Unit = if (i != asteroids.length) {
      val (cM, hM) = (asteroids(i), stack.head)
      if (stack.nonEmpty && hM > 0 && cM < 0)
        if (hM < math.abs(cM)) { stack.pop(); loop(i) }
        else if (hM == math.abs(cM)) { stack.pop(); loop(i + 1) }
        else loop(i + 1)
      else {
        stack.push(cM); loop(i + 1)
      }
    }

//    asteroids.foreach { m =>
//      if (stack.isEmpty || !(stack.head > 0 && m < 0)) stack.push(m)
//      else stack.dropWhileInPlace { h => h > 0 && m < 0 && h <= math.abs(m) }
//      println(s"m: $m, stack: [${stack.mkString(", ")}]")
//    }

    loop(0)
    stack.reverse.toArray
  }

  "asteroidCollision" should {
    "work as expected" in {
      asteroidCollision(Array(5, 10, -5)) shouldBe Array(5, 10)
      asteroidCollision(Array(8, -8)) shouldBe Array.empty
      asteroidCollision(Array(10, 2, -5)) shouldBe Array(10)
      asteroidCollision(Array(-2, -1, 1, 2)) shouldBe Array(-2, -1, 1, 2)
      asteroidCollision(Array(-2, -2, 1, -2)) shouldBe Array(-2, -2, -2)
    }
  }

}
