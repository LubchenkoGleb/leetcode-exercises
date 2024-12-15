package hlib.liubchenko.adventofcode._2024.day14

import hlib.liubchenko.adventofcode._2024.Utils
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Challenge2 extends AnyWordSpec with Matchers {
  def countRobots(robots: List[String], width: Int, height: Int, seconds: Int): Int = {
    val regex = "p=(\\d+),(\\d+) v=([-\\d]+),([-\\d]+)".r

    val originalState = robots
      .map { case regex(x, y, vX, vY) => (x.toInt, y.toInt, vX.toInt, vY.toInt) }

    (0 to seconds)
      .find { seconds =>
        val parsedRobots = originalState
          .map { case (x, y, vX, vY) =>
            val fX = (x + seconds * vX) % width
            val fY = (y + seconds * vY) % height
            (if (fX >= 0) fX else width + fX, if (fY >= 0) fY else height + fY)
          }

        val map = Array.fill(height)(Array.fill(width)('.'))
        parsedRobots.foreach { case (x, y) => map(y)(x) = 'x' }
        val tree = map.map(_.mkString).mkString("\n")
        tree.contains("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
      }
      .getOrElse(seconds)
  }

  "Day #14 Challenge #2" should {
    //                         x
    //                              x
    //                                                                                     x         x
    //                                              x                                         x
    //
    // x             x
    //
    //      x     x                                                       x                   x
    //x                                                                         x
    //                                          x
    //
    //                                                                              x                x
    //                                x   x               x                              x
    //                    x
    //                       x                                                        x
    //           x           x                     x
    //                                                   x          x
    //                  x           x                                                                   x
    //
    //                 x                           x
    //                x  x                                x
    //
    //
    //                            x x     xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    // x                                  x                             x
    //                                    x                             x
    //   x                                x                             x                          x
    //                          x         x                             x                   x
    //                                    x              x              x
    //                               x    x             xxx             x
    //                                    x            xxxxx            x
    //                                    x           xxxxxxx           x                      x        x
    //                                    x          xxxxxxxxx          x            x                x
    //                                    x            xxxxx            x        x
    //                                    x           xxxxxxx           x
    //                     x            x x          xxxxxxxxx          x x                            x
    //                                    x         xxxxxxxxxxx         x
    //                                    x        xxxxxxxxxxxxx        x
    //                                    x          xxxxxxxxx          x x                  xx
    //                                    x         xxxxxxxxxxx         x
    //                                    x        xxxxxxxxxxxxx        x
    //                      x             x       xxxxxxxxxxxxxxx       x
    //       x                            x      xxxxxxxxxxxxxxxxx      x
    //           x  x                     x        xxxxxxxxxxxxx        x                x
    //                                    x       xxxxxxxxxxxxxxx       x                           x
    //                                    x      xxxxxxxxxxxxxxxxx      x     x                 x
    //      x                             x     xxxxxxxxxxxxxxxxxxx     x          x
    //                x                   x    xxxxxxxxxxxxxxxxxxxxx    x                   x
    //                                  x x             xxx             x
    //                                    x             xxx             x      x
    //                                    x             xxx             x
    //                                    x                             x
    //                            x       x                             x
    //                                    x                             x
    //               x                    x                             x              x
    //              x         x           xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx                     x
    //                           x                                        x
    //                    x     x                x                          x
    //                            x
    //                                                                                               x
    //                         x                                      x
    //                                                                         x   x
    //                                            x
    //                                                                             x        x
    //
    //
    //   x                                            x              x
    //
    //
    //                                                                                    x            x
    //                                                                  x
    //                                x                       x
    //
    //                      x
    //                              x            x
    //                                                              x
    //                                                                x
    //                                      x                            x                    x
    //
    //
    //                                                                         x                    x
    //                                    x       x            x       x                         x
    //
    //                                   x                                x
    //                                                                          x
    //                                                                     x         x                 x
    //                                        x           x                   x
    //                                                                 x    x
    //                                           x
    //
    // x    x                                                                            x
    //                                                         x
    //                                                                                          x
    //                                   x                                                        x
    //                        x                                      x                          x x
    //       x                                                                  x                         x
    //
    //
    //                                                                  x                                x
    //                                                                         x
    //                          x
    //                                                                 x
    //           x
    "work as expected #2" in {
      // Randomly guessed, not a real solution :(

      val input = Utils.readInputFile(14)
      countRobots(input, 101, 103, 100000) shouldBe 7093
    }
  }
}
