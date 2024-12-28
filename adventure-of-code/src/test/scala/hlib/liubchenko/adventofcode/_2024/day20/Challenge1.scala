//package hlib.liubchenko.adventofcode._2024.day20
//
//import hlib.liubchenko.adventofcode._2024.Utils
//import org.scalatest.matchers.should.Matchers
//import org.scalatest.wordspec.AnyWordSpec
//
//class Challenge1 extends AnyWordSpec with Matchers {
//  def findPath(lines: List[String]): Int = {
////    val map = Array.fill(height)(Array.fill(width)(Int.MaxValue))
////    coordinates.take(corruptedBytes).map { _.split(",") }.foreach { case Array(x, y) => map(y.toInt)(x.toInt) = -1 }
////    map(0)(0) = 1
//
//    val (wall, start, end) = (-1, -2, -3)
//
//    val map = lines
//      .map(_.map {
//        case '.' => Int.MaxValue
//        case '#' => wall
//        case 'S' => start
//        case 'E' => end
//      }.toArray)
//      .toArray
//
//    def findElement(element: Int) = (for {
//      i <- map.indices.iterator
//      j <- map.head.indices.iterator
//      if map(i)(j) == element
//    } yield (i, j)).next()
//
//    val (startY, startX) = findElement(start)
//    val (endY, endX) = findElement(start)
//
//    def find() = {
//      val stack = collection.mutable.Stack((0, 0))
//
//      while (map(endY)(endX) == end && stack.nonEmpty) {
//        val (x, y) = stack.pop()
//        def update(xM: Int, yM: Int): Unit = {
//          val inBorder = x + xM >= 0 && x + xM < width && y + yM >= 0 && y + yM < height
//          lazy val element = map(y + yM)(x + xM)
//          if (inBorder && element != -1) {
//            map(y + yM)(x + xM) = math.min(map(y)(x) + 1, element)
//            if (map(y + yM)(x + xM) != element) stack.push((x + xM, y + yM))
//          }
//        }
//
//        List((1, 0), (0, 1), (-1, 0), (0, -1)).foreach { case (x, y) => update(x, y) }
//      }
//    }
//
////
////    map(height - 1)(width - 1) - 1
//    0
//  }
//
//  "Day #20 Challenge #1" should {
//    "work as expected #1" in {
//      val input = List(
//        "###############",
//        "#...#...#.....#",
//        "#.#.#.#.#.###.#",
//        "#S#...#.#.#...#",
//        "#######.#.#.###",
//        "#######.#.#...#",
//        "#######.#.###.#",
//        "###..E#...#...#",
//        "###.#######.###",
//        "#...###...#...#",
//        "#.#####.#.###.#",
//        "#.#...#.#.#...#",
//        "#.#.#.#.#.#.###",
//        "#...#...#...###",
//        "###############"
//      )
//      findPath(input) shouldBe 0
//    }
//
//    "work as expected #2" in {
//      val input = Utils.readInputFile(20)
////      method(input) shouldBe 0
//    }
//  }
//}
