package hlib.liubchenko.topinterview150.graph.general

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _6_course_schedule_2 extends AnyWordSpec with Matchers {
  def findOrder(numCourses: Int, prerequisites: Array[Array[Int]]): Array[Int] = ???

  "findOrder" should {
    "work as expected" in {
      findOrder(2, Array(Array(1, 0))) shouldBe Array(0, 1)
      findOrder(4, Array(Array(1, 0), Array(2, 0), Array(3, 1), Array(3, 2))) shouldBe Array(0, 2, 1, 3)
    }
  }
}
