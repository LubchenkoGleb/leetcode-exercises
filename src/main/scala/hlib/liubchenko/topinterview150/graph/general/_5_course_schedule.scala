package hlib.liubchenko.topinterview150.graph.general

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _5_course_schedule extends AnyWordSpec with Matchers {

  def canFinish(numCourses: Int, prerequisites: Array[Array[Int]]): Boolean = ???

  "canFinish" should {
    "work as expected" in {
      canFinish(2, Array(Array(1, 0))) shouldBe true
      canFinish(2, Array(Array(1, 0), Array(0, 1))) shouldBe false
    }
  }

}
