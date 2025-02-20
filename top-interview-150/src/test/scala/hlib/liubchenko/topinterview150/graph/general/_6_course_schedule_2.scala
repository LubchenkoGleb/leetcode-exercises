package hlib.liubchenko.topinterview150.graph.general

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _6_course_schedule_2 extends AnyWordSpec with Matchers {
  import scala.annotation.tailrec
  import scala.collection.mutable

  def findOrder(numCourses: Int, prerequisites: Array[Array[Int]]): Array[Int] = {
    val courses = (0 until numCourses).map(_ => mutable.Buffer.empty[Int]).toArray
    prerequisites.foreach { case Array(a, b) => courses(a).append(b) }

    @tailrec
    def loop(available: Iterable[Int], ordered: List[Int]): List[Int] =
      if (ordered.length == numCourses) ordered
      else
        available.find(courses(_).forall(ordered.contains)) match {
          case Some(value) => loop(available.filterNot(_ == value), ordered :+ value)
          case None        => List.empty
        }

    loop(0 until numCourses, Nil).toArray
  }


  "findOrder" should {
    "work as expected" in {
      findOrder(2, Array(Array(1, 0))) shouldBe Array(0, 1)
      findOrder(4, Array(Array(1, 0), Array(2, 0), Array(3, 1), Array(3, 2))) shouldBe Array(0, 1, 2, 3)
      findOrder(2, Array(Array(0, 1))) shouldBe Array(1, 0)
    }
  }
}
