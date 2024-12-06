package hlib.liubchenko.leetcode75.graph_dfs

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.Console.println

class _1_keys_and_rooms extends AnyWordSpec with Matchers {
  sealed trait Solution {
    def canVisitAllRooms(rooms: List[List[Int]]): Boolean
  }

  // Mine, Runtime 35, Memory 21
  object Solution1 extends Solution {
    import scala.annotation.tailrec
    import scala.collection.mutable

    override def canVisitAllRooms(rooms: List[List[Int]]): Boolean = {
      val openRooms = mutable.Set(0)
      val toCheck = mutable.Stack(0)

      @tailrec
      def loop: Boolean =
        if (openRooms.size == rooms.size) true
        else if (toCheck.isEmpty) false
        else {
          rooms(toCheck.pop()).foreach { newKey =>
            if (openRooms.add(newKey)) toCheck.push(newKey)
          }
          loop
        }

      loop
    }
  }

  // Mine, Runtime 35, Memory 21
  object Solution2 extends Solution {
    import scala.annotation.tailrec
    import scala.collection.mutable

    def canVisitAllRooms(rooms: List[List[Int]]): Boolean = {
      val openRooms = new Array[Boolean](rooms.length)
      openRooms(0) = true
      val toCheck = mutable.Stack(0)

      @tailrec
      def loop(openRoomsCount: Int): Boolean =
        if (openRoomsCount >= rooms.length) true
        else if (toCheck.isEmpty) false
        else {
          val newKeysCount = rooms(toCheck.pop()).count { key =>
            val isNew = !openRooms(key)
            if (isNew) { toCheck.push(key); openRooms(key) = true }
            isNew
          }
          loop(openRoomsCount + newKeysCount)
        }

      loop(1)
    }
  }

  // Mine, Runtime 100, Memory 50
  object Solution3 extends Solution {
    import scala.annotation.tailrec
    import scala.collection.mutable

    def canVisitAllRooms(rooms: List[List[Int]]): Boolean = {
      val roomsArray = rooms.toArray
      val openRooms = new Array[Boolean](roomsArray.length)
      openRooms(0) = true
      val toCheck = mutable.Stack(0)

      @tailrec
      def loop(openRoomsCount: Int): Boolean =
        if (openRoomsCount >= roomsArray.length) true
        else if (toCheck.isEmpty) false
        else {
          val newKeysCount = roomsArray(toCheck.pop()).count { key =>
            val isNew = !openRooms(key)
            if (isNew) {
              toCheck.push(key); openRooms(key) = true
            }
            isNew
          }
          loop(openRoomsCount + newKeysCount)
        }

      loop(1)
    }
  }

  "All solutions" should {
    "work as expected" in {
      List(Solution1, Solution2).map { solution =>
        println(solution)
        solution.canVisitAllRooms(List(List(1), List(2), List(3), List())) shouldBe true
        solution.canVisitAllRooms(List(List(1, 3), List(3, 0, 1), List(2), List(0))) shouldBe false
      }
    }
  }
}
