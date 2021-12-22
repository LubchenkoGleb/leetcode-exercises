package hlib.liubchenko

object _17_nextGreaterElementI extends App {
  def nextGreaterElement(nums1: Array[Int], nums2: Array[Int]): Array[Int] = {
    val greaterElements: Map[Int, Int] = nums2.tails
      .filter(_.nonEmpty)
      .map { arr =>
        arr.head -> arr.tail.find(_ > arr.head).getOrElse(-1)
      }
      .toMap

    nums1.map(greaterElements)
  }

  def nextGreaterElement2(nums1: Array[Int], nums2: Array[Int]): Array[Int] = {
    val greaterElements: Map[Int, Int] = nums2.tails
      .filter(_.nonEmpty)
      .flatMap { arr =>
        arr.tail.find(_ > arr.head).map(arr.head -> _)
      }
      .toMap
      .withDefaultValue(-1)

    nums1.map(greaterElements)
  }

  println(nextGreaterElement(Array(4, 1, 2), Array(1, 3, 4, 2)).toList)
}
