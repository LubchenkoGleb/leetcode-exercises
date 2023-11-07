package hlib.liubchenko

import scala.collection.mutable

object _27_containsDuplicate extends App {
  def containsDuplicate(nums: Array[Int]): Boolean = {
    nums.sorted.sliding(2).exists {
      case Array(x, y) => x == y
      case _           => false
    }
  }

  def containsDuplicate2(nums: Array[Int]): Boolean = {
    val set = new mutable.HashSet[Int]()
    !nums.forall(set.add)
  }

  println(containsDuplicate2(Array(1, 2, 3, 4)))
}
