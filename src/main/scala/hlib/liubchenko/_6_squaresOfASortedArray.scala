package hlib.liubchenko

object _6_squaresOfASortedArray extends App {
  def sortedSquares(nums: Array[Int]): Array[Int] = {
    nums.map(i => i * i).sorted
  }
}
