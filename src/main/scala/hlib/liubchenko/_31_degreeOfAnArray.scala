package hlib.liubchenko

object _31_degreeOfAnArray extends App {
  def findShortestSubArray(nums: Array[Long]): Int = {
    type Count = Int
    type From = Int
    type To = Int
    type Acc = (Count, From, To)
    def zeroAcc(i: Int): Acc = (1, i, i)

    implicit val ordering: Ordering[Acc] = Ordering
      .by[Acc, Count] { case (count, _, _) => count }
      .reverse
      .orElse(Ordering.by { case (_, from, to) => to - from })

    val (_, from, to) = nums.zipWithIndex
      .groupMapReduce(_._1)(p => zeroAcc(p._2))((l, r) => (l._1 + r._1, l._2, r._3))
      .values
      .min

    to - from + 1
  }

  println(findShortestSubArray(Array(1, 2, 2, 3, 1)))
  println(findShortestSubArray(Array(1, 2, 2, 3, 1, 4, 2)))
}
