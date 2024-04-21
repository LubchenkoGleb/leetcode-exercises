package hlib.liubchenko.leetcode_patterns

object _36_findTheTownJudge extends App {
  def findJudge(n: Int, trust: Array[Array[Int]]): Int = {
    if (n == 1) return 1

    val judgeCandidate = trust.groupMapReduce(_(1))(_ => 1)(_ + _).collectFirst {
      case (person, trustCount) if trustCount == n - 1 => person
    }

    def trustToAnybody(i: Int) = trust.exists(_(0) == i)

    judgeCandidate.filter(!trustToAnybody(_)).getOrElse(-1)
  }

  def findJudge2(n: Int, trust: Array[Array[Int]]): Int = {
    val res = new Array[Int](n)
    trust.foreach { case Array(a, b) =>
      res(b - 1) += 1
      res(a - 1) -= 1
    }
    res.indices.find(i => res(i) == n - 1).map(_ + 1).getOrElse(-1)
  }

  println(findJudge2(4, Array(Array(1, 4), Array(2, 4), Array(3, 4), Array(1, 2), Array(2, 3), Array(1, 3)))) // 4
  println(findJudge2(3, Array(Array(1, 3), Array(2, 3), Array(3, 1)))) // -1
  println(findJudge2(1, Array.empty))
}
