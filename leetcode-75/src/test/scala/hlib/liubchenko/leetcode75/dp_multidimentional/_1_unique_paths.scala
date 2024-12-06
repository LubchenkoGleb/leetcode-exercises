package hlib.liubchenko.leetcode75.dp_multidimentional

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _1_unique_paths extends AnyWordSpec with Matchers {
  def uniquePaths_1(m: Int, n: Int): Int = {
    def loop(currM: Int, currN: Int): Int = {
      if (currN == n) 1
      else if (currM == m) 1
      else {
        val res = loop(currM, currN + 1) + loop(currM + 1, currN)
        println(s"res: $res, currM: $currM, currN: $currN")
        res
      }
    }

    loop(1, 1)
  }

  def uniquePaths_2(m: Int, n: Int): Int = {
    val paths = Array.fill(n)(Array.fill(m)(0))

    def loop(currM: Int, currN: Int): Int = {
      println(s"loop(currM: $currM, currN: $currN, ${paths.map(_.mkString("[", ",", "]")).mkString("[", ",", "]")}")

      if (currN == n - 1 || currM == m - 1) { paths(currN)(currM) = 1; 1 }
      else if (paths(currN)(currM) != 0) paths(currN)(currM)
      else {
        val res = loop(currM, currN + 1) + loop(currM + 1, currN)
        paths(currN)(currM) = res
        res
      }
    }

    loop(0, 0)

//    def loop(currM: Int, currN: Int): Unit = {
//      println(s"loop(currM: $currM, currN: $currN, ${paths.map(_.mkString("[", ",", "]")).mkString("[", ",", "]")}")
//      if (currN == n - 1 || currM == m - 1) paths(currN)(currM) = 1
//      else {
//        if (paths(currN)(currM) == 0) {
//          loop(currM, currN + 1)
//          loop(currM + 1, currN)
//          paths(currN)(currM) = paths(currN + 1)(currM) + paths(currN)(currM + 1)
//        }
//      }
//    }

  }

  def uniquePaths(m: Int, n: Int): Int = {
    val paths = Array.ofDim[Int](m, n)
    for (i <- 0 until m; j <- 0 until n) {
      if (i == 0 || j == 0) paths(i)(j) = 1
      else paths(i)(j) = paths(i - 1)(j) + paths(i)(j - 1)
    }
    paths(m - 1)(n - 1)
  }

  "uniquePaths" should {
    "work as expected" in {
      uniquePaths(3, 7) shouldBe 28
//      uniquePaths(3, 2) shouldBe 3
    }
  }
}
