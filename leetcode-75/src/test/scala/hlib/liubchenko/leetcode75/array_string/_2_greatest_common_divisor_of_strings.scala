package hlib.liubchenko.leetcode75.array_string

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _2_greatest_common_divisor_of_strings extends AnyWordSpec with Matchers {
  private sealed trait Solution {
    def gcdOfStrings(str1: String, str2: String): String
  }
  private sealed trait Divisors {
    def divisors(str: String): IndexedSeq[String]
  }

  // Mine, Runtime 61, Memory 8
  private object Solution1 extends Solution with Divisors {
    def divisors(str: String): IndexedSeq[String] = (1 to str.length)
      .filter(str.length % _ == 0)
      .map(str.take)
      .filter { prefix =>
        prefix * (str.length / prefix.length) == str
      }

    def gcdOfStrings(str1: String, str2: String): String = {
      val str1Divisors = divisors(str1)
      val str2Divisors = divisors(str2)
      str1Divisors.intersect(str2Divisors).lastOption.getOrElse("")
    }
  }

  // Mine, Runtime 15, Memory 8
  private object Solution2 extends Solution with Divisors {
    def divisors(str: String): IndexedSeq[String] = (1 to (str.length / 2))
      .filter(str.length % _ == 0)
      .map(str.take)
      .filter { prefix =>
        str.sliding(prefix.length, prefix.length).forall(_ == prefix)
      } :+ str

    def gcdOfStrings(str1: String, str2: String): String = {
      val str1Divisors = divisors(str1)
      val str2Divisors = divisors(str2)
      str1Divisors.intersect(str2Divisors).lastOption.getOrElse("")
    }
  }

  // Mine, Runtime 50, Memory 28
  private object Solution3 extends Solution with Divisors {
    def divisors(str: String): IndexedSeq[String] = (1 to (str.length / 2))
      .filter(str.length % _ == 0)
      .map(str.take)
      .filter { prefix =>
        (0 until (str.length / prefix.length)).forall(i => str.startsWith(prefix, prefix.length * i))
      } :+ str

    def gcdOfStrings(str1: String, str2: String): String = {
      val str1Divisors = divisors(str1)
      val str2Divisors = divisors(str2)
      str1Divisors.intersect(str2Divisors).lastOption.getOrElse("")
    }
  }

  // Mine, Runtime 24, Memory 47
  private object Solution4 extends Solution {
    private def divisorsIndx(str: String) =
      str.length +: ((str.length / 2) to 1 by -1).filter(str.length % _ == 0)
    private def isDivisor(str: String, i: Int) = {
      val prefix = str.take(i)
      Option.when((0 until (str.length / i)).forall(j => str.startsWith(prefix, i * j)))(prefix)
    }

    def gcdOfStrings(str1: String, str2: String): String = divisorsIndx(str1)
      .intersect(divisorsIndx(str2))
      .find { i =>
        isDivisor(str1, i).zip(isDivisor(str2, i)).exists { case (a, b) => a == b }
      }
      .map(str1.take)
      .getOrElse("")
  }

  // Mine, Runtime 54, Memory 92
  //       Runtime 64, Memory 38
  private object Solution5 extends Solution {
    private def divisorsIndx(str: String) =
      str.length +: ((str.length / 2) to 1 by -1).filter(str.length % _ == 0)
    private def divisor(str: String, i: Int): Option[String] = {
      val prefix = str.take(i)
      Option.when((0 until (str.length / i)).forall(j => str.startsWith(prefix, i * j)))(prefix)
    }

    def gcdOfStrings(str1: String, str2: String): String = divisorsIndx(str1)
      .intersect(divisorsIndx(str2))
      .collectFirst {
        case i if divisor(str1, i).zip(divisor(str2, i)).exists { case (a, b) => a == b } =>
          str1.take(i)
      }
      .getOrElse("")
  }

  // Mine, Runtime 49, Memory 38
  private object Solution6 extends Solution {
    private def divisorsIndx(str: String) =
      str.length +: ((str.length / 2) to 1 by -1).filter(str.length % _ == 0)

    private def divisor(str: String, i: Int): Option[String] = {
      val prefix = str.take(i)
      Option.when((0 until (str.length / i)).forall(j => str.startsWith(prefix, i * j)))(prefix)
    }

    def gcdOfStrings(str1: String, str2: String): String = divisorsIndx(str1)
      .intersect(divisorsIndx(str2))
      .find { i =>
        divisor(str1, i).zip(divisor(str2, i)).exists { case (a, b) => a == b }
      }
      .fold("")(str1.take)
  }

  // Not mine, Runtime 89, 38
  private object Solution7 extends Solution {
    import scala.annotation.tailrec

    @tailrec
    private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

    def gcdOfStrings(str1: String, str2: String): String =
      if (str1 ++ str2 == str2 ++ str1) str1.substring(0, gcd(str1.length, str2.length))
      else "";
  }

  // Not mine, Runtime 89, 38
  private object Solution8 extends Solution {
    import scala.annotation.tailrec

    @tailrec
    private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

    def gcdOfStrings(str1: String, str2: String): String = {
      val hasCommonDivisor = (0 until (str1.length + str2.length)).forall { i =>
        val s1 = if (i >= str1.length) str2(i - str1.length) else str1(i)
        val s2 = if (i >= str2.length) str1(i - str2.length) else str2(i)
        s1 == s2
      }

      if (hasCommonDivisor) str1.substring(0, gcd(str1.length, str2.length)) else ""
    }
  }

  "divisors" should {
    "work as expected" in {
      List(Solution1, Solution2, Solution3).foreach { solution =>
        println(solution)
        solution.divisors("ABABABAB").toList shouldBe List("AB", "ABAB", "ABABABAB")
      }
      List(Solution1, Solution2, Solution3, Solution4, Solution5, Solution6, Solution7, Solution8).foreach { solution =>
        println(solution)
        solution.gcdOfStrings("ABCABC", "ABC") shouldBe "ABC"
        solution.gcdOfStrings("ABABAB", "ABAB") shouldBe "AB"
        solution.gcdOfStrings("LEET", "CODE") shouldBe ""
      }
    }
  }
}
