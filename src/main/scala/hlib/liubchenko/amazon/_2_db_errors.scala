package hlib.liubchenko.amazon

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

//
// Databases doesn't support very large numbers, so numbers are stored as a string of binary characters, '0' and '1.
// Accidentally, a ! was entered at some positions and it is unknown whether they should be '0' or ‘1’.
// The string of incorrect data is made up of the characters '0, 1' and ! where !' is the character that got entered
// incorrectly. '!' can be replaced with either '0' or '1'. Due to some internal faults, some errors are generated every
// time '0' and '1' occur together as '01' or 10 'in any subsequence of the string. It is observed that the number of
// errors a subsequence '01' generates is x, while a subsequence '10' generates y errors.
//
// Determine the minimum total errors generated? Since the answer can be very large, return it modulo 10^9+7.
// For example, given string errorString ="101!1", x = 2, y = 3
//
//  - If the '! at index 3 is replaced with '0, the string is "10101". The number of times the subsequence 01 occurs is
//    3 at indices (1, 2), (1, 4), and (3,4). The number of times the subsequence 10 occurs is also 3, indices
//    (0, 1), (0, 3) and (2, 3). The number of errors is 3 * x+3 * y=6 + 9 = 15.
//  - If the ! is replaced with '1, the string is "10111". The subsequence 01 occurs 3 times and 10 occurs 1 time.
//    The number of errors is 3 * x + y = 9.
//
// The minimum number of errors is min(9, 15) modulo (10^9 +7) = 9.
//
class _2_db_errors extends AnyWordSpec with Matchers {
  def getMinErrors(errorString: String, x: Int, y: Int): Int = {
    val zeroReplace = countErrors(errorString.replace('!', '0'), x, y)
    val oneReplace = countErrors(errorString.replace('!', '1'), x, y)
    math.min(zeroReplace, oneReplace)
  }

  private def countErrors(s: String, x: Int, y: Int): Int = {
    val combinations = new Array[(Int, Int)](s.length)
    for (i <- s.length - 1 to 0 by -1) {
      val (prev0, prev1) = if (i + 1 == s.length) (0, 0) else combinations(i + 1)
      combinations(i) = if (s(i) == '0') (1 + prev0, prev1) else (prev0, prev1 + 1)
    }
    val (_01, _02) = s.indices.foldLeft((0, 0)) { case ((_01count, _10count), i) =>
      if (s(i) == '0') (_01count + combinations(i)._2, _10count)
      else (_01count, _10count + combinations(i)._1)
    }
    x * _01 + y * _02
  }

  "countErrors" should {
    "work as expected" in {
      countErrors("10101", 2, 3) == 15
      getMinErrors("10101", 2, 3) == 9
    }
  }
}
