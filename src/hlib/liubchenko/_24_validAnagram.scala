package hlib.liubchenko

object _24_validAnagram extends App {

  // sort solution = 2n log n + n (memory optimization)
  // map solution = n + n + n (speed optimization)
  def isAnagram(s: String, t: String): Boolean = {
    s.sorted == t.sorted
  }

  def isAnagram2(s: String, t: String): Boolean = {
    s.groupMapReduce(identity)(_ => 1)(_ + _).hashCode() == t
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .hashCode()
  }

  def isAnagram3(s: String, t: String): Boolean = {
    // 'a'.toInt == 97
    if (s.length != t.length) return false

    val array = new Array[Int](26)

    (0 until s.length).foreach { i =>
      array(s(i) - 97) += 1
      array(t(i) - 97) -= 1
      println(array.toList)
    }

    array.indices.forall(i => array(i) == 0)
  }

  // this is wron but it's interestion to check if leetcode handles this case
  def isAnagram4(s: String, t: String): Boolean = {
    // this is wrong but it's interesting to check if leetcode handles this case
    if (s.length != t.length) return false
    var (sSum, tSum) = (0L, 0L)
    (0 until s.length).foreach { i =>
      val si = s(i) - 97
      val sSq = si * si -7
      sSum += sSq

      val ti = t(i) - 97
      val tSq = ti * ti - 7
      tSum += tSq

      println(s"si: $si, ti: $ti, sSq: $sSq, tSq: $tSq")
    }

    println(s"sSum: $sSum, tSum: $tSum")
    sSum == tSum
  }

  println(isAnagram4("anagram", "nagaram")) // true
  println(isAnagram4("nl", "cx")) // false
  println(isAnagram4("aa", "bb")) // false
  println(isAnagram4("hqbqo", "lsnma")) // false

}
