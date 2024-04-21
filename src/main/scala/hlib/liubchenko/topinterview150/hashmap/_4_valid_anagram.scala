package hlib.liubchenko.topinterview150.hashmap

class _4_valid_anagram {
  def isAnagram2(s: String, t: String): Boolean = {
    def countChars(s: String) = s.groupMapReduce(identity)(_ => 1)(_ + _)
    if (s.length != t.length) false
    else countChars(s) == countChars(t)
  }

  def isAnagram(s: String, t: String): Boolean = {
    def toIndex(c: Char) = c - 'a'
    if (s.length != t.length) false
    else {
      val count = new Array[Int](26)
      s.foreach(c => count(toIndex(c)) += 1)
      t.foreach(c => count(toIndex(c)) -= 1)
      count.forall(_ == 0)
    }
  }
}
