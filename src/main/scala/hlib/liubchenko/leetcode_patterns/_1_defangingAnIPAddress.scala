package hlib.liubchenko.leetcode_patterns

object _1_defangingAnIPAddress extends App {
  def defangIPaddr(address: String): String = address.replace(".", "[.]")

  println(defangIPaddr("1.1.1.1"))
}
