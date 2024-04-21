package hlib.liubchenko.tesco

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TestTask extends AnyWordSpec with Matchers {
//  Find index of the first unique character in the string.
//  Input: "tescostore"
//  Output: 3

  def uniqueChar(s: String): Int = {
    val uniqueChars = s
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .collect { case (c, 1) => c }
      .toSet

    s.collectFirst { case c if uniqueChars.contains(c) => s.indexOf(c) }.getOrElse(-1)
  }

  "uniqueChar" should {
    "work as expected" in {
      uniqueChar("tescostore") shouldBe 3
      uniqueChar("aabbcc") shouldBe -1
      uniqueChar("") shouldBe -1
      uniqueChar("abc") shouldBe 0
      uniqueChar("bac") shouldBe 0
    }
  }


//  Given 2 data sets, one with customer data and another with transactions made by customers, for customers that are 'premium',
//  if the total price of their sales on a given day exceeds 500, apply a 20% discount on the rest of purchases on that day.
//    The discounted price should be in another column.
//
//  Customer data:
//    cust_id,name,p-no,premium
//  101, J, 888, Y
//  102, M, 999, Y
//  103, L, 777, N
//
//  Product data:
//    {"CUST_ID" : 101, "PROD_ID" : "P1", "DT" : "2020-10-02", "TimePurchase" : "10", "price" : 100 }
//  {"CUST_ID" : 102, "PROD_ID" : "P7", "DT" : "2020-10-02", "TimePurchase" : "11", "price" : 420 }
//  {"CUST_ID" : 101, "PROD_ID" : "P4", "DT" : "2020-10-02", "TimePurchase" : "11", "price" : 210 }
//  {"CUST_ID" : 102, "PROD_ID" : "P2", "DT" : "2020-10-02", "TimePurchase" : "13", "price" : 120 }
//  {"CUST_ID" : 102, "PROD_ID" : "P1", "DT" : "2020-10-02", "TimePurchase" : "14", "price" : 20 }
//  {"CUST_ID" : 102, "PROD_ID" : "P7", "DT" : "2020-10-03", "TimePurchase" : "11", "price" : 520 }
//
//
//  Output:
//    +-------+----------+-------+------------+-----+----+-------+------------+
//  |CUST_ID|        DT|PROD_ID|TimePurchase|price|p-no|premium|price_recalc|
//    +-------+----------+-------+------------+-----+----+-------+------------+
//  |    101|2020-10-02|     P1|          10|  100| 888|      Y|       100.0|
//    |    101|2020-10-02|     P4|          11|  210| 888|      Y|       210.0|
//    |    102|2020-10-02|     P7|          11|  420| 999|      Y|       420.0|
//    |    102|2020-10-02|     P2|          13|  120| 999|      Y|       120.0|
//    |    102|2020-10-02|     P1|          14|   20| 999|      Y|        16.0|
//    |    102|2020-10-03|     P7|          11|  520| 999|      Y|       520.0|
//    +-------+----------+-------+------------+-----+----+-------+------------+
}
