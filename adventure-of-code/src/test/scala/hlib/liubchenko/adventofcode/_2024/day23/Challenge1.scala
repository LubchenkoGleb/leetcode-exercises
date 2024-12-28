package hlib.liubchenko.adventofcode._2024.day23

import hlib.liubchenko.adventofcode._2024.Utils
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Challenge1 extends AnyWordSpec with Matchers {
  def lanGames(lines: List[String]): Int = {
    val connections = lines
      .map(_.split("-"))
      .flatMap { case Array(a, b) => List(a -> b, b -> a) }
      .groupMap(_._1)(_._2)
      .view
      .mapValues(_.toSet)
      .toMap

    val res = collection.mutable.Set.empty[Set[String]]

    for {
      k1 <- connections.keys.filter(_.startsWith("t"))
      k2 <- connections(k1)
      k3 <- connections(k2)
      group = Set(k1, k2, k3)
      if group.size == 3 && !res.contains(group) && connections(k3).contains(k1)
    } {
      res.add(group)
    }

//    res.foreach(println)

    res.size
  }

  "Day #23 Challenge #1" should {
    "work as expected #1" in {
      val input = List(
        "kh-tc",
        "qp-kh",
        "de-cg",
        "ka-co",
        "yn-aq",
        "qp-ub",
        "cg-tb",
        "vc-aq",
        "tb-ka",
        "wh-tc",
        "yn-cg",
        "kh-ub",
        "ta-co",
        "de-co",
        "tc-td",
        "tb-wq",
        "wh-td",
        "ta-ka",
        "td-qp",
        "aq-cg",
        "wq-ub",
        "ub-vc",
        "de-ta",
        "wq-aq",
        "wq-vc",
        "wh-yn",
        "ka-de",
        "kh-ta",
        "co-tc",
        "wh-qp",
        "tb-vc",
        "td-yn"
      )
      lanGames(input) shouldBe 7
    }

    "work as expected #2" in {
      val input = Utils.readInputFile(23)
      lanGames(input) shouldBe 1215
    }
  }
}
