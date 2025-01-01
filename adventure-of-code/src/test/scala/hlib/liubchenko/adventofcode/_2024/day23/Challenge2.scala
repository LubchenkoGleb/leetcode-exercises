package hlib.liubchenko.adventofcode._2024.day23

import hlib.liubchenko.adventofcode._2024.Utils
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.mutable

class Challenge2 extends AnyWordSpec with Matchers {
  def lanGames(lines: List[String]): String = {
    def bronKerbosch(
        current: Set[String],
        potential: Set[String],
        graph: Map[String, Set[String]],
        largestClique: mutable.Set[String]
    ): Unit = {
      if (potential.isEmpty) {
        // If P and X are empty, R is a maximal clique
        if (current.size > largestClique.size) {
          largestClique.clear()
          largestClique ++= current
        }
      } else {
        // Choose a pivot vertex (from P ∪ X) that maximizes |neighbors ∩ P|
        val pivot = potential.maxBy(graph(_).intersect(potential).size)

        // Exclude neighbors of the pivot from P
        val nonNeighbors = potential -- graph(pivot)

        for (v <- nonNeighbors) { bronKerbosch(current + v, potential.intersect(graph(v)), graph, largestClique) }
      }
    }

    val graph = lines
      .map(_.split("-"))
      .flatMap { case Array(a, b) => List(a -> b, b -> a) }
      .toSet
      .groupMap[String, String](_._1)(_._2)

    val largestClique = mutable.Set.empty[String]
    bronKerbosch(Set.empty, graph.keySet, graph, largestClique)
    largestClique.toList.sorted.mkString(",")
  }

  "Day #23 Challenge #1" should {
    "work as expected #0" in {
      val input = List(
        "A-B",
        "A-D",
        "B-C",
        "B-D",
        "B-E",
        "C-F",
        "D-E",
        "E-F",
        "E-G",
        "G-H"
      )
      lanGames(input) shouldBe "B,D,E"
    }

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
      lanGames(input) shouldBe "co,de,ka,ta"
    }

    "work as expected #2" in {
      val input = Utils.readInputFile(23)
      lanGames(input) shouldBe "bm,by,dv,ep,ia,ja,jb,ks,lv,ol,oy,uz,yt"
    }
  }
}
