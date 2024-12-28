package hlib.liubchenko.adventofcode._2024.day23

import hlib.liubchenko.adventofcode._2024.Utils
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.mutable

class Challenge2 extends AnyWordSpec with Matchers {
  def lanGames(lines: List[String]): String = {
    val connections = lines
      .map(_.split("-"))
      .map { case Array(a, b) => a -> b }

    case class Graph(adjList: Map[String, Set[String]]) {
      def neighbors(vertex: String): Set[String] = adjList.getOrElse(vertex, Set.empty)
      def vertices: Set[String] = adjList.keySet
    }
    object Graph {
      def apply(edges: List[(String, String)]): Graph = {
        val adjList = edges.foldLeft(Map.empty[String, Set[String]]) { case (acc, (u, v)) =>
          acc
            .updated(u, acc.getOrElse(u, Set()) + v)
            .updated(v, acc.getOrElse(v, Set()) + u)
        }
        new Graph(adjList)
      }
    }

    def bronKerbosch(
        R: Set[String],
        possibleVertices: Set[String],
        excludedVertices: Set[String],
        graph: Graph,
        cliques: mutable.Set[Set[String]]
    ): Unit = {
      if (possibleVertices.isEmpty && excludedVertices.isEmpty) {
        cliques.add(R) // Found a maximal clique
      } else {
        possibleVertices.foreach { v =>
          bronKerbosch(
            R + v,
            possibleVertices.intersect(graph.neighbors(v)),
            excludedVertices.intersect(graph.neighbors(v)),
            graph,
            cliques
          )
          // Move v from P to X
          bronKerbosch(R, possibleVertices - v, excludedVertices + v, graph, cliques)
        }
      }
    }

    def findLargestClique(graph: Graph): Set[String] = {
      val cliques = scala.collection.mutable.Set[Set[String]]()
      bronKerbosch(Set.empty, graph.vertices, Set.empty, graph, cliques)
      cliques.maxBy(_.size)
    }

    val graph = Graph(connections)
    findLargestClique(graph).toList.sorted.mkString(",")
//    val elements =connections.flatten.distinct.zipWithIndex.toMap
//    val matrix = Array.fill(elements.size, elements.size)(0)
//    connections.foreach { case Array(a, b) =>
//      matrix(elements(a))(elements(b)) = 1
//      matrix(elements(b))(elements(a)) = 1
//    }
//    println("elements")
//    println(matrix.map(_.mkString).mkString("\n"))
//
//
//    val directions = List(
//      (-1, 0),  // Up
//      (1, 0),   // Down
//      (0, -1),  // Left
//      (0, 1)    // Right
//    )
//
//    def largestConnectedGroup(matrix: Array[Array[Int]]): Int = {
//      val rows = matrix.length
//      val cols = matrix(0).length
//      val visited = Array.fill(rows, cols)(false)
//
//      def isValid(x: Int, y: Int): Boolean =
//        x >= 0 && x < rows && y >= 0 && y < cols && !visited(x)(y) && matrix(x)(y) == 1
//
//      def dfs(x: Int, y: Int): Int = {
//        visited(x)(y) = true
//        var size = 1
//
//        for ((dx, dy) <- directions) {
//          val newX = x + dx
//          val newY = y + dy
//          if (isValid(newX, newY)) {
//            size += dfs(newX, newY)
//          }
//        }
//
//        size
//      }
//
//      var maxSize = 0
//
//      for (i <- matrix.indices; j <- matrix(i).indices) {
//        if (matrix(i)(j) == 1 && !visited(i)(j)) {
//          val currentSize = dfs(i, j)
//          maxSize = Math.max(maxSize, currentSize)
//        }
//      }
//
//      println("visited")
//      println(visited.map(_.map(if(_) 1 else 0).mkString).mkString("\n"))
//
//      maxSize
//    }
//
//    val res = largestConnectedGroup(matrix)
//    println(res)
//    val res = collection.mutable.Set.empty[Set[String]]
//
//    for {
//      k1 <- connections.keys.filter(_.startsWith("t"))
//      k2 <- connections(k1)
//      k3 <- connections(k2)
//      group = Set(k1, k2, k3)
//      if group.size == 3 && !res.contains(group) && connections(k3).contains(k1)
//    } {
//      res.add(group)
//    }
//
//    //    res.foreach(println)
//
//    res.size

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
      lanGames(input) shouldBe "co,de,ka,ta"
    }

    "work as expected #2" in {
      val input = Utils.readInputFile(23)
      lanGames(input) shouldBe 1215
    }
  }
}
