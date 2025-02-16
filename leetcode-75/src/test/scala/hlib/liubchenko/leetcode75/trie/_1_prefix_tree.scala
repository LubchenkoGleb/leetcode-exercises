package hlib.liubchenko.leetcode75.trie

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _1_prefix_tree extends AnyWordSpec with Matchers {

  import scala.collection.mutable

  class Tree(var children: mutable.HashMap[Char, Tree] = mutable.HashMap.empty, var isWord: Boolean = false)

  // Mine, Runtime 32, Memory 96
  class Trie() {
    import scala.annotation.tailrec
    import collection.mutable

    var tree: Tree = new Tree()

    def insert(word: String): Unit = {
      @tailrec
      def loop(tree: Tree, i: Int): Unit = if (word.length == i) tree.isWord = true
      else {
        val next = tree.children.getOrElseUpdate(word(i), new Tree(mutable.HashMap.empty))
        loop(next, i + 1)
      }

      loop(tree, 0)
    }

    def search(word: String): Boolean = searchLoop(tree, 0, word, isWord = true)

    def startsWith(prefix: String): Boolean = searchLoop(tree, 0, prefix, isWord = false)

    @tailrec
    private def searchLoop(tree: Tree, i: Int, word: String, isWord: Boolean): Boolean =
      if (i == word.length) tree.isWord || !isWord
      else tree.children.contains(word(i)) && searchLoop(tree.children(word(i)), i + 1, word, isWord)
  }

  "Trie" should {
    def treeToString(tree: Tree): String = {
      def loop(level: Int, tree: Tree): List[String] = {
        tree.children.flatMap { case (c, next) =>
          ("\t" * level + c) +: loop(level + 1, next)
        }
      }.toList

      loop(0, tree).mkString("\n")
    }

    "work as expected" in {
      val t = new Trie()
      t.insert("apple")
      t.insert("airport")
      t.insert("air")
      t.insert("banana")
      t.insert("book")
      println(treeToString(t.tree))
      t.search("apple") shouldBe true
      t.search("airport") shouldBe true
      t.search("banana") shouldBe true
      t.search("book") shouldBe true
      t.search("air") shouldBe true
      t.startsWith("ai") shouldBe true
    }
  }
}
