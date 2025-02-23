package hlib.liubchenko.topinterview150.binary_tree.general

import hlib.liubchenko.topinterview150.binary_tree.TreeNode
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _12_binary_search_tree_iterator extends AnyWordSpec with Matchers {
  import scala.collection.mutable

  class BSTIterator(_root: TreeNode) {
    private val stack = mutable.Stack.empty[TreeNode]

    def diveLeft(node: TreeNode): Unit =
      if (node == null) ()
      else { stack.push(node); diveLeft(node.left) }

    private var nextValue = { diveLeft(_root); stack.pop() }

    def next(): Int = {
      val toReturn = nextValue

      if (nextValue.right != null) diveLeft(nextValue.right)

      if (stack.nonEmpty) nextValue = stack.pop()
      else nextValue = null

      toReturn.value
    }

    def hasNext(): Boolean = nextValue != null
  }

  "BSTIterator" should {
    "work as expected, testcase 1" in {
      val tree = new TreeNode(7, new TreeNode(3), new TreeNode(15, new TreeNode(9), new TreeNode(20)))
      val iterator = new BSTIterator(tree)
      iterator.next() shouldBe 3
      iterator.next() shouldBe 7
      iterator.hasNext() shouldBe true
      iterator.next() shouldBe 9
      iterator.hasNext() shouldBe true
      iterator.next() shouldBe 15
      iterator.hasNext() shouldBe true
      iterator.next() shouldBe 20
      iterator.hasNext() shouldBe false
    }

    "work as expected, testcase 2" in {
      val tree = new TreeNode(1, null, new TreeNode(2))
      val iterator = new BSTIterator(tree)
      iterator.hasNext() shouldBe true
      iterator.next() shouldBe 1
      iterator.hasNext() shouldBe true
      iterator.next() shouldBe 2
      iterator.hasNext() shouldBe false
    }
  }
}
