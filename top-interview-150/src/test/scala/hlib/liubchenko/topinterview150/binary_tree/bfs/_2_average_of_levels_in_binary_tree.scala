package hlib.liubchenko.topinterview150.binary_tree.bfs

import hlib.liubchenko.topinterview150.binary_tree.TreeNode
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec


class _2_average_of_levels_in_binary_tree extends AnyWordSpec with Matchers {
  def averageOfLevels(root: TreeNode): Array[Double] = {
    import scala.collection.mutable

    def loop(node: TreeNode, level: Int, acc: mutable.Map[Int, (Long, Int)]): Unit = {
      if (node == null) ()
      else {
        acc.updateWith(level) {
          case Some((sum, count)) => Some(sum + node.value, count + 1)
          case None               => Some(node.value, 1)
        }
        loop(node.left, level + 1, acc)
        loop(node.right, level + 1, acc)
      }
    }

    val acc = mutable.Map.empty[Int, (Long, Int)]
    loop(root, 1, acc)
    acc.toArray.sortBy(_._1).map { case (_, (sum, count)) => sum.toDouble / count }
  }

  "averageOfLevels" should {
    "work as expected" in {
      val tree = new TreeNode(3, new TreeNode(9), new TreeNode(20, new TreeNode(15), new TreeNode(7)))
      averageOfLevels(tree).toList shouldBe List(3.0, 14.5, 11.0)
    }
  }
}
