package hlib.liubchenko.leetcode75.bfs

import hlib.liubchenko.leetcode75.common.TreeNode
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _1_binary_tree_right_side_view extends AnyWordSpec with Matchers {
  sealed trait Solution {
    def rightSideView(root: TreeNode): List[Int]
  }

  // Mine, Runtime 80, Memory 91
  object Solution1 extends Solution {
    def rightSideView(root: TreeNode): List[Int] =
      if (root == null) Nil
      else {
        val l = rightSideView(root.left)
        val r = rightSideView(root.right)
        root.value :: r ::: l.drop(r.length)
      }
  }

  "All solutions" should {
    "work as expected" in {
      List(Solution1).foreach { solution =>
        //@formatter:off
        val tree = new TreeNode(1,
          new TreeNode(2,
            null,
            new TreeNode(5,
              new TreeNode(4))
          ),
          new TreeNode(3,
            new TreeNode(6)
          )
        )
        solution.rightSideView(tree) shouldBe List(1,3,6,4)
        //@formatter:on

      }
    }
  }
}
