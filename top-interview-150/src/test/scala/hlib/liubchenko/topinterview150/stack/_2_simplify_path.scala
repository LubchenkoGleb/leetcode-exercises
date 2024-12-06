package hlib.liubchenko.topinterview150.stack

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec


class _2_simplify_path extends AnyWordSpec with Matchers {
  def simplifyPath(path: String): String = {
    import scala.collection.mutable

    val stack = mutable.Stack.empty[String]
    path.split("/").foreach {
      case "." | ""  => ()
      case ".." => if (stack.nonEmpty) stack.pop()
      case rest => stack.push(rest)
    }
    stack.reverse.mkString("/", "/", "")
  }

  "simplifyPath" should {
    "work as expected" in {
      simplifyPath("/home/") shouldBe "/home"
      simplifyPath("/home//foo/") shouldBe "/home/foo"
      simplifyPath("/home/user/Documents/../Pictures") shouldBe "/home/user/Pictures"
      simplifyPath("/../") shouldBe "/"
      simplifyPath("/.../a/../b/c/../d/./") shouldBe "/.../b/d"
    }
  }
}
