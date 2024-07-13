package hlib.liubchenko.topinterview150.stack

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _3_min_stack extends AnyWordSpec with Matchers {
  class MinStack() {
    class Node(_value: Int, var next: Node = null) {
      def value: Int = _value
    }

    private var stack: Node = null
    private var min: Node = null

    def push(`val`: Int): Unit = {
      stack = new Node(`val`, stack)
      if (min == null || min.value >= `val`) min = new Node(`val`, min)
    }

    def pop(): Unit = {
      if (stack.value == min.value) min = min.next
      stack = stack.next
    }

    def top(): Int = {
      stack.value
    }

    def getMin(): Int = {
      min.value
    }
  }

  "MinStack" should {
    "work as expected #1" in {
      val stack = new MinStack()
      stack.push(4)
      stack.push(2)
      stack.push(5)
      stack.push(1)
      stack.top() shouldBe 1

      stack.push(3)
      stack.top() shouldBe 3
      stack.getMin() shouldBe 1

      stack.pop()
      stack.top() shouldBe 1

      stack.pop()
      stack.top() shouldBe 5
      stack.getMin() shouldBe 2

      stack.pop()
      stack.pop()
      stack.top() shouldBe 4
      stack.getMin() shouldBe 4

      // 4 2 5 1 3 Min 1
      // 4 2 5 1   Min 1
      // 4 2 5     Min 2
      // 4 2       Min 2
      // 4         Min 4
    }

    "work as expected #2" in {
      val stack = new MinStack()

      stack.push(0)
      stack.push(1)
      stack.push(0)

      stack.getMin shouldBe 0
      stack.pop()
      stack.getMin shouldBe 0

    }
  }
}
