package algo.data.stack.mutable

import algo.testing.BaseSpec

import scala.collection.mutable
import scala.util.Random

final class MinMaxStackSpec extends BaseSpec {

  "Factory|empty" in {

    val stack = MinMaxStack.empty[Int]
    stack.size shouldBe 0

  }

  "From|from" in {

    val source = Vector(1, 2, 3)
    val stack = MinMaxStack.from(source)
    for (elem <- source) {
      stack.pop() shouldBe elem
    }

  }

  "Factory|apply" in {

    val source = Vector(1, 2, 3)
    val stack = MinMaxStack(source: _*)
    for (elem <- source) {
      stack.pop() shouldBe elem
    }

  }

  "Factory|to" in {

    val stack = mutable.Stack(1, 2, 3)
    val minmaxStack = stack.to(MinMaxStack)

    minmaxStack.size shouldBe stack.size
    while (minmaxStack.nonEmpty) {
      val expectedMinMax = (minmaxStack.iterator.min, minmaxStack.iterator.max)
      minmaxStack.minmax shouldBe expectedMinMax
      minmaxStack.pop() shouldBe stack.pop()
    }

  }

  "size" in {

    for (sz <- 0 until 100) {
      val stack = MinMaxStack.from(Seq.tabulate(sz)(identity))
      stack.size shouldBe sz
    }

  }

  "push" in {

    val size = 100
    val stack = MinMaxStack.empty[Int]
    for (i <- 0 to size) {
      stack.push(i)
      stack.top shouldBe i
      stack.size shouldBe i + 1
    }

  }

  "pop" in {

    val size = 100
    val stack = MinMaxStack.from(Seq.tabulate(size)(identity))
    for (s <- 0 until size) {
      stack.pop() shouldBe s
      stack.size shouldBe size - (s + 1)
    }

  }

  "pop from an empty stack" in {

    val emptyStack = MinMaxStack.empty[Int]
    a[NoSuchElementException] shouldBe thrownBy {
      emptyStack.pop()
    }

  }

  "top" in {

    val stack = MinMaxStack.empty[Int]
    for (i <- 0 until 100) {
      stack.push(i)
      stack.top shouldBe i
    }

  }

  "top of an empty stack" in {

    val emptyStack = MinMaxStack.empty[Int]
    a[NoSuchElementException] shouldBe thrownBy {
      emptyStack.top
    }

  }

  "topOption" in {

    val stack = MinMaxStack.empty[Int]
    stack.topOption shouldBe None
    for (i <- 0 until 100) {
      stack.push(i)
      stack.topOption shouldBe Option(i)
    }

  }

  "bottom" in {

    val stack = MinMaxStack.empty[Int]
    val bottomElement = Random.nextInt()
    stack.push(bottomElement)
    for (i <- 0 until 10) {
      stack.push(i)
      stack.bottom shouldBe bottomElement
    }

  }

  "bottom of an empty stack" in {

    val emptyStack = MinMaxStack.empty[Int]
    a[NoSuchElementException] shouldBe thrownBy {
      emptyStack.bottom
    }

  }

  "bottomOption" in {

    val stack = MinMaxStack.empty[Int]
    stack.bottomOption shouldBe None
    val bottomElement = Random.nextInt()
    stack.push(bottomElement)
    for (i <- 0 until 10) {
      stack.push(i)
      stack.bottomOption shouldBe Option(bottomElement)
    }

  }

  "apply" in {

    val stack = MinMaxStack(3, 2, 1, 4)
    a[IndexOutOfBoundsException] shouldBe thrownBy {
      stack(-1)
    }
    stack(0) shouldBe 3
    stack(1) shouldBe 2
    stack(2) shouldBe 1
    stack(3) shouldBe 4
    a[IndexOutOfBoundsException] shouldBe thrownBy {
      stack(4)
    }

  }

  "clear" in {

    val stack = MinMaxStack(1, 2, 3, 4)
    stack.clear()
    stack.size shouldBe 0

  }

  "ordering" in {

    implicit val ordering: Ordering[Int] = Ordering.Int.reverse
    val stack = MinMaxStack.empty[Int]
    stack.ordering shouldBe ordering

    stack.push(2)
    stack.min shouldBe 2
    stack.max shouldBe 2
    stack.push(3)
    stack.min shouldBe 3
    stack.max shouldBe 2
    stack.push(1)
    stack.min shouldBe 3
    stack.max shouldBe 1

  }

  "min" in {

    val stack = MinMaxStack.empty[Int]
    for (_ <- 0 until 100) {
      stack.push(Random.nextInt())
      stack.min shouldBe stack.iterator.min
    }
    for (_ <- 0 until 100) {
      stack.min shouldBe stack.iterator.min
      stack.pop()
    }

  }

  "min of an empty stack" in {

    val stack = MinMaxStack.empty[Int]
    a[UnsupportedOperationException] shouldBe thrownBy {
      stack.min
    }

  }

  "minOption" in {

    val stack = MinMaxStack.empty[Int]
    stack.minOption shouldBe None
    for (_ <- 0 until 100) {
      stack.push(Random.nextInt())
      stack.minOption shouldBe Option(stack.iterator.min)
    }
    for (_ <- 0 until 100) {
      stack.minOption shouldBe Option(stack.iterator.min)
      stack.pop()
    }
    stack.minOption shouldBe None

  }

  "max" in {

    val stack = MinMaxStack.empty[Int]
    for (_ <- 0 until 100) {
      stack.push(Random.nextInt())
      stack.max shouldBe stack.iterator.max
    }
    for (_ <- 0 until 100) {
      stack.max shouldBe stack.iterator.max
      stack.pop()
    }

  }

  "max of an empty stack" in {

    val stack = MinMaxStack.empty[Int]
    a[UnsupportedOperationException] shouldBe thrownBy {
      stack.max
    }

  }

  "maxOption" in {

    val stack = MinMaxStack.empty[Int]
    stack.maxOption shouldBe None
    for (_ <- 0 until 100) {
      stack.push(Random.nextInt())
      stack.maxOption shouldBe Option(stack.iterator.max)
    }
    for (_ <- 0 until 100) {
      stack.maxOption shouldBe Option(stack.iterator.max)
      stack.pop()
    }
    stack.maxOption shouldBe None

  }

  "minmax" in {

    val stack = MinMaxStack.empty[Int]
    for (_ <- 0 until 100) {
      stack.push(Random.nextInt())
      val expectedValue = (stack.iterator.min, stack.iterator.max)
      stack.minmax shouldBe expectedValue
    }
    for (_ <- 0 until 100) {
      val expectedValue = (stack.iterator.min, stack.iterator.max)
      stack.minmax shouldBe expectedValue
      stack.pop()
    }

  }

  "minmax of an empty stack" in {

    val stack = MinMaxStack.empty[Int]
    a[UnsupportedOperationException] shouldBe thrownBy {
      stack.minmax
    }

  }

  "minmaxOption" in {

    val stack = MinMaxStack.empty[Int]
    stack.minmaxOption shouldBe None
    for (_ <- 0 until 100) {
      stack.push(Random.nextInt())
      val expectedValue = Option((stack.iterator.min, stack.iterator.max))
      stack.minmaxOption shouldBe expectedValue
    }
    for (_ <- 0 until 100) {
      val expectedValue = Option((stack.iterator.min, stack.iterator.max))
      stack.minmaxOption shouldBe expectedValue
      stack.pop()
    }
    stack.minmaxOption shouldBe None

  }

  "isEmpty" in {

    val emptyStack = MinMaxStack.empty[Int]
    emptyStack.isEmpty shouldBe true

    val nonEmptyStack = MinMaxStack(1)
    nonEmptyStack.isEmpty shouldBe false

  }

  "nonEmpty" in {

    val emptyStack = MinMaxStack.empty[Int]
    emptyStack.nonEmpty shouldBe false

    val nonEmptyStack = MinMaxStack(1)
    nonEmptyStack.nonEmpty shouldBe true

  }

  "iterator" in {

    val stack = MinMaxStack.empty[Int]
    stack.push(1)
    stack.push(2)
    stack.push(3)
    stack.iterator.toSeq shouldBe Seq(3, 2, 1)

  }

  "reverseIterator" in {

    val stack = MinMaxStack.empty[Int]
    stack.push(1)
    stack.push(2)
    stack.push(3)
    stack.reverseIterator.toSeq shouldBe Seq(1, 2, 3)

  }

  "to" in {

    val minmaxStack = MinMaxStack(1, 2, 3)
    val stack = minmaxStack.to(mutable.Stack)

    stack.size shouldBe minmaxStack.size
    while (stack.nonEmpty) {
      stack.pop() shouldBe minmaxStack.pop()
    }

  }

}
