package algo.data.stack.mutable

import algo.testing.BaseSpec

import scala.collection.mutable
import scala.util.Random

final class MinMaxStackSpec extends BaseSpec {

  "Factory|empty" in {

    val stack = MinMaxStack.empty[Int]
    assert(stack.size === 0)

  }

  "From|from" in {

    val source = Vector(1, 2, 3)
    val stack = MinMaxStack.from(source)
    for (elem <- source) {
      assert(stack.pop() === elem)
    }

  }

  "Factory|apply" in {

    val source = Vector(1, 2, 3)
    val stack = MinMaxStack(source: _*)
    for (elem <- source) {
      assert(stack.pop() === elem)
    }

  }

  "Factory|to" in {

    val stack = mutable.Stack(1, 2, 3)
    val minmaxStack = stack.to(MinMaxStack)

    assert(minmaxStack.size === stack.size)
    while (minmaxStack.nonEmpty) {
      val expectedMinMax = (minmaxStack.iterator.min, minmaxStack.iterator.max)
      assert(minmaxStack.minmax === expectedMinMax)
      assert(minmaxStack.pop() === stack.pop())
    }

  }

  "size" in {

    for (sz <- 0 until 100) {
      val stack = MinMaxStack.from(Seq.tabulate(sz)(identity))
      assert(stack.size === sz)
    }

  }

  "push" in {

    val size = 100
    val stack = MinMaxStack.empty[Int]
    for (i <- 0 to size) {
      stack.push(i)
      assert(stack.top === i)
      assert(stack.size === i + 1)
    }

  }

  "push(value, ...)" in {

    val stack = MinMaxStack.empty[Int]
    val source = Seq.fill(10)(Random.nextInt())
    stack.push(0, source.reverse: _*)
    for (i <- source.indices) {
      assert(stack.min === stack.iterator.min)
      assert(stack.max === stack.iterator.max)
      assert(stack.pop() === source(i))
    }
    assert(stack.pop() === 0)

  }

  "pushAll" in {

    val stack = MinMaxStack.empty[Int]
    val source = Seq.fill(10)(Random.nextInt())
    stack.pushAll(source.reverse)
    for (i <- source.indices) {
      assert(stack.min === stack.iterator.min)
      assert(stack.max === stack.iterator.max)
      assert(stack.pop() === source(i))
    }

  }

  "pop" in {

    val size = 100
    val stack = MinMaxStack.from(Seq.tabulate(size)(identity))
    for (s <- 0 until size) {
      assert(stack.pop() === s)
      assert(stack.size === size - (s + 1))
    }

  }

  "pop from an empty stack" in {

    val emptyStack = MinMaxStack.empty[Int]
    intercept[NoSuchElementException] {
      emptyStack.pop()
    }

  }

  "popAll" in {

    val source = Seq.fill(100)(Random.nextInt())
    val stack = MinMaxStack.from(source)
    val elements = stack.popAll()
    assert(elements === source)
    assert(stack.isEmpty)

  }

  "popWhile" in {

    val stack = MinMaxStack(1, 2, 3, 2, 1, 4, 5)
    val elementsLessThanThree = stack.popWhile(_ < 3)
    assert(elementsLessThanThree === Seq(1, 2))

    val noElements = stack.popWhile(_ < 3)
    assert(noElements.isEmpty)

    val elementsLessThanSix = stack.popWhile(_ < 6)
    assert(elementsLessThanSix === Seq(3, 2, 1, 4, 5))

    assert(stack.isEmpty)

  }

  "top" in {

    val stack = MinMaxStack.empty[Int]
    for (i <- 0 until 100) {
      stack.push(i)
      assert(stack.top === i)
    }

  }

  "top of an empty stack" in {

    val emptyStack = MinMaxStack.empty[Int]
    intercept[NoSuchElementException] {
      emptyStack.top
    }

  }

  "topOption" in {

    val stack = MinMaxStack.empty[Int]
    assert(stack.topOption === None)
    for (i <- 0 until 100) {
      stack.push(i)
      assert(stack.topOption === Option(i))
    }

  }

  "bottom" in {

    val stack = MinMaxStack.empty[Int]
    val bottomElement = Random.nextInt()
    stack.push(bottomElement)
    for (i <- 0 until 10) {
      stack.push(i)
      assert(stack.bottom === bottomElement)
    }

  }

  "bottom of an empty stack" in {

    val emptyStack = MinMaxStack.empty[Int]
    intercept[NoSuchElementException] {
      emptyStack.bottom
    }

  }

  "bottomOption" in {

    val stack = MinMaxStack.empty[Int]
    assert(stack.bottomOption === None)
    val bottomElement = Random.nextInt()
    stack.push(bottomElement)
    for (i <- 0 until 10) {
      stack.push(i)
      assert(stack.bottomOption === Option(bottomElement))
    }

  }

  "apply" in {

    val stack = MinMaxStack(3, 2, 1, 4)
    intercept[IndexOutOfBoundsException] {
      stack(-1)
    }
    assert(stack(0) === 3)
    assert(stack(1) === 2)
    assert(stack(2) === 1)
    assert(stack(3) === 4)
    intercept[IndexOutOfBoundsException] {
      stack(4)
    }

  }

  "clear" in {

    val stack = MinMaxStack(1, 2, 3, 4)
    stack.clear()
    assert(stack.size === 0)

  }

  "ordering" in {

    implicit val ordering: Ordering[Int] = Ordering.Int.reverse
    val stack = MinMaxStack.empty[Int]
    assert(stack.ordering === ordering)

    stack.push(2)
    assert(stack.min === 2)
    assert(stack.max === 2)
    stack.push(3)
    assert(stack.min === 3)
    assert(stack.max === 2)
    stack.push(1)
    assert(stack.min === 3)
    assert(stack.max === 1)

  }

  "min" in {

    val stack = MinMaxStack.empty[Int]
    for (_ <- 0 until 100) {
      stack.push(Random.nextInt())
      assert(stack.min === stack.iterator.min)
    }
    for (_ <- 0 until 100) {
      assert(stack.min === stack.iterator.min)
      stack.pop()
    }

  }

  "min of an empty stack" in {

    val stack = MinMaxStack.empty[Int]
    intercept[UnsupportedOperationException] {
      stack.min
    }

  }

  "minOption" in {

    val stack = MinMaxStack.empty[Int]
    assert(stack.minOption === None)
    for (_ <- 0 until 100) {
      stack.push(Random.nextInt())
      assert(stack.minOption === Option(stack.iterator.min))
    }
    for (_ <- 0 until 100) {
      assert(stack.minOption === Option(stack.iterator.min))
      stack.pop()
    }
    assert(stack.minOption === None)

  }

  "max" in {

    val stack = MinMaxStack.empty[Int]
    for (_ <- 0 until 100) {
      stack.push(Random.nextInt())
      assert(stack.max === stack.iterator.max)
    }
    for (_ <- 0 until 100) {
      assert(stack.max === stack.iterator.max)
      stack.pop()
    }

  }

  "max of an empty stack" in {

    val stack = MinMaxStack.empty[Int]
    intercept[UnsupportedOperationException] {
      stack.max
    }

  }

  "maxOption" in {

    val stack = MinMaxStack.empty[Int]
    assert(stack.maxOption === None)
    for (_ <- 0 until 100) {
      stack.push(Random.nextInt())
      assert(stack.maxOption === Option(stack.iterator.max))
    }
    for (_ <- 0 until 100) {
      assert(stack.maxOption === Option(stack.iterator.max))
      stack.pop()
    }
    assert(stack.maxOption === None)

  }

  "minmax" in {

    val stack = MinMaxStack.empty[Int]
    for (_ <- 0 until 100) {
      stack.push(Random.nextInt())
      val expectedValue = (stack.iterator.min, stack.iterator.max)
      assert(stack.minmax === expectedValue)
    }
    for (_ <- 0 until 100) {
      val expectedValue = (stack.iterator.min, stack.iterator.max)
      assert(stack.minmax === expectedValue)
      stack.pop()
    }

  }

  "minmax of an empty stack" in {

    val stack = MinMaxStack.empty[Int]
    intercept[UnsupportedOperationException] {
      stack.minmax
    }

  }

  "minmaxOption" in {

    val stack = MinMaxStack.empty[Int]
    assert(stack.minmaxOption === None)
    for (_ <- 0 until 100) {
      stack.push(Random.nextInt())
      val expectedValue = Option((stack.iterator.min, stack.iterator.max))
      assert(stack.minmaxOption === expectedValue)
    }
    for (_ <- 0 until 100) {
      val expectedValue = Option((stack.iterator.min, stack.iterator.max))
      assert(stack.minmaxOption === expectedValue)
      stack.pop()
    }
    assert(stack.minmaxOption === None)

  }

  "isEmpty" in {

    val emptyStack = MinMaxStack.empty[Int]
    assert(emptyStack.isEmpty)

    val nonEmptyStack = MinMaxStack(1)
    assert(!nonEmptyStack.isEmpty)

  }

  "nonEmpty" in {

    val emptyStack = MinMaxStack.empty[Int]
    assert(!emptyStack.nonEmpty)

    val nonEmptyStack = MinMaxStack(1)
    assert(nonEmptyStack.nonEmpty)

  }

  "iterator" in {

    val stack = MinMaxStack.empty[Int]
    stack.push(1)
    stack.push(2)
    stack.push(3)
    assert(stack.iterator.toSeq === Seq(3, 2, 1))

  }

  "reverseIterator" in {

    val stack = MinMaxStack.empty[Int]
    stack.push(1)
    stack.push(2)
    stack.push(3)
    assert(stack.reverseIterator.toSeq === Seq(1, 2, 3))

  }

  "to" in {

    val minmaxStack = MinMaxStack(1, 2, 3)
    val stack = minmaxStack.to(mutable.Stack)

    assert(stack.size === minmaxStack.size)
    while (stack.nonEmpty) {
      assert(stack.pop() === minmaxStack.pop())
    }

  }

}
