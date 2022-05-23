package algo.data.stack.mutable

import algo.testing.BaseSpec

import scala.collection.mutable
import scala.util.Random

final class MinMaxStackSpec extends BaseSpec {

  "Factory|empty" in {

    val stack = MinMaxStack.empty[Int]
    assert(stack.size === 0)

  }

  "Factory|from" in {

    val source = Vector(1, 2, 3)
    val stack = MinMaxStack.from(source)
    for (elem <- source) {
      assert(stack.pop() === elem)
    }

  }

  "Factory|apply" in {

    val source = Vector(1, 2, 3)
    val stack = MinMaxStack(source*)
    for (elem <- source) {
      assert(stack.pop() === elem)
    }

  }

  "Factory|to" in {

    val stack = mutable.Stack(1, 2, 3)
    val minmaxStack = stack.to(MinMaxStack)

    assert(minmaxStack.size === stack.size)
    while (minmaxStack.nonEmpty) {
      val expectedMinMax = (stack.min, stack.max)
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

    val stack = MinMaxStack.empty[Int]
    for (i <- 0 to 100) {
      stack.push(i)
      assert(stack.top === i)
      assert(stack.size === i + 1)
    }

  }

  "push(value, ...)" in {

    val seed = System.nanoTime()
    withClue(s"seed=[$seed]") {
      val random = new Random(seed)

      val source = IndexedSeq.fill(100)(random.nextInt())
      val stack = MinMaxStack.empty[Int]
      stack.push(0, source.reverse*)
      for (i <- source.indices) {
        assert(stack.min === math.min(source.drop(i).min, 0))
        assert(stack.max === math.max(source.drop(i).max, 0))
        assert(stack.pop() === source(i))
      }
      assert(stack.pop() === 0)

    }

  }

  "pushAll" in {

    val seed = System.nanoTime()
    withClue(s"seed=[$seed]") {
      val random = new Random(seed)

      val source = IndexedSeq.fill(100)(random.nextInt())
      val stack = MinMaxStack.empty[Int]
      stack.pushAll(source.reverse)
      for (i <- source.indices) {
        assert(stack.min === source.drop(i).min)
        assert(stack.max === source.drop(i).max)
        assert(stack.pop() === source(i))
      }

    }

  }

  "pop" in {

    val stack = MinMaxStack.from(Seq.tabulate(100)(identity))
    for (i <- 0 until 100) {
      assert(stack.pop() === i)
      assert(stack.size === 100 - (i + 1))
    }

  }

  "pop from an empty stack" in {

    val stack = MinMaxStack.empty[Int]
    intercept[NoSuchElementException] {
      stack.pop()
    }

  }

  "popAll" in {

    val seed = System.nanoTime()
    withClue(s"seed=[$seed]") {
      val random = new Random(seed)

      val source = IndexedSeq.fill(100)(random.nextInt())
      val stack = MinMaxStack.from(source)
      val elements = stack.popAll()
      assert(elements === source)
      assert(stack.isEmpty)

    }

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

    val stack = MinMaxStack.empty[Int]
    intercept[NoSuchElementException] {
      stack.top
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

    val seed = System.nanoTime()
    withClue(s"seed=[$seed]") {
      val random = new Random(seed)

      val expectedBottom = random.nextInt()
      val stack = MinMaxStack.empty[Int]
      stack.push(expectedBottom)
      for (i <- 0 until 10) {
        stack.push(i)
        assert(stack.bottom === expectedBottom)
      }

    }

  }

  "bottom of an empty stack" in {

    val stack = MinMaxStack.empty[Int]
    intercept[NoSuchElementException] {
      stack.bottom
    }

  }

  "bottomOption" in {

    val seed = System.nanoTime()
    withClue(s"seed=[$seed]") {
      val random = new Random(seed)

      val expectedBottom = random.nextInt()
      val stack = MinMaxStack.empty[Int]
      assert(stack.bottomOption === None)
      stack.push(expectedBottom)
      for (i <- 0 until 10) {
        stack.push(i)
        assert(stack.bottomOption === Option(expectedBottom))
      }

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

  }

  "min" in {

    val seed = System.nanoTime()
    withClue(s"seed=[$seed]") {
      val random = new Random(seed)

      val source = IndexedSeq.fill(100)(random.nextInt())
      val stack = MinMaxStack.empty[Int]
      for (i <- source.indices) {
        stack.push(source(i))
        assert(stack.min === source.take(i + 1).min)
      }
      for (i <- source.indices) {
        assert(stack.min === source.dropRight(i).min)
        stack.pop()
      }

    }

  }

  "min of an empty stack" in {

    val stack = MinMaxStack.empty[Int]
    intercept[NoSuchElementException] {
      stack.min
    }

  }

  "minOption" in {

    val seed = System.nanoTime()
    withClue(s"seed=[$seed]") {
      val random = new Random(seed)

      val source = IndexedSeq.fill(100)(random.nextInt())
      val stack = MinMaxStack.empty[Int]
      assert(stack.minOption === None)
      for (i <- source.indices) {
        stack.push(source(i))
        assert(stack.minOption === Option(source.take(i + 1).min))
      }
      for (i <- source.indices) {
        assert(stack.minOption === Option(source.dropRight(i).min))
        stack.pop()
      }
      assert(stack.minOption === None)

    }

  }

  "max" in {

    val seed = System.nanoTime()
    withClue(s"seed=[$seed]") {
      val random = new Random(seed)

      val source = IndexedSeq.fill(100)(random.nextInt())
      val stack = MinMaxStack.empty[Int]
      for (i <- source.indices) {
        stack.push(source(i))
        assert(stack.max === source.take(i + 1).max)
      }
      for (i <- source.indices) {
        assert(stack.max === source.dropRight(i).max)
        stack.pop()
      }

    }

  }

  "max of an empty stack" in {

    val stack = MinMaxStack.empty[Int]
    intercept[NoSuchElementException] {
      stack.max
    }

  }

  "maxOption" in {

    val seed = System.nanoTime()
    withClue(s"seed=[$seed]") {
      val random = new Random(seed)

      val source = IndexedSeq.fill(100)(random.nextInt())
      val stack = MinMaxStack.empty[Int]
      assert(stack.maxOption === None)
      for (i <- source.indices) {
        stack.push(source(i))
        assert(stack.maxOption === Option(source.take(i + 1).max))
      }
      for (i <- source.indices) {
        assert(stack.maxOption === Option(source.dropRight(i).max))
        stack.pop()
      }
      assert(stack.maxOption === None)

    }

  }

  "minmax" in {

    val seed = System.nanoTime()
    withClue(s"seed=[$seed]") {
      val random = new Random(seed)

      val source = IndexedSeq.fill(100)(random.nextInt())
      val stack = MinMaxStack.empty[Int]
      for (i <- source.indices) {
        stack.push(source(i))
        val expectedValue = (source.take(i + 1).min, source.take(i + 1).max)
        assert(stack.minmax === expectedValue)
      }
      for (i <- source.indices) {
        val expectedValue = (source.dropRight(i).min, source.dropRight(i).max)
        assert(stack.minmax === expectedValue)
        stack.pop()
      }

    }

  }

  "minmax of an empty stack" in {

    val stack = MinMaxStack.empty[Int]
    intercept[NoSuchElementException] {
      stack.minmax
    }

  }

  "minmaxOption" in {

    val seed = System.nanoTime()
    withClue(s"seed=[$seed]") {
      val random = new Random(seed)

      val source = IndexedSeq.fill(100)(random.nextInt())
      val stack = MinMaxStack.empty[Int]
      assert(stack.minmaxOption === None)
      for (i <- source.indices) {
        stack.push(source(i))
        val expectedValue =
          Option((source.take(i + 1).min, source.take(i + 1).max))
        assert(stack.minmaxOption === expectedValue)
      }
      for (i <- source.indices) {
        val expectedValue =
          Option((source.dropRight(i).min, source.dropRight(i).max))
        assert(stack.minmaxOption === expectedValue)
        stack.pop()
      }
      assert(stack.minmaxOption === None)

    }

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

  "knownSize" in {

    for (size <- 0 until 100) {
      val stack = MinMaxStack.from(Seq.tabulate(size)(identity))
      assert(stack.knownSize === size)
    }

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

  "clone" in {

    val minmaxStack = MinMaxStack(1, 2, 3)
    val clonedMinMaxStack = minmaxStack.clone

    assert(clonedMinMaxStack.size === minmaxStack.size)
    while (clonedMinMaxStack.nonEmpty) {
      assert(clonedMinMaxStack.minmax === minmaxStack.minmax)
      assert(clonedMinMaxStack.pop() === minmaxStack.pop())
    }

  }

}
