package algo.data.queue.mutable

import algo.testing.BaseSpec

import scala.collection.mutable
import scala.util.Random

final class MinMaxQueueSpec extends BaseSpec {

  "Factory|empty" in {

    val queue = MinMaxQueue.empty[Int]
    assert(queue.size === 0)

  }

  "From|from" in {

    val source = Vector(1, 2, 3)
    val queue = MinMaxQueue.from(source)
    for (elem <- source) {
      assert(queue.dequeue() === elem)
    }

  }

  "Factory|apply" in {

    val source = Vector(1, 2, 3)
    val queue = MinMaxQueue(source*)
    for (elem <- source) {
      assert(queue.dequeue() === elem)
    }

  }

  "Factory|to" in {

    val queue = mutable.Queue(1, 2, 3)
    val minmaxQueue = queue.to(MinMaxQueue)

    assert(minmaxQueue.size === queue.size)
    while (minmaxQueue.nonEmpty) {
      val expectedMinMax = (minmaxQueue.iterator.min, minmaxQueue.iterator.max)
      assert(minmaxQueue.minmax === expectedMinMax)
      assert(minmaxQueue.dequeue() === queue.dequeue())
    }

  }

  "size" in {

    for (sz <- 0 until 100) {
      val queue = MinMaxQueue.from(Seq.tabulate(sz)(identity))
      assert(queue.size === sz)
    }

  }

  "enqueue" in {

    val size = 100
    val queue = MinMaxQueue.empty[Int]
    for (i <- 0 to size) {
      queue.enqueue(i)
      assert(queue.size === i + 1)
    }

  }

  "enqueue(value, ...)" in {

    val queue = MinMaxQueue.empty[Int]
    val source = Seq.fill(100)(Random.nextInt())
    queue.enqueue(0, source*)
    for (elem <- 0 +: source) {
      assert(queue.min === queue.iterator.min)
      assert(queue.max === queue.iterator.max)
      assert(queue.dequeue() === elem)
    }

  }

  "enqueueAll" in {

    val queue = MinMaxQueue.empty[Int]
    val source = Seq.fill(100)(Random.nextInt())
    queue.enqueueAll(source)
    for (elem <- source) {
      assert(queue.min === queue.iterator.min)
      assert(queue.max === queue.iterator.max)
      assert(queue.dequeue() === elem)
    }

  }

  "dequeue" in {

    val size = 100
    val queue = MinMaxQueue.from(Seq.tabulate(size)(identity))
    for (s <- 0 until size) {
      assert(queue.dequeue() === s)
      assert(queue.size === size - (s + 1))
    }

  }

  "dequeue from an empty queue" in {

    val emptyQueue = MinMaxQueue.empty[Int]
    intercept[NoSuchElementException] {
      emptyQueue.dequeue()
    }

  }

  "dequeueAll" in {

    val source = Seq.fill(10)(Random.nextInt())
    val queue = MinMaxQueue.from(source)
    val elements = queue.dequeueAll()
    assert(elements === source)
    assert(queue.isEmpty)

  }

  "dequeueWhile" in {

    val queue = MinMaxQueue(2, 1, 3, 2, 1, 4, 5)

    val elementsLessThanThree = queue.dequeueWhile(_ < 3)
    assert(elementsLessThanThree === Seq(2, 1))

    val noElements = queue.dequeueWhile(_ < 3)
    assert(noElements.isEmpty)

    val elementsLessThanSix = queue.dequeueWhile(_ < 6)
    assert(elementsLessThanSix === Seq(3, 2, 1, 4, 5))

    assert(queue.isEmpty)

  }

  "front" in {

    val queue = MinMaxQueue.empty[Int]
    val frontElement = Random.nextInt()
    queue.enqueue(frontElement)
    for (i <- 0 until 100) {
      queue.enqueue(i)
      assert(queue.front === frontElement)
    }

  }

  "front of an empty queue" in {

    val emptyStack = MinMaxQueue.empty[Int]
    intercept[NoSuchElementException] {
      emptyStack.front
    }

  }

  "frontOption" in {

    val queue = MinMaxQueue.empty[Int]
    assert(queue.frontOption === None)
    val frontElement = Random.nextInt()
    queue.enqueue(frontElement)
    for (i <- 0 until 100) {
      queue.enqueue(i)
      assert(queue.frontOption === Option(frontElement))
    }

  }

  "back" in {

    val queue = MinMaxQueue.empty[Int]
    for (i <- 0 until 10) {
      queue.enqueue(i)
      assert(queue.back === i)
    }

  }

  "back of an empty queue" in {

    val emptyQueue = MinMaxQueue.empty[Int]
    intercept[NoSuchElementException] {
      emptyQueue.back
    }

  }

  "backOption" in {

    val queue = MinMaxQueue.empty[Int]
    for (i <- 0 until 10) {
      queue.enqueue(i)
      assert(queue.backOption === Option(i))
    }

  }

  "apply" in {

    val queue = MinMaxQueue.empty[Int]
    // To ensure internal structure varies, do some enqueue & dequeue
    queue.enqueue(1)
    queue.enqueue(2)
    queue.enqueue(3)
    queue.dequeue()
    queue.enqueue(4)
    queue.enqueue(5)

    // queue: [2, 3, 4, 5]
    intercept[IndexOutOfBoundsException] {
      queue(-1)
    }
    assert(queue(0) === 2)
    assert(queue(1) === 3)
    assert(queue(2) === 4)
    assert(queue(3) === 5)
    intercept[IndexOutOfBoundsException] {
      queue(4)
    }

  }

  "clear" in {

    val queue = MinMaxQueue(1, 2, 3, 4)
    queue.clear()
    assert(queue.size === 0)

  }

  "ordering" in {

    implicit val ordering: Ordering[Int] = Ordering.Int.reverse
    val queue = MinMaxQueue.empty[Int]
    assert(queue.ordering === ordering)

    // [2]
    queue.enqueue(2)
    assert(queue.min === 2)
    assert(queue.max === 2)

    // [2,3]
    queue.enqueue(3)
    assert(queue.min === 3)
    assert(queue.max === 2)

    // [2,3,1]
    queue.enqueue(1)
    assert(queue.min === 3)
    assert(queue.max === 1)

    // [3,1]
    queue.dequeue()
    assert(queue.min === 3)
    assert(queue.max === 1)

    // [3,1,4]
    queue.enqueue(4)
    assert(queue.min === 4)
    assert(queue.max === 1)

  }

  "min" in {

    val queue = MinMaxQueue.empty[Int]
    for (_ <- 0 until 100) {
      queue.enqueue(Random.nextInt())
      assert(queue.min === queue.iterator.min)
    }
    for (_ <- 0 until 50) {
      assert(queue.min === queue.iterator.min)
      queue.dequeue()
    }
    for (_ <- 0 until 50) {
      queue.enqueue(Random.nextInt())
      assert(queue.min === queue.iterator.min)
    }
    for (_ <- 0 until 100) {
      assert(queue.min === queue.iterator.min)
      queue.dequeue()
    }

  }

  "min of an empty queue" in {

    val queue = MinMaxQueue.empty[Int]
    intercept[UnsupportedOperationException] {
      queue.min
    }

  }

  "minOption" in {

    val queue = MinMaxQueue.empty[Int]
    assert(queue.minOption === None)
    for (_ <- 0 until 100) {
      queue.enqueue(Random.nextInt())
      assert(queue.minOption === Option(queue.iterator.min))
    }
    for (_ <- 0 until 50) {
      assert(queue.minOption === Option(queue.iterator.min))
      queue.dequeue()
    }
    for (_ <- 0 until 50) {
      queue.enqueue(Random.nextInt())
      assert(queue.minOption === Option(queue.iterator.min))
    }
    for (_ <- 0 until 100) {
      assert(queue.minOption === Option(queue.iterator.min))
      queue.dequeue()
    }
    assert(queue.minOption === None)

  }

  "max" in {

    val queue = MinMaxQueue.empty[Int]
    for (_ <- 0 until 100) {
      queue.enqueue(Random.nextInt())
      assert(queue.max === queue.iterator.max)
    }
    for (_ <- 0 until 50) {
      assert(queue.max === queue.iterator.max)
      queue.dequeue()
    }
    for (_ <- 0 until 50) {
      queue.enqueue(Random.nextInt())
      assert(queue.max === queue.iterator.max)
    }
    for (_ <- 0 until 100) {
      assert(queue.max === queue.iterator.max)
      queue.dequeue()
    }

  }

  "maxOption" in {

    val queue = MinMaxQueue.empty[Int]
    assert(queue.maxOption === None)
    for (_ <- 0 until 100) {
      queue.enqueue(Random.nextInt())
      assert(queue.maxOption === Option(queue.iterator.max))
    }
    for (_ <- 0 until 50) {
      assert(queue.maxOption === Option(queue.iterator.max))
      queue.dequeue()
    }
    for (_ <- 0 until 50) {
      queue.enqueue(Random.nextInt())
      assert(queue.maxOption === Option(queue.iterator.max))
    }
    for (_ <- 0 until 100) {
      assert(queue.maxOption === Option(queue.iterator.max))
      queue.dequeue()
    }
    assert(queue.maxOption === None)

  }

  "max of an empty queue" in {

    val queue = MinMaxQueue.empty[Int]
    intercept[UnsupportedOperationException] {
      queue.max
    }

  }

  "minmax" in {

    val queue = MinMaxQueue.empty[Int]
    for (_ <- 0 until 100) {
      queue.enqueue(Random.nextInt())
      val expectedValue = (queue.iterator.min, queue.iterator.max)
      assert(queue.minmax === expectedValue)
    }
    for (_ <- 0 until 50) {
      val expectedValue = (queue.iterator.min, queue.iterator.max)
      assert(queue.minmax === expectedValue)
      queue.dequeue()
    }
    for (_ <- 0 until 50) {
      queue.enqueue(Random.nextInt())
      val expectedValue = (queue.iterator.min, queue.iterator.max)
      assert(queue.minmax === expectedValue)
    }
    for (_ <- 0 until 100) {
      val expectedValue = (queue.iterator.min, queue.iterator.max)
      assert(queue.minmax === expectedValue)
      queue.dequeue()
    }

  }

  "minmax of an empty queue" in {

    val queue = MinMaxQueue.empty[Int]
    intercept[UnsupportedOperationException] {
      queue.minmax
    }

  }

  "minmaxOption" in {

    val queue = MinMaxQueue.empty[Int]
    assert(queue.minmaxOption === None)
    for (_ <- 0 until 100) {
      queue.enqueue(Random.nextInt())
      val expectedValue = Option((queue.iterator.min, queue.iterator.max))
      assert(queue.minmaxOption === expectedValue)
    }
    for (_ <- 0 until 50) {
      val expectedValue = Option((queue.iterator.min, queue.iterator.max))
      assert(queue.minmaxOption === expectedValue)
      queue.dequeue()
    }
    for (_ <- 0 until 50) {
      queue.enqueue(Random.nextInt())
      val expectedValue = Option((queue.iterator.min, queue.iterator.max))
      assert(queue.minmaxOption === expectedValue)
    }
    for (_ <- 0 until 100) {
      val expectedValue = Option((queue.iterator.min, queue.iterator.max))
      assert(queue.minmaxOption === expectedValue)
      queue.dequeue()
    }
    assert(queue.minmaxOption === None)

  }

  "isEmpty" in {

    val emptyQueue = MinMaxQueue.empty[Int]
    assert(emptyQueue.isEmpty)

    val nonEmptyQueue = MinMaxQueue(1)
    assert(!nonEmptyQueue.isEmpty)

  }

  "nonEmpty" in {

    val emptyQueue = MinMaxQueue.empty[Int]
    assert(!emptyQueue.nonEmpty)

    val nonEmptyQueue = MinMaxQueue(1)
    assert(nonEmptyQueue.nonEmpty)

  }

  "iterator" in {

    val queue = MinMaxQueue.empty[Int]
    queue.enqueue(1)
    queue.enqueue(2)
    queue.enqueue(3)
    assert(queue.iterator.toSeq === Seq(1, 2, 3))

  }

  "reverseIterator" in {

    val queue = MinMaxQueue.empty[Int]
    queue.enqueue(1)
    queue.enqueue(2)
    queue.enqueue(3)
    assert(queue.reverseIterator.toSeq === Seq(3, 2, 1))

  }

  "to" in {

    val minmaxQueue = MinMaxQueue(1, 2, 3)
    val queue = minmaxQueue.to(mutable.Queue)

    assert(queue.size === minmaxQueue.size)
    while (queue.nonEmpty) {
      assert(queue.dequeue() === minmaxQueue.dequeue())
    }

  }

}
