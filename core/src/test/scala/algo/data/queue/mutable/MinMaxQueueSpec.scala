package algo.data.queue.mutable

import algo.testing.BaseSpec

import scala.collection.mutable
import scala.util.Random

final class MinMaxQueueSpec extends BaseSpec {

  "Factory|empty" in {

    val queue = MinMaxQueue.empty[Int]
    queue.size shouldBe 0

  }

  "From|from" in {

    val source = Vector(1, 2, 3)
    val queue = MinMaxQueue.from(source)
    for (elem <- source) {
      queue.dequeue() shouldBe elem
    }

  }

  "Factory|apply" in {

    val source = Vector(1, 2, 3)
    val queue = MinMaxQueue(source: _*)
    for (elem <- source) {
      queue.dequeue() shouldBe elem
    }

  }

  "Factory|to" in {

    val queue = mutable.Queue(1, 2, 3)
    val minmaxQueue = queue.to(MinMaxQueue)

    minmaxQueue.size shouldBe queue.size
    while (minmaxQueue.nonEmpty) {
      val expectedMinMax = (minmaxQueue.iterator.min, minmaxQueue.iterator.max)
      minmaxQueue.minmax shouldBe expectedMinMax
      minmaxQueue.dequeue() shouldBe queue.dequeue()
    }

  }

  "size" in {

    for (sz <- 0 until 100) {
      val queue = MinMaxQueue.from(Seq.tabulate(sz)(identity))
      queue.size shouldBe sz
    }

  }

  "enqueue" in {

    val size = 100
    val queue = MinMaxQueue.empty[Int]
    for (i <- 0 to size) {
      queue.enqueue(i)
      queue.size shouldBe i + 1
    }

  }

  "enqueue(value, ...)" in {

    val queue = MinMaxQueue.empty[Int]
    val source = Seq.fill(100)(Random.nextInt())
    queue.enqueue(0, source: _*)
    for (elem <- 0 +: source) {
      queue.min shouldBe queue.iterator.min
      queue.max shouldBe queue.iterator.max
      queue.dequeue() shouldBe elem
    }

  }

  "enqueueAll" in {

    val queue = MinMaxQueue.empty[Int]
    val source = Seq.fill(100)(Random.nextInt())
    queue.enqueueAll(source)
    for (elem <- source) {
      queue.min shouldBe queue.iterator.min
      queue.max shouldBe queue.iterator.max
      queue.dequeue() shouldBe elem
    }

  }

  "dequeue" in {

    val size = 100
    val queue = MinMaxQueue.from(Seq.tabulate(size)(identity))
    for (s <- 0 until size) {
      queue.dequeue() shouldBe s
      queue.size shouldBe size - (s + 1)
    }

  }

  "dequeue from an empty queue" in {

    val emptyQueue = MinMaxQueue.empty[Int]
    a[NoSuchElementException] shouldBe thrownBy {
      emptyQueue.dequeue()
    }

  }

  "dequeueAll" in {

    val source = Seq.fill(10)(Random.nextInt())
    val queue = MinMaxQueue.from(source)
    val elements = queue.dequeueAll()
    elements shouldBe source
    queue.isEmpty shouldBe true

  }

  "dequeueWhile" in {

    val queue = MinMaxQueue(2, 1, 3, 2, 1, 4, 5)

    val elementsLessThanThree = queue.dequeueWhile(_ < 3)
    elementsLessThanThree shouldBe Seq(2, 1)

    val noElements = queue.dequeueWhile(_ < 3)
    noElements.isEmpty shouldBe true

    val elementsLessThanSix = queue.dequeueWhile(_ < 6)
    elementsLessThanSix shouldBe Seq(3, 2, 1, 4, 5)

    queue.isEmpty shouldBe true

  }

  "front" in {

    val queue = MinMaxQueue.empty[Int]
    val frontElement = Random.nextInt()
    queue.enqueue(frontElement)
    for (i <- 0 until 100) {
      queue.enqueue(i)
      queue.front shouldBe frontElement
    }

  }

  "front of an empty queue" in {

    val emptyStack = MinMaxQueue.empty[Int]
    a[NoSuchElementException] shouldBe thrownBy {
      emptyStack.front
    }

  }

  "frontOption" in {

    val queue = MinMaxQueue.empty[Int]
    queue.frontOption shouldBe None
    val frontElement = Random.nextInt()
    queue.enqueue(frontElement)
    for (i <- 0 until 100) {
      queue.enqueue(i)
      queue.frontOption shouldBe Option(frontElement)
    }

  }

  "back" in {

    val queue = MinMaxQueue.empty[Int]
    for (i <- 0 until 10) {
      queue.enqueue(i)
      queue.back shouldBe i
    }

  }

  "back of an empty queue" in {

    val emptyQueue = MinMaxQueue.empty[Int]
    a[NoSuchElementException] shouldBe thrownBy {
      emptyQueue.back
    }

  }

  "backOption" in {

    val queue = MinMaxQueue.empty[Int]
    for (i <- 0 until 10) {
      queue.enqueue(i)
      queue.backOption shouldBe Option(i)
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
    a[IndexOutOfBoundsException] shouldBe thrownBy {
      queue(-1)
    }
    queue(0) shouldBe 2
    queue(1) shouldBe 3
    queue(2) shouldBe 4
    queue(3) shouldBe 5
    a[IndexOutOfBoundsException] shouldBe thrownBy {
      queue(4)
    }

  }

  "clear" in {

    val queue = MinMaxQueue(1, 2, 3, 4)
    queue.clear()
    queue.size shouldBe 0

  }

  "ordering" in {

    implicit val ordering: Ordering[Int] = Ordering.Int.reverse
    val queue = MinMaxQueue.empty[Int]
    queue.ordering shouldBe ordering

    // [2]
    queue.enqueue(2)
    queue.min shouldBe 2
    queue.max shouldBe 2

    // [2,3]
    queue.enqueue(3)
    queue.min shouldBe 3
    queue.max shouldBe 2

    // [2,3,1]
    queue.enqueue(1)
    queue.min shouldBe 3
    queue.max shouldBe 1

    // [3,1]
    queue.dequeue()
    queue.min shouldBe 3
    queue.max shouldBe 1

    // [3,1,4]
    queue.enqueue(4)
    queue.min shouldBe 4
    queue.max shouldBe 1

  }

  "min" in {

    val queue = MinMaxQueue.empty[Int]
    for (_ <- 0 until 100) {
      queue.enqueue(Random.nextInt())
      queue.min shouldBe queue.iterator.min
    }
    for (_ <- 0 until 50) {
      queue.min shouldBe queue.iterator.min
      queue.dequeue()
    }
    for (_ <- 0 until 50) {
      queue.enqueue(Random.nextInt())
      queue.min shouldBe queue.iterator.min
    }
    for (_ <- 0 until 100) {
      queue.min shouldBe queue.iterator.min
      queue.dequeue()
    }

  }

  "min of an empty queue" in {

    val queue = MinMaxQueue.empty[Int]
    a[UnsupportedOperationException] shouldBe thrownBy {
      queue.min
    }

  }

  "minOption" in {

    val queue = MinMaxQueue.empty[Int]
    queue.minOption shouldBe None
    for (_ <- 0 until 100) {
      queue.enqueue(Random.nextInt())
      queue.minOption shouldBe Option(queue.iterator.min)
    }
    for (_ <- 0 until 50) {
      queue.minOption shouldBe Option(queue.iterator.min)
      queue.dequeue()
    }
    for (_ <- 0 until 50) {
      queue.enqueue(Random.nextInt())
      queue.minOption shouldBe Option(queue.iterator.min)
    }
    for (_ <- 0 until 100) {
      queue.minOption shouldBe Option(queue.iterator.min)
      queue.dequeue()
    }
    queue.minOption shouldBe None

  }

  "max" in {

    val queue = MinMaxQueue.empty[Int]
    for (_ <- 0 until 100) {
      queue.enqueue(Random.nextInt())
      queue.max shouldBe queue.iterator.max
    }
    for (_ <- 0 until 50) {
      queue.max shouldBe queue.iterator.max
      queue.dequeue()
    }
    for (_ <- 0 until 50) {
      queue.enqueue(Random.nextInt())
      queue.max shouldBe queue.iterator.max
    }
    for (_ <- 0 until 100) {
      queue.max shouldBe queue.iterator.max
      queue.dequeue()
    }

  }

  "maxOption" in {

    val queue = MinMaxQueue.empty[Int]
    queue.maxOption shouldBe None
    for (_ <- 0 until 100) {
      queue.enqueue(Random.nextInt())
      queue.maxOption shouldBe Option(queue.iterator.max)
    }
    for (_ <- 0 until 50) {
      queue.maxOption shouldBe Option(queue.iterator.max)
      queue.dequeue()
    }
    for (_ <- 0 until 50) {
      queue.enqueue(Random.nextInt())
      queue.maxOption shouldBe Option(queue.iterator.max)
    }
    for (_ <- 0 until 100) {
      queue.maxOption shouldBe Option(queue.iterator.max)
      queue.dequeue()
    }
    queue.maxOption shouldBe None

  }

  "max of an empty queue" in {

    val queue = MinMaxQueue.empty[Int]
    a[UnsupportedOperationException] shouldBe thrownBy {
      queue.max
    }

  }

  "minmax" in {

    val queue = MinMaxQueue.empty[Int]
    for (_ <- 0 until 100) {
      queue.enqueue(Random.nextInt())
      val expectedValue = (queue.iterator.min, queue.iterator.max)
      queue.minmax shouldBe expectedValue
    }
    for (_ <- 0 until 50) {
      val expectedValue = (queue.iterator.min, queue.iterator.max)
      queue.minmax shouldBe expectedValue
      queue.dequeue()
    }
    for (_ <- 0 until 50) {
      queue.enqueue(Random.nextInt())
      val expectedValue = (queue.iterator.min, queue.iterator.max)
      queue.minmax shouldBe expectedValue
    }
    for (_ <- 0 until 100) {
      val expectedValue = (queue.iterator.min, queue.iterator.max)
      queue.minmax shouldBe expectedValue
      queue.dequeue()
    }

  }

  "minmax of an empty queue" in {

    val queue = MinMaxQueue.empty[Int]
    a[UnsupportedOperationException] shouldBe thrownBy {
      queue.minmax
    }

  }

  "minmaxOption" in {

    val queue = MinMaxQueue.empty[Int]
    queue.minmaxOption shouldBe None
    for (_ <- 0 until 100) {
      queue.enqueue(Random.nextInt())
      val expectedValue = Option((queue.iterator.min, queue.iterator.max))
      queue.minmaxOption shouldBe expectedValue
    }
    for (_ <- 0 until 50) {
      val expectedValue = Option((queue.iterator.min, queue.iterator.max))
      queue.minmaxOption shouldBe expectedValue
      queue.dequeue()
    }
    for (_ <- 0 until 50) {
      queue.enqueue(Random.nextInt())
      val expectedValue = Option((queue.iterator.min, queue.iterator.max))
      queue.minmaxOption shouldBe expectedValue
    }
    for (_ <- 0 until 100) {
      val expectedValue = Option((queue.iterator.min, queue.iterator.max))
      queue.minmaxOption shouldBe expectedValue
      queue.dequeue()
    }
    queue.minmaxOption shouldBe None

  }

  "isEmpty" in {

    val emptyQueue = MinMaxQueue.empty[Int]
    emptyQueue.isEmpty shouldBe true

    val nonEmptyQueue = MinMaxQueue(1)
    nonEmptyQueue.isEmpty shouldBe false

  }

  "nonEmpty" in {

    val emptyQueue = MinMaxQueue.empty[Int]
    emptyQueue.nonEmpty shouldBe false

    val nonEmptyQueue = MinMaxQueue(1)
    nonEmptyQueue.nonEmpty shouldBe true

  }

  "iterator" in {

    val queue = MinMaxQueue.empty[Int]
    queue.enqueue(1)
    queue.enqueue(2)
    queue.enqueue(3)
    queue.iterator.toSeq shouldBe Seq(1, 2, 3)

  }

  "reverseIterator" in {

    val queue = MinMaxQueue.empty[Int]
    queue.enqueue(1)
    queue.enqueue(2)
    queue.enqueue(3)
    queue.reverseIterator.toSeq shouldBe Seq(3, 2, 1)

  }

  "to" in {

    val minmaxQueue = MinMaxQueue(1, 2, 3)
    val queue = minmaxQueue.to(mutable.Queue)

    queue.size shouldBe minmaxQueue.size
    while (queue.nonEmpty) {
      queue.dequeue() shouldBe minmaxQueue.dequeue()
    }

  }

}
