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
      val expectedMinMax = (queue.min, queue.max)
      assert(minmaxQueue.minmax === expectedMinMax)
      assert(minmaxQueue.dequeue() === queue.dequeue())
    }

  }

  "size" in {

    for (size <- 0 until 100) {
      val queue = MinMaxQueue.from(Seq.tabulate(size)(identity))
      assert(queue.size === size)
    }

  }

  "enqueue" in {

    val queue = MinMaxQueue.empty[Int]
    for (i <- 0 until 100) {
      queue.enqueue(i)
      assert(queue.back === i)
      assert(queue.size === i + 1)
    }

  }

  "enqueue(value, ...)" in {

    val seed = System.nanoTime()
    withClue(s"seed=[$seed]") {
      val random = new Random(seed)

      val source = IndexedSeq.fill(100)(random.nextInt())
      val queue = MinMaxQueue.empty[Int]
      queue.enqueue(source.head, source.drop(1)*)
      for (i <- source.indices) {
        assert(queue.min === source.drop(i).min)
        assert(queue.max === source.drop(i).max)
        assert(queue.dequeue() === source(i))
      }

    }

  }

  "enqueueAll" in {

    val seed = System.nanoTime()
    withClue(s"seed=[$seed]") {
      val random = new Random(seed)

      val source = IndexedSeq.fill(100)(random.nextInt())
      val queue = MinMaxQueue.empty[Int]
      queue.enqueueAll(source)
      for (i <- source.indices) {
        assert(queue.min === source.drop(i).min)
        assert(queue.max === source.drop(i).max)
        assert(queue.dequeue() === source(i))
      }

    }

  }

  "dequeue" in {

    val queue = MinMaxQueue.from(IndexedSeq.tabulate(100)(identity))
    for (i <- 0 until 100) {
      assert(queue.dequeue() === i)
      assert(queue.size === 100 - (i + 1))
    }

  }

  "dequeue from an empty queue" in {

    val emptyQueue = MinMaxQueue.empty[Int]
    intercept[NoSuchElementException] {
      emptyQueue.dequeue()
    }

  }

  "dequeueAll" in {

    val seed = System.nanoTime()
    withClue(s"seed=[$seed]") {
      val random = new Random(seed)

      val source = IndexedSeq.fill(10)(random.nextInt())
      val queue = MinMaxQueue.from(source)
      val elements = queue.dequeueAll()
      assert(elements === source)
      assert(queue.isEmpty)

    }

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

    val seed = System.nanoTime()
    withClue(s"seed=[$seed]") {
      val random = new Random(seed)

      val queue = MinMaxQueue.empty[Int]
      val expectedFront = random.nextInt()
      queue.enqueue(expectedFront)
      for (i <- 0 until 100) {
        queue.enqueue(i)
        assert(queue.front === expectedFront)
      }

    }

  }

  "front of an empty queue" in {

    val emptyStack = MinMaxQueue.empty[Int]
    intercept[NoSuchElementException] {
      emptyStack.front
    }

  }

  "frontOption" in {

    val seed = System.nanoTime()
    withClue(s"seed=[$seed]") {
      val random = new Random(seed)

      val queue = MinMaxQueue.empty[Int]
      val expectedFront = random.nextInt()
      assert(queue.frontOption === None)
      queue.enqueue(expectedFront)
      for (i <- 0 until 100) {
        queue.enqueue(i)
        assert(queue.frontOption === Option(expectedFront))
      }

    }

  }

  "back" in {

    val queue = MinMaxQueue.empty[Int]
    for (i <- 0 until 100) {
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
    assert(queue.backOption === None)
    for (i <- 0 until 100) {
      queue.enqueue(i)
      assert(queue.backOption === Option(i))
    }

  }

  "apply" in {

    val queue = MinMaxQueue.empty[Int]
    // To vary internal, do some enqueue & dequeue
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

  }

  "min" in {

    val seed = System.nanoTime()
    withClue(s"seed=[$seed]") {
      val random = new Random(seed)

      val source = IndexedSeq.fill(150)(random.nextInt())
      val queue = MinMaxQueue.empty[Int]
      for (i <- 0 until 100) {
        queue.enqueue(source(i))
        assert(queue.min === source.take(i + 1).min)
      }
      for (i <- 0 until 50) {
        assert(queue.min === source.slice(i, 100).min)
        queue.dequeue()
      }
      for (i <- 100 until 150) {
        queue.enqueue(source(i))
        assert(queue.min === source.slice(50, i + 1).min)
      }
      for (i <- 50 until 150) {
        assert(queue.min === source.drop(i).min)
        queue.dequeue()
      }

    }

  }

  "min of an empty queue" in {

    val emptyQueue = MinMaxQueue.empty[Int]
    intercept[NoSuchElementException] {
      emptyQueue.min
    }

  }

  "minOption" in {

    val seed = System.nanoTime()
    withClue(s"seed=[$seed]") {
      val random = new Random(seed)

      val source = IndexedSeq.fill(150)(random.nextInt())
      val queue = MinMaxQueue.empty[Int]
      assert(queue.minOption === None)
      for (i <- 0 until 100) {
        queue.enqueue(source(i))
        assert(queue.minOption === source.take(i + 1).minOption)
      }
      for (i <- 0 until 50) {
        assert(queue.minOption === source.slice(i, 100).minOption)
        queue.dequeue()
      }
      for (i <- 100 until 150) {
        queue.enqueue(source(i))
        assert(queue.minOption === source.slice(50, i + 1).minOption)
      }
      for (i <- 50 until 150) {
        assert(queue.minOption === source.drop(i).minOption)
        queue.dequeue()
      }
      assert(queue.minOption === None)

    }

  }

  "max" in {

    val seed = System.nanoTime()
    withClue(s"seed=[$seed]") {
      val random = new Random(seed)

      val source = IndexedSeq.fill(150)(random.nextInt())
      val queue = MinMaxQueue.empty[Int]
      for (i <- 0 until 100) {
        queue.enqueue(source(i))
        assert(queue.max === source.take(i + 1).max)
      }
      for (i <- 0 until 50) {
        assert(queue.max === source.slice(i, 100).max)
        queue.dequeue()
      }
      for (i <- 100 until 150) {
        queue.enqueue(source(i))
        assert(queue.max === source.slice(50, i + 1).max)
      }
      for (i <- 50 until 150) {
        assert(queue.max === source.drop(i).max)
        queue.dequeue()
      }

    }

  }

  "max of an empty queue" in {

    val emptyQueue = MinMaxQueue.empty[Int]
    intercept[NoSuchElementException] {
      emptyQueue.max
    }

  }

  "maxOption" in {

    val seed = System.nanoTime()
    withClue(s"seed=[$seed]") {
      val random = new Random(seed)

      val source = IndexedSeq.fill(150)(random.nextInt())
      val queue = MinMaxQueue.empty[Int]
      assert(queue.maxOption === None)
      for (i <- 0 until 100) {
        queue.enqueue(source(i))
        assert(queue.maxOption === source.take(i + 1).maxOption)
      }
      for (i <- 0 until 50) {
        assert(queue.maxOption === source.slice(i, 100).maxOption)
        queue.dequeue()
      }
      for (i <- 100 until 150) {
        queue.enqueue(source(i))
        assert(queue.maxOption === source.slice(50, i + 1).maxOption)
      }
      for (i <- 50 until 150) {
        assert(queue.maxOption === source.drop(i).maxOption)
        queue.dequeue()
      }
      assert(queue.maxOption === None)

    }

  }

  "minmax" in {

    val seed = System.nanoTime()
    withClue(s"seed=[$seed]") {
      val random = new Random(seed)

      val source = IndexedSeq.fill(150)(random.nextInt())
      val queue = MinMaxQueue.empty[Int]
      for (i <- 0 until 100) {
        queue.enqueue(source(i))
        val expected = (source.take(i + 1).min, source.take(i + 1).max)
        assert(queue.minmax === expected)
      }
      for (i <- 0 until 50) {
        val expected = (source.slice(i, 100).min, source.slice(i, 100).max)
        assert(queue.minmax === expected)
        queue.dequeue()
      }
      for (i <- 100 until 150) {
        queue.enqueue(source(i))
        val expected =
          (source.slice(50, i + 1).min, source.slice(50, i + 1).max)
        assert(queue.minmax === expected)
      }
      for (i <- 50 until 150) {
        val expected =
          (source.drop(i).min, source.drop(i).max)
        assert(queue.minmax === expected)
        queue.dequeue()
      }

    }

  }

  "minmax of an empty queue" in {

    val emptyQueue = MinMaxQueue.empty[Int]
    intercept[NoSuchElementException] {
      emptyQueue.minmax
    }

  }

  "minmaxOption" in {

    val seed = System.nanoTime()
    withClue(s"seed=[$seed]") {
      val random = new Random(seed)

      val source = IndexedSeq.fill(150)(random.nextInt())
      val queue = MinMaxQueue.empty[Int]
      assert(queue.minmaxOption === None)
      for (i <- 0 until 100) {
        queue.enqueue(source(i))
        val expected = Option((source.take(i + 1).min, source.take(i + 1).max))
        assert(queue.minmaxOption === expected)
      }
      for (i <- 0 until 50) {
        val expected =
          Option((source.slice(i, 100).min, source.slice(i, 100).max))
        assert(queue.minmaxOption === expected)
        queue.dequeue()
      }
      for (i <- 100 until 150) {
        queue.enqueue(source(i))
        val expected =
          Option((source.slice(50, i + 1).min, source.slice(50, i + 1).max))
        assert(queue.minmaxOption === expected)
      }
      for (i <- 50 until 150) {
        val expected =
          Option((source.drop(i).min, source.drop(i).max))
        assert(queue.minmaxOption === expected)
        queue.dequeue()
      }
      assert(queue.minmaxOption === None)

    }

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
    queue.dequeue()
    queue.enqueue(3)
    queue.enqueue(4)
    assert(queue.iterator.toSeq === Seq(2, 3, 4))

  }

  "knownSize" in {

    for (size <- 0 until 100) {
      val queue = MinMaxQueue.from(Seq.tabulate(size)(identity))
      assert(queue.knownSize === size)
    }

  }

  "reverseIterator" in {

    val queue = MinMaxQueue.empty[Int]
    queue.enqueue(1)
    queue.enqueue(2)
    queue.dequeue()
    queue.enqueue(3)
    queue.enqueue(4)
    assert(queue.reverseIterator.toSeq === Seq(4, 3, 2))

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
