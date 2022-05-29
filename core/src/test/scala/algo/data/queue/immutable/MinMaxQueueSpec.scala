package algo.data.queue.immutable

import algo.data.queue.immutable.syntax.*
import algo.testing.BaseSpec
import cats.instances.all.*
import cats.syntax.foldable.*

import scala.collection.immutable
import scala.util.Random

final class MinMaxQueueSpec extends BaseSpec {

  "Factory|empty" in {

    val queue = MinMaxQueue.empty[Int]
    assert(queue.size === 0)

  }

  "Factory|from" in {

    val source = Vector(1, 2, 3)
    val queue = MinMaxQueue.from(source)
    assert(queue.size === source.size)
    assert(dequeueAll.runA(queue).value === source)

  }

  "Factory|apply" in {

    val source = Vector(1, 2, 3)
    val queue = MinMaxQueue(source*)
    assert(queue.size === source.size)
    assert(dequeueAll.runA(queue).value === source)

  }

  "Factory|to" in {

    val queue = immutable.Queue(1, 2, 3)
    val minmaxQueue = queue.to(MinMaxQueue)

    assert(minmaxQueue.size === queue.size)
    val spec = List
      .unfold(queue) { queue =>
        if (queue.nonEmpty) {
          val minimum = queue.min
          val maximum = queue.max
          val (value, newQueue) = queue.dequeue
          Some(((value, minimum, maximum), newQueue))
        } else {
          None
        }
      }
      .map { case (i, minimum, maximum) =>
        for {
          minmax <- minmax[Int]
          value <- dequeue
        } yield {
          assert(minmax === (minimum, maximum))
          assert(value === i)
        }
      }
      .sequence_
    spec.run(minmaxQueue).value

  }

  "size" in {

    for (size <- 0 until 100) {
      val queue = MinMaxQueue.from(Seq.tabulate(size)(identity))
      assert(queue.size === size)
    }

  }

  "enqueue" in {

    val queue = MinMaxQueue.empty[Int]
    val spec = (0 until 100)
      .map { i =>
        for {
          _ <- enqueue(i)
          back <- back
          size <- size
        } yield {
          assert(back === i)
          assert(size === i + 1)
        }
      }
      .toList
      .sequence_
    spec.run(queue).value

  }

  "enqueue(value, ...)" in {

    val seed = System.nanoTime()
    withClue(s"seed=[$seed]") {
      val random = new Random(seed)

      val source = IndexedSeq.fill(100)(random.nextInt())
      val queue = MinMaxQueue.empty[Int]
      val spec = for {
        _ <- enqueue(source.head, source.drop(1)*)
        _ <- source.indices
          .map { i =>
            for {
              min <- min[Int]
              max <- max
              value <- dequeue
            } yield {
              assert(min === source.drop(i).min)
              assert(max === source.drop(i).max)
              assert(value === source(i))
            }
          }
          .toList
          .sequence_
      } yield ()
      spec.run(queue).value
    }

  }

  "enqueueAll" in {

    val seed = System.nanoTime()
    withClue(s"seed=[$seed]") {
      val random = new Random(seed)

      val source = IndexedSeq.fill(100)(random.nextInt())
      val queue = MinMaxQueue.empty[Int]
      val spec = for {
        _ <- enqueueAll(source)
        _ <- source.indices
          .map { i =>
            for {
              min <- min[Int]
              max <- max
              value <- dequeue
            } yield {
              assert(min === source.drop(i).min)
              assert(max === source.drop(i).max)
              assert(value === source(i))
            }
          }
          .toList
          .sequence_
      } yield ()
      spec.run(queue).value
    }

  }

  "dequeue" in {

    val queue = MinMaxQueue.from(IndexedSeq.tabulate(100)(identity))
    val spec = (0 until 100)
      .map { i =>
        for {
          value <- dequeue[Int]
          size <- size
        } yield {
          assert(value === i)
          assert(size === 100 - i - 1)
        }
      }
      .toList
      .sequence_
    spec.run(queue).value

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

      val source = IndexedSeq.fill(100)(random.nextInt())
      val queue = MinMaxQueue.from(source)
      val (newQueue, elements) = dequeueAll.run(queue).value
      assert(elements === source)
      assert(newQueue.size === 0)

    }

  }

  "dequeueWhile" in {

    val queue = MinMaxQueue(2, 1, 3, 2, 1, 4, 5)
    val spec = for {
      elementsLessThanThree <- dequeueWhile[Int](_ < 3)
      noElements <- dequeueWhile[Int](_ < 3)
      elementsLessThanSix <- dequeueWhile[Int](_ < 6)
    } yield {
      assert(elementsLessThanThree === Seq(2, 1))
      assert(noElements.isEmpty)
      assert(elementsLessThanSix === Seq(3, 2, 1, 4, 5))
    }
    assert(spec.runS(queue).value.isEmpty)

  }

  "front" in {

    val seed = System.nanoTime()
    withClue(s"seed=[$seed]") {
      val random = new Random(seed)

      val queue = MinMaxQueue.empty[Int]
      val expectedFront = random.nextInt()
      val spec = for {
        _ <- enqueue(expectedFront)
        _ <- (0 until 100)
          .map { i =>
            for {
              _ <- enqueue(i)
              front <- front
            } yield {
              assert(front === expectedFront)
            }
          }
          .toList
          .sequence_
      } yield ()
      spec.run(queue).value

    }

  }

  "front of an empty queue" in {

    val emptyQueue = MinMaxQueue.empty[Int]
    intercept[NoSuchElementException] {
      emptyQueue.front
    }

  }

  "frontOption" in {

    val seed = System.nanoTime()
    withClue(s"seed=[$seed]") {
      val random = new Random(seed)

      val queue = MinMaxQueue.empty[Int]
      val expectedFront = random.nextInt()
      assert(queue.frontOption === None)
      val spec = for {
        _ <- enqueue(expectedFront)
        _ <- (0 until 100)
          .map { i =>
            for {
              _ <- enqueue(i)
              frontOption <- frontOption
            } yield {
              assert(frontOption === Option(expectedFront))
            }
          }
          .toList
          .sequence_
      } yield ()
      spec.run(queue).value

    }

  }

  "back" in {

    val queue = MinMaxQueue.empty[Int]
    val spec = (0 until 100)
      .map { i =>
        for {
          _ <- enqueue(i)
          back <- back
        } yield {
          assert(back == i)
        }
      }
      .toList
      .sequence_
    spec.run(queue).value

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
    val spec = (0 until 100)
      .map { i =>
        for {
          _ <- enqueue(i)
          backOption <- backOption
        } yield {
          assert(backOption == Option(i))
        }
      }
      .toList
      .sequence_
    spec.run(queue).value

  }

  "apply" in {

    // To vary internal, do some enqueue & dequeue
    val initialize = for {
      _ <- enqueue(1)
      _ <- enqueue(2)
      _ <- enqueue(3)
      _ <- dequeue
      _ <- enqueue(4)
      _ <- enqueue(5)
    } yield ()
    val queue = initialize.runS(MinMaxQueue.empty[Int]).value

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
    val newQueue = clear.runS(queue).value
    assert(newQueue.size === 0)

  }

  "ordering" in {

    implicit val reverseOrdering: Ordering[Int] = Ordering.Int.reverse
    val queue = MinMaxQueue.empty[Int]
    assert(ordering.runA(queue).value === reverseOrdering)

  }

  "min" in {

    val seed = System.nanoTime()
    withClue(s"seed=[$seed]") {
      val random = new Random(seed)

      val source = IndexedSeq.fill(150)(random.nextInt())
      val queue = MinMaxQueue.empty[Int]

      val spec1 = (0 until 100).map { i =>
        for {
          _ <- enqueue(source(i))
          min <- min
        } yield {
          assert(min === source.take(i + 1).min)
        }
      }
      val spec2 = (0 until 50).map { i =>
        for {
          min <- min[Int]
          _ <- dequeue
        } yield {
          assert(min === source.slice(i, 100).min)
        }
      }
      val spec3 = (100 until 150).map { i =>
        for {
          _ <- enqueue(source(i))
          min <- min
        } yield {
          assert(min === source.slice(50, i + 1).min)
        }
      }
      val spec4 = (50 until 150).map { i =>
        for {
          min <- min[Int]
          _ <- dequeue
        } yield {
          assert(min === source.drop(i).min)
        }
      }
      val spec = List
        .concat(
          spec1,
          spec2,
          spec3,
          spec4
        )
        .sequence_
      spec.run(queue).value

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

      val spec1 = (0 until 100).map { i =>
        for {
          _ <- enqueue(source(i))
          minOption <- minOption
        } yield {
          assert(minOption === source.take(i + 1).minOption)
        }
      }
      val spec2 = (0 until 50).map { i =>
        for {
          minOption <- minOption[Int]
          _ <- dequeue
        } yield {
          assert(minOption === source.slice(i, 100).minOption)
        }
      }
      val spec3 = (100 until 150).map { i =>
        for {
          _ <- enqueue(source(i))
          minOption <- minOption
        } yield {
          assert(minOption === source.slice(50, i + 1).minOption)
        }
      }
      val spec4 = (50 until 150).map { i =>
        for {
          minOption <- minOption[Int]
          _ <- dequeue
        } yield {
          assert(minOption === source.drop(i).minOption)
        }
      }
      val spec = List
        .concat(
          spec1,
          spec2,
          spec3,
          spec4
        )
        .sequence_

      assert(queue.minOption === None)
      val newQueue = spec.runS(queue).value
      assert(newQueue.minOption === None)

    }

  }

  "max" in {

    val seed = System.nanoTime()
    withClue(s"seed=[$seed]") {
      val random = new Random(seed)

      val source = IndexedSeq.fill(150)(random.nextInt())
      val queue = MinMaxQueue.empty[Int]

      val spec1 = (0 until 100).map { i =>
        for {
          _ <- enqueue(source(i))
          max <- max
        } yield {
          assert(max === source.take(i + 1).max)
        }
      }
      val spec2 = (0 until 50).map { i =>
        for {
          max <- max[Int]
          _ <- dequeue
        } yield {
          assert(max === source.slice(i, 100).max)
        }
      }
      val spec3 = (100 until 150).map { i =>
        for {
          _ <- enqueue(source(i))
          max <- max
        } yield {
          assert(max === source.slice(50, i + 1).max)
        }
      }
      val spec4 = (50 until 150).map { i =>
        for {
          max <- max[Int]
          _ <- dequeue
        } yield {
          assert(max === source.drop(i).max)
        }
      }
      val spec = List
        .concat(
          spec1,
          spec2,
          spec3,
          spec4
        )
        .sequence_
      spec.run(queue).value

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

      val spec1 = (0 until 100).map { i =>
        for {
          _ <- enqueue(source(i))
          maxOption <- maxOption
        } yield {
          assert(maxOption === source.take(i + 1).maxOption)
        }
      }
      val spec2 = (0 until 50).map { i =>
        for {
          maxOption <- maxOption[Int]
          _ <- dequeue
        } yield {
          assert(maxOption === source.slice(i, 100).maxOption)
        }
      }
      val spec3 = (100 until 150).map { i =>
        for {
          _ <- enqueue(source(i))
          maxOption <- maxOption
        } yield {
          assert(maxOption === source.slice(50, i + 1).maxOption)
        }
      }
      val spec4 = (50 until 150).map { i =>
        for {
          maxOption <- maxOption[Int]
          _ <- dequeue
        } yield {
          assert(maxOption === source.drop(i).maxOption)
        }
      }
      val spec = List
        .concat(
          spec1,
          spec2,
          spec3,
          spec4
        )
        .sequence_

      assert(queue.maxOption === None)
      val newQueue = spec.runS(queue).value
      assert(newQueue.maxOption === None)

    }

  }

  "minmax" in {

    val seed = System.nanoTime()
    withClue(s"seed=[$seed]") {
      val random = new Random(seed)

      val source = IndexedSeq.fill(150)(random.nextInt())
      val queue = MinMaxQueue.empty[Int]

      val spec1 = (0 until 100).map { i =>
        for {
          _ <- enqueue(source(i))
          minmax <- minmax
        } yield {
          val expected = (source.take(i + 1).min, source.take(i + 1).max)
          assert(minmax === expected)
        }
      }
      val spec2 = (0 until 50).map { i =>
        for {
          minmax <- minmax[Int]
          _ <- dequeue
        } yield {
          val expected = (source.slice(i, 100).min, source.slice(i, 100).max)
          assert(minmax === expected)
        }
      }
      val spec3 = (100 until 150).map { i =>
        for {
          _ <- enqueue(source(i))
          minmax <- minmax
        } yield {
          val expected =
            (source.slice(50, i + 1).min, source.slice(50, i + 1).max)
          assert(minmax === expected)
        }
      }
      val spec4 = (50 until 150).map { i =>
        for {
          minmax <- minmax[Int]
          _ <- dequeue
        } yield {
          val expected =
            (source.drop(i).min, source.drop(i).max)
          assert(minmax === expected)
        }
      }
      val spec = List
        .concat(
          spec1,
          spec2,
          spec3,
          spec4
        )
        .sequence_
      spec.run(queue).value

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

      val spec1 = (0 until 100).map { i =>
        for {
          _ <- enqueue(source(i))
          minmaxOption <- minmaxOption
        } yield {
          val expected =
            Option((source.take(i + 1).min, source.take(i + 1).max))
          assert(minmaxOption === expected)
        }
      }
      val spec2 = (0 until 50).map { i =>
        for {
          minmaxOption <- minmaxOption[Int]
          _ <- dequeue
        } yield {
          val expected =
            Option((source.slice(i, 100).min, source.slice(i, 100).max))
          assert(minmaxOption === expected)
        }
      }
      val spec3 = (100 until 150).map { i =>
        for {
          _ <- enqueue(source(i))
          minmaxOption <- minmaxOption
        } yield {
          val expected =
            Option((source.slice(50, i + 1).min, source.slice(50, i + 1).max))
          assert(minmaxOption === expected)
        }
      }
      val spec4 = (50 until 150).map { i =>
        for {
          minmaxOption <- minmaxOption[Int]
          _ <- dequeue
        } yield {
          val expected =
            Option((source.drop(i).min, source.drop(i).max))
          assert(minmaxOption === expected)
        }
      }
      val spec = List
        .concat(
          spec1,
          spec2,
          spec3,
          spec4
        )
        .sequence_

      assert(queue.minmaxOption === None)
      val newQueue = spec.runS(queue).value
      assert(newQueue.minmaxOption === None)

    }

  }

  "isEmpty" in {

    val emptyQueue = MinMaxQueue.empty[Int]
    assert(isEmpty.runA(emptyQueue).value)

    val nonEmptyQueue = MinMaxQueue(1)
    assert(!isEmpty.runA(nonEmptyQueue).value)

  }

  "nonEmpty" in {

    val emptyQueue = MinMaxQueue.empty[Int]
    assert(!nonEmpty.runA(emptyQueue).value)

    val nonEmptyQueue = MinMaxQueue(1)
    assert(nonEmpty.runA(nonEmptyQueue).value)

  }

  "iterator" in {

    val initialize = for {
      _ <- enqueue(1)
      _ <- enqueue(2)
      _ <- dequeue
      _ <- enqueue(3)
      _ <- enqueue(4)
    } yield ()
    val queue = initialize.runS(MinMaxQueue.empty[Int]).value
    assert(iterator.runA(queue).value.toSeq === Seq(2, 3, 4))

  }

  "knownSize" in {

    for (size <- 0 until 100) {
      val queue = MinMaxQueue.from(Seq.tabulate(size)(identity))
      assert(knownSize.runA(queue).value === size)
    }

  }

  "reverseIterator" in {

    val initialize = for {
      _ <- enqueue(1)
      _ <- enqueue(2)
      _ <- dequeue
      _ <- enqueue(3)
      _ <- enqueue(4)
    } yield ()
    val queue = initialize.runS(MinMaxQueue.empty[Int]).value
    assert(reverseIterator.runA(queue).value.toSeq === Seq(4, 3, 2))

  }

  "to" in {

    val minmaxQueue = MinMaxQueue(1, 2, 3)
    val queue = minmaxQueue.to(immutable.Queue)
    val spec = List
      .unfold(queue)(_.dequeueOption)
      .map { n =>
        for {
          value <- dequeue[Int]
        } yield {
          assert(value === n)
        }
      }
      .sequence_
    spec.run(minmaxQueue).value

  }

}
