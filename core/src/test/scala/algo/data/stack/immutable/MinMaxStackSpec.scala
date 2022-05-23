package algo.data.stack.immutable

import algo.data.stack.immutable.syntax.*
import algo.testing.BaseSpec
import cats.instances.all.*
import cats.syntax.foldable.*

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
    assert(popAll[Int].runA(stack).value === source)

  }

  "Factory|apply" in {

    val source = Vector(1, 2, 3)
    val stack = MinMaxStack(source*)
    assert(popAll[Int].runA(stack).value === source)

  }

  "Factory|to" in {

    val source = IndexedSeq(1, 2, 3)
    val stack = source.to(MinMaxStack)
    assert(popAll[Int].runA(stack).value === source)

  }

  "size" in {

    for (sz <- 0 until 100) {
      val stack = MinMaxStack.from(Seq.tabulate(sz)(identity))
      assert(size.runA(stack).value === sz)
    }

  }

  "push" in {

    val stack = MinMaxStack.empty[Int]
    val spec = (0 until 100)
      .map { i =>
        for {
          _ <- push(i)
          top <- top[Int]
          size <- size
        } yield {
          assert(top === i)
          assert(size === i + 1)
        }
      }
      .toList
      .sequence_
    spec.runS(stack).value

  }

  "push(value, ...)" in {

    val seed = System.nanoTime()
    withClue(s"seed=[$seed]") {
      val random = new Random(seed)

      val source = IndexedSeq.fill(100)(random.nextInt())
      val stack = MinMaxStack.empty[Int]
      val spec = for {
        _ <- push(0, source.reverse*)
        _ <- source.indices
          .map { i =>
            for {
              top <- top[Int]
              min <- min[Int]
              max <- max[Int]
              _ <- pop[Int]
            } yield {
              assert(top === source(i))
              assert(min === math.min(source.drop(i).min, 0))
              assert(max === math.max(source.drop(i).max, 0))
            }
          }
          .toList
          .sequence_
        _ <- pop[Int].map(value => assert(value === 0))
      } yield ()
      spec.runS(stack).value

    }

  }

  "pushAll" in {

    val seed = System.nanoTime()
    withClue(s"seed=[$seed]") {
      val random = new Random(seed)

      val source = IndexedSeq.fill(100)(random.nextInt())
      val stack = MinMaxStack.empty[Int]
      val spec = for {
        _ <- pushAll(source.reverse)
        _ <- source.indices
          .map { i =>
            for {
              top <- top[Int]
              min <- min[Int]
              max <- max[Int]
              _ <- pop[Int]
            } yield {
              assert(top === source(i))
              assert(min === source.drop(i).min)
              assert(max === source.drop(i).max)
            }
          }
          .toList
          .sequence_
      } yield ()
      spec.runS(stack).value

    }

  }

  "pop" in {

    val stack = MinMaxStack.from(Seq.tabulate(100)(identity))
    val spec = (0 until 100)
      .map { i =>
        for {
          value <- pop[Int]
          size <- size
        } yield {
          assert(value === i)
          assert(size === 100 - (i + 1))
        }
      }
      .toList
      .sequence_
    spec.runS(stack).value

  }

  "pop from an empty stack" in {

    val emptyStack = MinMaxStack.empty[Int]
    intercept[NoSuchElementException] {
      emptyStack.pop()
    }

  }

  "popAll" in {

    val seed = System.nanoTime()
    withClue(s"seed=[$seed]") {
      val random = new Random(seed)

      val source = IndexedSeq.fill(100)(random.nextInt())
      val stack = MinMaxStack.from(source)
      val (newStack, elements) = popAll.run(stack).value
      assert(elements === source)
      assert(newStack.isEmpty)

    }

  }

  "popWhile" in {

    val stack = MinMaxStack(1, 2, 3, 2, 1, 4, 5)
    val spec = for {
      elementsLessThanTree <- popWhile[Int](_ < 3)
      noElements <- popWhile[Int](_ < 3)
      elementsLessThanSix <- popWhile[Int](_ < 6)
    } yield {
      assert(elementsLessThanTree === Seq(1, 2))
      assert(noElements.isEmpty)
      assert(elementsLessThanSix === Seq(3, 2, 1, 4, 5))
    }
    val newStack = spec.runS(stack).value
    assert(newStack.isEmpty)

  }

  "top" in {

    val stack = MinMaxStack.empty[Int]
    val spec = (0 until 100)
      .map { i =>
        for {
          _ <- push(i)
          top <- top[Int]
        } yield {
          assert(top === i)
        }
      }
      .toList
      .sequence_
    spec.runS(stack).value

  }

  "top of an empty stack" in {

    val emptyStack = MinMaxStack.empty[Int]
    intercept[NoSuchElementException] {
      emptyStack.top
    }

  }

  "topOption" in {

    val stack = MinMaxStack.empty[Int]
    assert(topOption.runA(stack).value === None)
    val spec = (0 until 100)
      .map { i =>
        for {
          _ <- push(i)
          topOption <- topOption[Int]
        } yield {
          assert(topOption === Option(i))
        }
      }
      .toList
      .sequence_
    spec.runS(stack).value

  }

  "bottom" in {

    val seed = System.nanoTime()
    withClue(s"seed=[$seed]") {
      val random = new Random(seed)

      val expectedBottom = random.nextInt()
      val stack = MinMaxStack.empty[Int]
      val spec = for {
        _ <- push(expectedBottom)
        _ <- (0 until 10)
          .map { i =>
            for {
              _ <- push(i)
              bottom <- bottom[Int]
            } yield {
              assert(bottom === expectedBottom)
            }
          }
          .toList
          .sequence_
      } yield ()
      spec.runS(stack).value

    }

  }

  "bottom of an empty stack" in {

    val emptyStack = MinMaxStack.empty[Int]
    intercept[NoSuchElementException] {
      emptyStack.bottom
    }

  }

  "bottomOption" in {

    val seed = System.nanoTime()
    withClue(s"seed=[$seed]") {
      val random = new Random(seed)

      val expectedBottom = random.nextInt()
      val stack = MinMaxStack.empty[Int]
      assert(bottomOption.runA(stack).value === None)
      val spec = for {
        _ <- push(expectedBottom)
        _ <- (0 until 10)
          .map { i =>
            for {
              _ <- push(i)
              bottomOption <- bottomOption[Int]
            } yield {
              assert(bottomOption === Option(expectedBottom))
            }
          }
          .toList
          .sequence_
      } yield ()
      spec.runS(stack).value

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
    assert(clear.runS(stack).value.size === 0)

  }

  "ordering" in {

    implicit val reverseOrdering: Ordering[Int] = Ordering.Int.reverse
    val stack = MinMaxStack.empty[Int]
    assert(ordering.runA(stack).value === reverseOrdering)

  }

  "min" in {

    val seed = System.nanoTime()
    withClue(s"seed=[$seed]") {
      val random = new Random(seed)

      val source = IndexedSeq.fill(100)(random.nextInt())
      val stack = MinMaxStack.empty[Int]
      val pushSpec = (0 until 100)
        .map { i =>
          for {
            _ <- push(source(i))
            min <- min[Int]
          } yield {
            assert(min === source.take(i + 1).min)
          }
        }
        .toList
        .sequence_
      val popSpec = (0 until 100)
        .map { i =>
          for {
            min <- min[Int]
            _ <- pop
          } yield {
            assert(min === source.dropRight(i).min)
          }
        }
        .toList
        .sequence_
      val spec = List(pushSpec, popSpec).sequence_
      spec.runS(stack).value

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
      assert(minOption.runA(stack).value === None)
      val pushSpec = (0 until 100)
        .map { i =>
          for {
            _ <- push(source(i))
            minOption <- minOption[Int]
          } yield {
            assert(minOption === Option(source.take(i + 1).min))
          }
        }
        .toList
        .sequence_
      val popSpec = (0 until 100)
        .map { i =>
          for {
            minOption <- minOption[Int]
            _ <- pop
          } yield {
            assert(minOption === Option(source.dropRight(i).min))
          }
        }
        .toList
        .sequence_
      val spec = List(pushSpec, popSpec).sequence_
      spec.runS(stack).value

    }

  }

  "max" in {

    val seed = System.nanoTime()
    withClue(s"seed=[$seed]") {
      val random = new Random(seed)

      val source = IndexedSeq.fill(100)(random.nextInt())
      val stack = MinMaxStack.empty[Int]
      val pushSpec = (0 until 100)
        .map { i =>
          for {
            _ <- push(source(i))
            max <- max[Int]
          } yield {
            assert(max === source.take(i + 1).max)
          }
        }
        .toList
        .sequence_
      val popSpec = (0 until 100)
        .map { i =>
          for {
            max <- max[Int]
            _ <- pop
          } yield {
            assert(max === source.dropRight(i).max)
          }
        }
        .toList
        .sequence_
      val spec = List(pushSpec, popSpec).sequence_
      spec.runS(stack).value

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
      assert(maxOption.runA(stack).value === None)
      val pushSpec = (0 until 100)
        .map { i =>
          for {
            _ <- push(source(i))
            maxOption <- maxOption[Int]
          } yield {
            assert(maxOption === Option(source.take(i + 1).max))
          }
        }
        .toList
        .sequence_
      val popSpec = (0 until 100)
        .map { i =>
          for {
            maxOption <- maxOption[Int]
            _ <- pop
          } yield {
            assert(maxOption === Option(source.dropRight(i).max))
          }
        }
        .toList
        .sequence_
      val spec = List(pushSpec, popSpec).sequence_
      spec.runS(stack).value

    }

  }

  "minmax" in {

    val seed = System.nanoTime()
    withClue(s"seed=[$seed]") {
      val random = new Random(seed)

      val source = IndexedSeq.fill(100)(random.nextInt())
      val stack = MinMaxStack.empty[Int]
      val pushSpec = (0 until 100)
        .map { i =>
          for {
            _ <- push(source(i))
            minmax <- minmax[Int]
          } yield {
            val min = source.take(i + 1).min
            val max = source.take(i + 1).max
            assert(minmax === (min, max))
          }
        }
        .toList
        .sequence_
      val popSpec = (0 until 100)
        .map { i =>
          for {
            minmax <- minmax[Int]
            _ <- pop
          } yield {
            val min = source.dropRight(i).min
            val max = source.dropRight(i).max
            assert(minmax === (min, max))
          }
        }
        .toList
        .sequence_
      val spec = List(pushSpec, popSpec).sequence_
      spec.runS(stack).value

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
      assert(minmaxOption.runA(stack).value === None)
      val pushSpec = (0 until 100)
        .map { i =>
          for {
            _ <- push(source(i))
            minmaxOption <- minmaxOption[Int]
          } yield {
            val min = source.take(i + 1).min
            val max = source.take(i + 1).max
            assert(minmaxOption === Option((min, max)))
          }
        }
        .toList
        .sequence_
      val popSpec = (0 until 100)
        .map { i =>
          for {
            minmaxOption <- minmaxOption[Int]
            _ <- pop
          } yield {
            val min = source.dropRight(i).min
            val max = source.dropRight(i).max
            assert(minmaxOption === Option((min, max)))
          }
        }
        .toList
        .sequence_
      val spec = List(pushSpec, popSpec).sequence_
      spec.runS(stack).value

    }

  }

  "isEmpty" in {

    val emptyStack = MinMaxStack.empty[Int]
    assert(isEmpty.runA(emptyStack).value)

    val nonEmptyStack = MinMaxStack(1)
    assert(!isEmpty.runA(nonEmptyStack).value)

  }

  "nonEmpty" in {

    val emptyStack = MinMaxStack.empty[Int]
    assert(!nonEmpty.runA(emptyStack).value)

    val nonEmptyStack = MinMaxStack(1)
    assert(nonEmpty.runA(nonEmptyStack).value)

  }

  "iterator" in {

    val stack = MinMaxStack(3, 2, 1)
    assert(iterator.runA(stack).value.toSeq === Seq(3, 2, 1))

  }

  "knownSize" in {

    for (size <- 0 until 100) {
      val stack = MinMaxStack.from(Seq.tabulate(size)(identity))
      assert(knownSize.runA(stack).value === size)
    }

  }

  "reverseIterator" in {

    val stack = MinMaxStack(3, 2, 1)
    assert(stack.iterator.toSeq === Seq(3, 2, 1))
    assert(reverseIterator.runA(stack).value.toSeq === Seq(1, 2, 3))

  }

  "to" in {

    var minmaxStack = MinMaxStack(1, 2, 3)
    val stack = minmaxStack.to(mutable.Stack)

    assert(stack.size === minmaxStack.size)
    while (stack.nonEmpty) {
      val (value, newMinmaxStack) = minmaxStack.pop()
      minmaxStack = newMinmaxStack
      assert(stack.pop() === value)
    }

  }

}
