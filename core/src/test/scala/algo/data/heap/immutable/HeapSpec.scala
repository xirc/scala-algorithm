package algo.data.heap.immutable

import algo.data.heap.immutable.syntax.*
import algo.testing.BaseSpec
import cats.Eval
import cats.data.IndexedStateT
import cats.instances.all.*
import cats.syntax.foldable.*

import scala.util.Random

final class HeapSpec extends BaseSpec {

  "Factory|empty" in {

    val heap = Heap.empty[Int]
    assert(size.runA(heap).value === 0)

  }

  "Factory|from" in {

    val heap = Heap.from(Seq(1, 5, 3, 2, 4))
    assert(size.runA(heap).value === 5)
    assert(popAll.runA(heap).value === Seq(5, 4, 3, 2, 1))

  }

  "Factory|apply" in {

    val heap = Heap(1, 5, 3, 2, 4)
    assert(size.runA(heap).value === 5)
    assert(popAll.runA(heap).value === Seq(5, 4, 3, 2, 1))

  }

  "Factory|fill" in {

    val xs = IndexedSeq(1, 5, 3, 2, 4)
    val heap = {
      val it = xs.iterator
      Heap.fill(5) { it.next() }
    }
    assert(size.runA(heap).value === 5)
    assert(popAll.runA(heap).value === Seq(5, 4, 3, 2, 1))

  }

  "Factory|tabulate" in {

    val heap = Heap.tabulate(5)(n => n + 1)
    assert(size.runA(heap).value === 5)
    assert(popAll.runA(heap).value === Seq(5, 4, 3, 2, 1))

  }

  "Factory|iterate" in {

    val heap = Heap.iterate(1, 5)(n => n + 1)
    assert(size.runA(heap).value === 5)
    assert(popAll.runA(heap).value === Seq(5, 4, 3, 2, 1))

  }

  "Factory|unfold" in {

    val heap = Heap.unfold(1) {
      case n if n <= 5 => Some((n, n + 1))
      case _           => None
    }
    assert(size.runA(heap).value === 5)
    assert(popAll.runA(heap).value === Seq(5, 4, 3, 2, 1))

  }

  "Factory|withBranchingFactor" in {

    val heap = Heap.withBranchingFactor(2)(3, 5, 2, 4, 1)
    assert(branchingFactor.runA(heap).value === 2)
    assert(popAll.runA(heap).value === Seq(5, 4, 3, 2, 1))

  }

  "Factory|withBranchingFactor(1)" in {

    intercept[IllegalArgumentException] {
      Heap.withBranchingFactor(1)
    }

  }

  "Factory|to" in {

    val defaultHeap = Seq(3, 5, 2, 4, 1).to(Heap)
    assert(branchingFactor.runA(defaultHeap).value === 4)
    assert(popAll.runA(defaultHeap).value === Seq(5, 4, 3, 2, 1))

    val customHeap = Seq(3, 5, 2, 4, 1).to(Heap.withBranchingFactor(2))
    assert(branchingFactor.runA(customHeap).value === 2)
    assert(popAll.runA(customHeap).value === Seq(5, 4, 3, 2, 1))

  }

  "Factory|newBuilder" in {

    val builder = Heap.newBuilder[Int]
    builder.addOne(3)
    builder.clear()
    builder.addOne(1)
    builder.addAll(Seq(5, 2, 4))

    val heap = builder.result()
    assert(branchingFactor.runA(heap).value === 4)
    assert(size.runA(heap).value === 4)
    assert(popAll.runA(heap).value === Seq(5, 4, 2, 1))

  }

  "size" in {

    for (sz <- 0 until 100) {
      val heap = Heap.from(Seq.tabulate(sz)(identity))
      assert(size.runA(heap).value === sz)
    }

  }

  "isEmpty" in {

    val emptyHeap = Heap.empty[Int]
    assert(size.runA(emptyHeap).value === 0)
    assert(isEmpty.runA(emptyHeap).value === true)

    val nonEmptyHeap = Heap(1)
    assert(size.runA(nonEmptyHeap).value === 1)
    assert(isEmpty.runA(nonEmptyHeap).value === false)

  }

  "nonEmpty" in {

    val emptyHeap = Heap.empty[Int]
    assert(size.runA(emptyHeap).value === 0)
    assert(nonEmpty.runA(emptyHeap).value === false)

    val nonEmptyHeap = Heap(1)
    assert(size.runA(nonEmptyHeap).value === 1)
    assert(nonEmpty.runA(nonEmptyHeap).value === true)

  }

  "ordering" in {

    val heap = Heap.empty[Int](Ordering.Int)
    assert(ordering.runA(heap).value === Ordering.Int)

    val reverseOrdering = Ordering.Int.reverse
    val reverseHeap = Heap.empty[Int](reverseOrdering)
    assert(reverseHeap.ordering === reverseOrdering)

  }

  "branchingFactor" in {

    val defaultHeap = Heap.empty[Int]
    assert(branchingFactor.runA(defaultHeap).value === 4)

    val customHeap = Heap.withBranchingFactor(2).empty[Int]
    assert(branchingFactor.runA(customHeap).value === 2)

  }

  "top" in {

    val heap = Heap.empty[Int]
    val xs = IndexedSeq(1, 3, 5, 2, 4, 6, 8, 9, 7, 10)
    val expected = IndexedSeq(1, 3, 5, 5, 5, 6, 8, 9, 9, 10)

    val topSpec = xs.indices
      .map { i =>
        for {
          _ <- push(xs(i))
          v <- top
        } yield {
          assert(v === expected(i))
        }
      }
      .toList
      .sequence_

    topSpec.run(heap).value

  }

  "top from an empty heap" in {

    intercept[NoSuchElementException] {
      top.runA(Heap.empty[Int]).value
    }

  }

  "topOption" in {

    val heap = Heap.empty[Int]
    val xs = IndexedSeq(1, 3, 5, 2, 4, 6, 8, 9, 7, 10)
    val expected = IndexedSeq(1, 3, 5, 5, 5, 6, 8, 9, 9, 10)

    val topOptionSpec = xs.indices
      .map { i =>
        for {
          _ <- push(xs(i))
          v <- topOption
        } yield {
          assert(v.contains(expected(i)))
        }
      }
      .toList
      .sequence_

    topOptionSpec.run(heap).value

  }

  "topOption from an empty heap" in {

    assert(topOption.runA(Heap.empty[Int]).value === None)

  }

  "pop" in {

    val heap = Heap(1, 5, 3, 2, 4)
    val popSpec = (5 to 1 by -1)
      .map { n =>
        pop[Int].map(v => assert(v === n))
      }
      .toList
      .sequence_
    popSpec.run(heap).value

  }

  "pop from an empty heap" in {

    intercept[NoSuchElementException] {
      pop.runA(Heap.empty[Int]).value
    }

  }

  "popAll from non-empty heap" in {

    val heap = Heap(1, 5, 3, 2, 4)
    val popAllSpec = for {
      xs <- popAll[Int]
      sz <- size
    } yield {
      assert(xs === Seq(5, 4, 3, 2, 1))
      assert(sz === 0)
    }
    popAllSpec.run(heap).value

  }

  "popAll from empty heap" in {

    assert(popAll[Int].runA(Heap.empty[Int]).value === Seq.empty)

  }

  "push" in {

    val heap = Heap.empty[Int]
    val pushSpec = for {
      // push 1
      _ <- push(1)
      _ <- size.map(s => assert(s === 1))
      _ <- top[Int].map(v => assert(v === 1))
      _ <- contains(1).map(c => assert(c))
      // push 5
      _ <- push(5)
      _ <- size.map(s => assert(s === 2))
      _ <- top[Int].map(v => assert(v === 5))
      _ <- contains(5).map(c => assert(c))
      // push 3
      _ <- push(3)
      _ <- size.map(s => assert(s === 3))
      _ <- top[Int].map(v => assert(v === 5))
      _ <- contains(3).map(c => assert(c))
      // push 1 again
      // Heap will not grow if the element is already in the heap.
      _ <- push(1)
      _ <- size.map(s => assert(s === 3))
      _ <- top[Int].map(v => assert(v === 5))
      _ <- contains(1).map(c => assert(c))
    } yield ()
    pushSpec.run(heap).value

  }

  "contains" in {

    val heap = Heap(1, 2, 3, 4, 5)
    val containsSpec = for {
      _ <- contains(1).map(c => assert(c === true))
      _ <- contains(2).map(c => assert(c === true))
      _ <- contains(3).map(c => assert(c === true))
      _ <- contains(4).map(c => assert(c === true))
      _ <- contains(5).map(c => assert(c === true))
      _ <- contains(0).map(c => assert(c === false))
      _ <- contains(6).map(c => assert(c === false))
    } yield ()
    containsSpec.run(heap).value

  }

  "remove" in {

    val heap = Heap(3, 1, 2, 4, 5, 6)
    val removeSpec = for {
      // remove 3
      _ <- remove(3)
      _ <- size.map(s => assert(s === 5))
      _ <- top[Int].map(v => assert(v === 6))
      _ <- contains(3).map(c => assert(c === false))
      // remove 5
      _ <- remove(5)
      _ <- size.map(s => assert(s === 4))
      _ <- top[Int].map(v => assert(v === 6))
      _ <- contains(5).map(c => assert(c === false))
      // remove 0
      // Heap will not change if the element is not in the heap.
      _ <- remove(0)
      _ <- size.map(s => assert(s === 4))
      _ <- top[Int].map(v => assert(v === 6))
      _ <- contains(0).map(c => assert(c === false))
      // Remaining elements are Seq(6,4,2,1)
      _ <- popAll[Int].map(xs => assert(xs === Seq(6, 4, 2, 1)))
    } yield ()
    removeSpec.run(heap).value

  }

  "update" in {

    val heap = Heap(3, 1, 2, 4, 5, 6)
    val updateSpec = for {
      // update 3 => 7
      _ <- update(3, 7)
      _ <- size.map(s => assert(s === 6))
      _ <- top[Int].map(v => assert(v === 7))
      _ <- contains(3).map(c => assert(c === false))
      _ <- contains(7).map(c => assert(c === true))
      // update 2 => 0
      _ <- update(2, 0)
      _ <- size.map(s => assert(s === 6))
      _ <- top[Int].map(v => assert(v === 7))
      _ <- contains(2).map(c => assert(c === false))
      _ <- contains(0).map(c => assert(c === true))
      // update 1 => 6
      // Heap will shrink if the new element is already in the heap.
      _ <- update(1, 6)
      _ <- size.map(s => assert(s === 5))
      _ <- top[Int].map(v => assert(v === 7))
      _ <- contains(1).map(c => assert(c === false))
      _ <- contains(6).map(c => assert(c === true))
      // update 10 => 8
      // Heap will grow if the old element is not in the heap.
      _ <- update(10, 8)
      _ <- size.map(s => assert(s === 6))
      _ <- top[Int].map(v => assert(v === 8))
      _ <- contains(10).map(c => assert(c === false))
      _ <- contains(8).map(c => assert(c === true))
      // update 10 => 5
      // Heap will change if the old element is not in the heap and the new element is already in the heap.
      _ <- update(10, 5)
      _ <- size.map(s => assert(s === 6))
      _ <- top[Int].map(v => assert(v === 8))
      _ <- contains(10).map(c => assert(c === false))
      _ <- contains(5).map(c => assert(c === true))
      // update 4 => 4
      // Heap will not change if the old and new elements are the same.
      _ <- update(4, 4)
      _ <- size.map(s => assert(s === 6))
      _ <- top[Int].map(v => assert(v === 8))
      _ <- contains(4).map(c => assert(c === true))
    } yield ()
    updateSpec.run(heap).value

  }

  "clear non-empty heap" in {

    val heap = Heap(1, 2, 3, 4, 5)
    val clearSpec = for {
      _ <- isEmpty[Int].map(c => assert(c === false))
      _ <- clear
      _ <- isEmpty.map(c => assert(c === true))
    } yield ()
    clearSpec.run(heap).value

  }

  "clear empty heap" in {

    val heap = Heap.empty[Int]
    val clearSpec = for {
      _ <- clear[Int]
      _ <- isEmpty.map(c => assert(c === true))
    } yield ()
    clearSpec.run(heap).value

  }

  "reverse" in {

    val heap = Heap(1, 2, 3, 4, 5)
    val reverseSpec = for {
      _ <- reverse
      _ <- popAll[Int].map(xs => assert(xs === Seq(1, 2, 3, 4, 5)))
    } yield ()
    reverseSpec.run(heap).value

  }

  "iterator" in {

    val heap = Heap(1, 2, 3, 4, 5)
    val reverseSpec = for {
      forwardIterator <- iterator[Int]
      _ <- reverse
      reverseIterator <- iterator
    } yield (forwardIterator, reverseIterator)

    val (forwardIterator, reverseIterator) = reverseSpec.runA(heap).value
    assert(forwardIterator.toSeq === Seq(5, 4, 3, 2, 1))
    assert(reverseIterator.toSeq === Seq(1, 2, 3, 4, 5))
    popAll.runA(heap).value === Seq(5, 4, 3, 2, 1)

  }

  "to" in {

    val heap = Heap(1, 2, 3, 4, 5)
    assert(heap.to(Seq) === Seq(5, 4, 3, 2, 1))

  }

  "randomized|push,top,pop,contains" in {

    val seed = System.nanoTime()
    withClue(seed) {
      val random = new Random(seed)
      val heap = Heap.empty[Int]
      val xs = random.shuffle(IndexedSeq.from(1 to 10_000))
      val expected = xs.scan(0)(math.max).drop(1)

      val pushSpec = xs.indices
        .map { i =>
          for {
            _ <- push(xs(i))
            _ <- top[Int].map(v => assert(v === expected(i)))
            _ <- contains(xs(i)).map(c => assert(c === true))
          } yield ()
        }
        .toList
        .sequence_
      val popSpec = (10_000 to 1 by -1)
        .map { n =>
          for {
            _ <- top[Int].map(v => assert(v === n))
            _ <- pop[Int].map(v => assert(v === n))
            _ <- contains(n).map(c => assert(c === false))
          } yield ()
        }
        .toList
        .sequence_
      val spec = for {
        _ <- pushSpec
        _ <- popSpec
      } yield ()
      spec.run(heap).value

    }

  }

  "randomized|remove" in {

    val seed = System.nanoTime()
    withClue(seed) {
      val random = new Random(seed)
      val xs = random.shuffle(IndexedSeq.from(1 to 10_000))
      val heap = Heap.from(xs)
      val spec = xs
        .map { x =>
          for {
            _ <- contains(x).map(c => assert(c === true))
            sz <- size
            _ <- remove(x)
            _ <- contains(x).map(c => assert(c === false))
            _ <- size.map(s => assert(s === sz - 1))
          } yield ()
        }
        .toList
        .sequence_
      spec.run(heap).value
    }

  }

  "randomized|update" in {

    val seed = System.nanoTime()
    withClue(seed) {
      val random = new Random(seed)
      val xs = random.shuffle(IndexedSeq.from(1 to 10_000))
      val heap = Heap.from(xs)

      def updateSpec(
          oldValue: Int,
          newValue: Int
      ): IndexedStateT[Eval, Heap[Int], Heap[Int], Unit] = for {
        _ <- contains(oldValue).map(c => assert(c === true))
        containsNewValue <- contains(newValue)
        _ <-
          if (containsNewValue) {
            updateSpecIfNewValueExists(oldValue, newValue)
          } else {
            updateSpecIfNewValueDoesNotExist(oldValue, newValue)
          }
      } yield ()

      def updateSpecIfNewValueExists(
          oldValue: Int,
          newValue: Int
      ): IndexedStateT[Eval, Heap[Int], Heap[Int], Unit] = for {
        sz <- size
        _ <- update(oldValue, newValue)
        _ <- contains(oldValue).map(c => assert(c === false))
        _ <- contains(newValue).map(c => assert(c === true))
        _ <- size.map(s => assert(s === sz - 1))
      } yield ()

      def updateSpecIfNewValueDoesNotExist(
          oldValue: Int,
          newValue: Int
      ): IndexedStateT[Eval, Heap[Int], Heap[Int], Unit] = for {
        sz <- size
        _ <- update(oldValue, newValue)
        _ <- contains(oldValue).map(c => assert(c === false))
        _ <- contains(newValue).map(c => assert(c === true))
        _ <- size.map(s => assert(s === sz))
      } yield ()

      val spec = xs
        .map { oldValue =>
          val newValue = oldValue + 100
          updateSpec(oldValue, newValue)
        }
        .toList
        .sequence_
      spec.run(heap).value

    }

  }

  "advanced| hashCode doesn't contain priority" in {

    final class Item(val value: String, val priority: Int) {
      override def hashCode(): Int = value.hashCode()
      override def equals(obj: Any): Boolean = obj match {
        case that: Item => this.value == that.value
        case _          => false
      }
    }
    object Item {
      implicit val ordering: Ordering[Item] = Ordering.by[Item, Int](_.priority)
      def apply(value: String, priority: Int): Item = new Item(value, priority)
    }

    val heap = Heap(
      Item("A", 1),
      Item("B", 2),
      Item("C", 4),
      Item("D", 4)
    )(Item.ordering)

    val spec = for {
      _ <- size.map(s => assert(s === 4))
      _ <- top[Item].map(v => assert(v === Item("C", 4)))
      // Update A's priority to 5.
      _ <- push(Item("A", 5))
      _ <- top[Item].map(v => assert(v === Item("A", 5)))
      // Update A's priority to 2.
      _ <- push(Item("A", 2))
      _ <- top[Item].map(v => assert(v === Item("C", 4)))
      // Update A's priority to 5.
      _ <- push(Item("A", 5))
      _ <- top[Item].map(v => assert(v === Item("A", 5)))
      // Update A's priority to 2.
      _ <- update(Item("A", 5), Item("A", 2))
      _ <- top[Item].map(v => assert(v === Item("C", 4)))
      // Remove B and Update A's priority to 5.
      _ <- contains(Item("B", 2)).map(c => assert(c === true))
      _ <- update(Item("B", 2), Item("A", 5))
      _ <- contains(Item("B", 2)).map(c => assert(c === false))
      _ <- top[Item].map(v => assert(v === Item("A", 5)))
      // Remove D and Update A's priority to 2.
      _ <- contains(Item("D", 3)).map(c => assert(c === true))
      _ <- update(Item("D", 3), Item("A", 2))
      _ <- contains(Item("D", 3)).map(c => assert(c === false))
      _ <- top[Item].map(v => assert(v === Item("C", 4)))
    } yield ()

    spec.run(heap).value

  }

}
