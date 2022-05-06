package algo.data.heap.mutable

import algo.testing.BaseSpec

import scala.util.Random

final class HeapSpec extends BaseSpec {

  "Factory|empty" in {

    val heap = Heap.empty[Int]
    assert(heap.size === 0)

  }

  "Factory|from" in {

    val heap = Heap.from(Seq(1, 5, 3, 2, 4))
    assert(heap.size === 5)
    assert(heap.popAll() === Seq(5, 4, 3, 2, 1))

  }

  "Factory|apply" in {

    val heap = Heap(1, 5, 3, 2, 4)
    assert(heap.size === 5)
    assert(heap.popAll() === Seq(5, 4, 3, 2, 1))

  }

  "Factory|fill" in {

    val xs = IndexedSeq(1, 5, 3, 2, 4)
    val heap = {
      val it = xs.iterator
      Heap.fill(5) { it.next() }
    }
    assert(heap.size === 5)
    assert(heap.popAll() === Seq(5, 4, 3, 2, 1))

  }

  "Factory|tabulate" in {

    val heap = Heap.tabulate(5)(n => n + 1)
    assert(heap.size === 5)
    assert(heap.popAll() === Seq(5, 4, 3, 2, 1))

  }

  "Factory|iterate" in {

    val heap = Heap.iterate(1, 5)(n => n + 1)
    assert(heap.size === 5)
    assert(heap.popAll() === Seq(5, 4, 3, 2, 1))

  }

  "Factory|unfold" in {

    val heap = Heap.unfold(1) {
      case n if n <= 5 => Some((n, n + 1))
      case _           => None
    }
    assert(heap.size === 5)
    assert(heap.popAll() === Seq(5, 4, 3, 2, 1))

  }

  "Factory|withBranchingFactor" in {

    val heap = Heap.withBranchingFactor(2)(3, 5, 2, 4, 1)
    assert(heap.branchingFactor === 2)
    assert(heap.popAll() === Seq(5, 4, 3, 2, 1))

  }

  "Factory|withBranchingFactor(1)" in {

    intercept[IllegalArgumentException] {
      Heap.withBranchingFactor(1)
    }

  }

  "Factory|to" in {

    val defaultHeap = Seq(3, 5, 2, 4, 1).to(Heap)
    assert(defaultHeap.branchingFactor === 4)
    assert(defaultHeap.popAll() === Seq(5, 4, 3, 2, 1))

    val customHeap = Seq(3, 5, 2, 4, 1).to(Heap.withBranchingFactor(2))
    assert(customHeap.branchingFactor === 2)
    assert(customHeap.popAll() === Seq(5, 4, 3, 2, 1))

  }

  "Factory|newBuilder" in {

    val builder = Heap.newBuilder[Int]
    builder.addOne(3)
    builder.clear()
    builder.addOne(1)
    builder.addAll(Seq(5, 2, 4))

    val heap = builder.result()
    assert(heap.branchingFactor === 4)
    assert(heap.size === 4)
    assert(heap.popAll() === Seq(5, 4, 2, 1))

  }

  "size" in {

    for (sz <- 0 until 100) {
      val heap = Heap.from(Seq.tabulate(sz)(identity))
      assert(heap.size === sz)
    }

  }

  "isEmpty" in {

    val emptyHeap = Heap.empty[Int]
    assert(emptyHeap.size === 0)
    assert(emptyHeap.isEmpty === true)

    val nonEmptyHeap = Heap(1)
    assert(nonEmptyHeap.size === 1)
    assert(nonEmptyHeap.isEmpty === false)

  }

  "nonEmpty" in {

    val emptyHeap = Heap.empty[Int]
    assert(emptyHeap.size === 0)
    assert(emptyHeap.nonEmpty === false)

    val nonEmptyHeap = Heap(1)
    assert(nonEmptyHeap.size === 1)
    assert(nonEmptyHeap.nonEmpty === true)

  }

  "ordering" in {

    val heap = Heap.empty[Int](Ordering.Int)
    assert(heap.ordering === Ordering.Int)

    val reverseOrdering = Ordering.Int.reverse
    val reverseHeap = Heap.empty[Int](reverseOrdering)
    assert(reverseHeap.ordering === reverseOrdering)

  }

  "branchingFactor" in {

    val defaultHeap = Heap.empty[Int]
    assert(defaultHeap.branchingFactor === 4)

    val customHeap = Heap.withBranchingFactor(2).empty[Int]
    assert(customHeap.branchingFactor === 2)

  }

  "top" in {

    val heap = Heap.empty[Int]
    val xs = IndexedSeq(1, 3, 5, 2, 4, 6, 8, 9, 7, 10)
    val expected = IndexedSeq(1, 3, 5, 5, 5, 6, 8, 9, 9, 10)

    for (i <- xs.indices) {
      heap.push(xs(i))
      assert(heap.top === expected(i))
    }

  }

  "top from an empty heap" in {

    intercept[NoSuchElementException] {
      Heap.empty[Int].top
    }

  }

  "topOption" in {

    val heap = Heap.empty[Int]
    val xs = IndexedSeq(1, 3, 5, 2, 4, 6, 8, 9, 7, 10)
    val expected = IndexedSeq(1, 3, 5, 5, 5, 6, 8, 9, 9, 10)

    for (i <- xs.indices) {
      heap.push(xs(i))
      assert(heap.topOption === Some(expected(i)))
    }

  }

  "topOption from an empty heap" in {

    assert(Heap.empty[Int].topOption === None)

  }

  "pop" in {

    val heap = Heap(1, 5, 3, 2, 4)
    for (n <- 5 to 1 by -1) {
      assert(heap.pop() === n)
    }

  }

  "pop from an empty heap" in {

    intercept[NoSuchElementException] {
      Heap.empty[Int].pop()
    }

  }

  "popAll from non-empty heap" in {

    val heap = Heap(1, 5, 3, 2, 4)
    assert(heap.popAll() === Seq(5, 4, 3, 2, 1))
    assert(heap.size === 0)

  }

  "popAll from empty heap" in {

    val heap = Heap.empty[Int]
    assert(heap.popAll() === Seq.empty)

  }

  "push" in {

    val heap = Heap.empty[Int]

    heap.push(1)
    assert(heap.size === 1)
    assert(heap.top === 1)
    assert(heap.contains(1))

    heap.push(5)
    assert(heap.size === 2)
    assert(heap.top === 5)
    assert(heap.contains(5))

    heap.push(3)
    assert(heap.size === 3)
    assert(heap.top === 5)
    assert(heap.contains(3))

    // Heap will not grow if the element is already in the heap.
    heap.push(1)
    assert(heap.size === 3)
    assert(heap.top === 5)
    assert(heap.contains(1))

  }

  "contains" in {

    val heap = Heap(1, 2, 3, 4, 5)
    assert(heap.contains(1) === true)
    assert(heap.contains(2) === true)
    assert(heap.contains(3) === true)
    assert(heap.contains(4) === true)
    assert(heap.contains(5) === true)

    assert(heap.contains(0) === false)
    assert(heap.contains(6) === false)

  }

  "remove" in {

    val heap = Heap(3, 1, 2, 4, 5, 6)

    heap.remove(3)
    assert(heap.size === 5)
    assert(heap.top === 6)
    assert(heap.contains(3) === false)

    heap.remove(5)
    assert(heap.size === 4)
    assert(heap.top === 6)
    assert(heap.contains(5) === false)

    // Heap will not change if the element is not in the heap.
    heap.remove(0)
    assert(heap.size === 4)
    assert(heap.top === 6)
    assert(heap.contains(0) === false)

    assert(heap.popAll() === Seq(6, 4, 2, 1))

  }

  "update" in {

    val heap = Heap(3, 1, 2, 4, 5, 6)

    heap.update(3, 7)
    assert(heap.size === 6)
    assert(heap.top === 7)
    assert(heap.contains(3) === false)
    assert(heap.contains(7) === true)

    heap.update(2, 0)
    assert(heap.size === 6)
    assert(heap.top === 7)
    assert(heap.contains(2) === false)
    assert(heap.contains(0) === true)

    // Heap will shrink if the new element is already in the heap.
    heap.update(1, 6)
    assert(heap.size === 5)
    assert(heap.top === 7)
    assert(heap.contains(1) === false)
    assert(heap.contains(6) === true)

    // Heap will grow if the old element is not in the heap.
    heap.update(10, 8)
    assert(heap.size === 6)
    assert(heap.top === 8)
    assert(heap.contains(10) === false)
    assert(heap.contains(8) === true)

    // Heap will change if the old element is not in the heap and the new element is already in the heap.
    heap.update(10, 5)
    assert(heap.size === 6)
    assert(heap.top === 8)
    assert(heap.contains(10) === false)
    assert(heap.contains(5) === true)

    // Heap will not change if the old and new elements are the same.
    heap.update(4, 4)
    assert(heap.size === 6)
    assert(heap.top === 8)
    assert(heap.contains(4) === true)

    assert(heap.popAll() === Seq(8, 7, 6, 5, 4, 0))

  }

  "clear non-empty heap" in {

    val heap = Heap(1, 2, 3, 4, 5)
    assert(heap.isEmpty === false)
    heap.clear()
    assert(heap.isEmpty === true)

  }

  "clear empty heap" in {

    val heap = Heap.empty[Int]
    heap.clear()
    assert(heap.isEmpty === true)

  }

  "reversed" in {

    val heap = Heap(1, 2, 3, 4, 5)

    val reversedHeap = heap.reversed
    for (n <- 1 to 5) {
      assert(reversedHeap.pop() === n)
    }

    for (n <- 5 to 1 by -1) {
      assert(heap.pop() === n)
    }

  }

  "clone" in {

    val heap = Heap(1, 2, 3, 4, 5)
    val clonedHeap = heap.clone

    // Operations of the original heap should not affect the cloned heap.
    for (n <- 5 to 1 by -1) {
      assert(heap.pop() === n)
    }

    // Operations of the cloned heap should not affect the original heap.
    for (n <- 5 to 1 by -1) {
      assert(clonedHeap.pop() === n)
    }

  }

  "randomized|push,top,pop,contains" in {

    val seed = System.nanoTime()
    withClue(seed) {
      val random = new Random(seed)
      val heap = Heap.empty[Int]
      val xs = random.shuffle(IndexedSeq.from(1 to 10_000))
      val expected = xs.scan(0)(math.max).drop(1)

      // grow
      for (i <- xs.indices) {
        heap.push(xs(i))
        assert(heap.top === expected(i))
        assert(heap.contains(xs(i)) === true)
      }

      // shrink
      for (n <- 10_000 to 1 by -1) {
        assert(heap.top === n)
        assert(heap.pop() === n)
        assert(heap.contains(n) === false)
      }

    }

  }

  "randomized|remove" in {

    val seed = System.nanoTime()
    withClue(seed) {
      val random = new Random(seed)
      val xs = random.shuffle(IndexedSeq.from(1 to 10_000))
      val heap = Heap.from(xs)
      for (x <- xs) {
        assert(heap.contains(x) === true)
        val size = heap.size
        heap.remove(x)
        assert(heap.contains(x) === false)
        assert(heap.size === size - 1)
      }
    }

  }

  "randomized|update" in {

    val seed = System.nanoTime()
    withClue(seed) {
      val random = new Random(seed)
      val xs = random.shuffle(IndexedSeq.from(1 to 10_000))
      val heap = Heap.from(xs)
      for (oldValue <- xs) {
        val newValue = oldValue + 100
        val size = heap.size
        if (heap.contains(newValue)) {
          heap.update(oldValue, newValue)
          assert(heap.contains(oldValue) === false)
          assert(heap.contains(newValue) === true)
          assert(heap.size === size - 1)
        } else {
          heap.update(oldValue, newValue)
          assert(heap.contains(oldValue) === false)
          assert(heap.contains(newValue) === true)
          assert(heap.size === size)
        }
      }
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

    val heap = Heap.empty[Item](Item.ordering)
    heap.push(Item("A", 1))
    heap.push(Item("B", 2))
    heap.push(Item("C", 4))
    heap.push(Item("D", 3))

    assert(heap.size === 4)
    assert(heap.top === Item("C", 4))

    // Update A's priority to 5.
    heap.push(Item("A", 5))
    assert(heap.top === Item("A", 5))

    // Update A's priority to 2.
    heap.push(Item("A", 2))
    assert(heap.top === Item("C", 4))

    // Update a's priority to 5.
    heap.update(Item("A", 2), Item("A", 5))
    assert(heap.top === Item("A", 5))

    // Update A's priority to 2.
    heap.update(Item("A", 5), Item("A", 2))
    assert(heap.top === Item("C", 4))

    // Remove B and Update A's priority to 5.
    assert(heap.contains(Item("B", 2)) === true)
    heap.update(Item("B", 2), Item("A", 5))
    assert(heap.contains(Item("B", 2)) === false)
    assert(heap.top === Item("A", 5))

    // Remove D and Update A's priority to 2.
    assert(heap.contains(Item("D", 3)) === true)
    heap.update(Item("D", 3), Item("A", 2))
    assert(heap.contains(Item("D", 3)) === false)
    assert(heap.top === Item("C", 4))

  }

}
