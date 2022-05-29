package algo.data.heap.immutable

import cats.data.State

trait HeapSyntax {

  /** @see [[Heap.size]] */
  def size[V]: State[Heap[V], Int] = State.inspect { s =>
    s.size
  }

  /** @see [[Heap.isEmpty]] */
  def isEmpty[V]: State[Heap[V], Boolean] = State.inspect { s =>
    s.isEmpty
  }

  /** @see [[Heap.nonEmpty]] */
  def nonEmpty[V]: State[Heap[V], Boolean] = State.inspect { s =>
    s.nonEmpty
  }

  /** @see [[Heap.ordering]] */
  def ordering[V]: State[Heap[V], Ordering[V]] = State.inspect { s =>
    s.ordering
  }

  /** @see [[Heap.branchingFactor]] */
  def branchingFactor[V]: State[Heap[V], Int] = State.inspect { s =>
    s.branchingFactor
  }

  /** @see [[Heap.top]] */
  def top[V]: State[Heap[V], V] = State.inspect { s =>
    s.top
  }

  /** @see [[Heap.topOption]] */
  def topOption[V]: State[Heap[V], Option[V]] = State.inspect { s =>
    s.topOption
  }

  /** @see [[Heap.pop]] */
  def pop[V]: State[Heap[V], V] = State { s =>
    s.pop().swap
  }

  /** @see [[Heap.popAll]] */
  def popAll[V]: State[Heap[V], IndexedSeq[V]] = State { s =>
    s.popAll().swap
  }

  /** @see [[Heap.push]] */
  def push[V](value: V): State[Heap[V], Unit] = State.modify { s =>
    s.push(value)
  }

  /** @see [[Heap.contains]] */
  def contains[V](value: V): State[Heap[V], Boolean] = State.inspect { s =>
    s.contains(value)
  }

  /** @see [[Heap.remove]] */
  def remove[V](value: V): State[Heap[V], Unit] = State.modify { s =>
    s.remove(value)
  }

  /** @see [[Heap.update]] */
  def update[V](oldValue: V, newValue: V): State[Heap[V], Unit] = State.modify {
    s =>
      s.update(oldValue, newValue)
  }

  /** @see [[Heap.clear]] */
  def clear[V]: State[Heap[V], Unit] = State.modify { s =>
    s.clear()
  }

  /** @see [[Heap.reverse]] */
  def reverse[V]: State[Heap[V], Unit] = State.modify { s =>
    s.reverse
  }

  /** @see [[Heap.iterator]] */
  def iterator[V]: State[Heap[V], Iterator[V]] = State.inspect { s =>
    s.iterator
  }

  /** @see [[Heap.knownSize]] */
  def knownSize[V]: State[Heap[V], Int] = State.inspect { s =>
    s.knownSize
  }

}
