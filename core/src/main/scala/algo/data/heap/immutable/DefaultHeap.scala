package algo.data.heap.immutable

import scala.collection.{AbstractIterator, Factory}
import scala.collection.immutable.{HashMap, HashSet}

private final class DefaultHeap[V: Ordering] private (
    val branchingFactor: Int,
    // buffer must not have the same element.
    // buffer must be heapified.
    private val buffer: Vector[V],
    private val indexOfElement: HashMap[V, Int]
) extends Heap[V] {
  require(branchingFactor > 1, "Branching factor must be greater than 1.")

  override def size: Int = buffer.size

  override def isEmpty: Boolean = buffer.isEmpty

  override def nonEmpty: Boolean = buffer.nonEmpty

  override def ordering: Ordering[V] = Ordering[V]

  override def top: V = {
    throwIfEmpty()
    buffer.head
  }

  override def topOption: Option[V] = {
    buffer.headOption
  }

  override def pop(): (V, Heap[V]) = {
    throwIfEmpty()
    val (newBuffer, newIndexOfElement, element) =
      ops.remove(buffer, indexOfElement, 0)
    (element, newHeap(newBuffer, newIndexOfElement))
  }

  override def popAll(): (IndexedSeq[V], Heap[V]) = {
    val builder = IndexedSeq.newBuilder[V]
    builder.addAll(iterator)
    (builder.result(), newHeap(Vector.empty, HashMap.empty))
  }

  override def push(value: V): Heap[V] = {
    indexOfElement.get(value) match {
      case Some(index) =>
        val (newBuffer, newIndexOfElement) =
          ops.update(buffer, indexOfElement, index, value)
        newHeap(newBuffer, newIndexOfElement)
      case None =>
        val buf = buffer :+ value
        val ioe = indexOfElement + (value -> (buf.size - 1))
        val (newBuffer, newIndexOfElement) =
          ops.bubbleUp(buf, ioe, buf.size - 1)
        newHeap(newBuffer, newIndexOfElement)
    }
  }

  override def contains(value: V): Boolean = {
    indexOfElement.contains(value)
  }

  override def remove(value: V): Heap[V] = {
    indexOfElement.get(value) match {
      case Some(index) =>
        val (newBuffer, newIndexOfElement, _) =
          ops.remove(buffer, indexOfElement, index)
        newHeap(newBuffer, newIndexOfElement)
      case None =>
        // Do nothing if the value does not exists.
        this
    }
  }

  override def update(oldValue: V, newValue: V): Heap[V] = {
    if (oldValue.hashCode() == newValue.hashCode()) {
      // The old and new values have the same hash code.
      // Insert the new value again, which might update its priority.
      push(newValue)
    } else {
      indexOfElement.get(oldValue) match {
        case Some(oldValueIndex) =>
          if (indexOfElement.contains(newValue)) {
            // Both the old and new values exist.
            // Insert the new value again, which might update its priority.
            val (buf, ioe, _) =
              ops.remove(buffer, indexOfElement, oldValueIndex)
            val (newBuffer, newIndexOfElement) =
              ops.update(buf, ioe, ioe(newValue), newValue)
            newHeap(newBuffer, newIndexOfElement)
          } else {
            // The old value exists; The new value does not exist.
            val (newBuffer, newIndexOfElement) =
              ops.update(buffer, indexOfElement, oldValueIndex, newValue)
            newHeap(newBuffer, newIndexOfElement)
          }
        case None =>
          // The old value does not exist; The new value might exist.
          // Insert the new value again, which might update its priority.
          push(newValue)
      }
    }
  }

  override def clear(): Heap[V] = {
    newHeap(Vector.empty, HashMap.empty)
  }

  override def reverse: Heap[V] = {
    val reverseOrdering = ordering.reverse
    val (newBuffer, newIndexOfElement) =
      ops.heapify(buffer, indexOfElement)(reverseOrdering)
    newHeap(newBuffer, newIndexOfElement)(reverseOrdering)
  }

  override def iterator: Iterator[V] =
    new AbstractIterator[V] {
      private var self: Heap[V] = DefaultHeap.this
      override def knownSize: Int = self.size
      override def hasNext: Boolean = self.nonEmpty
      override def next(): V = {
        val (value, newSelf) = self.pop()
        self = newSelf
        value
      }
    }

  override def knownSize: Int = size

  override def to[C](factory: Factory[V, C]): C = {
    factory.fromSpecific(iterator)
  }

  @inline private def throwIfEmpty(): Unit = {
    if (size == 0) throw new NoSuchElementException("The heap is empty.")
  }

  @inline private def ops: DefaultHeap.Ops =
    new DefaultHeap.Ops(branchingFactor)

  @inline private def newHeap(
      newBuffer: Vector[V],
      newIndexOfElement: HashMap[V, Int]
  )(implicit ordering: Ordering[V]): Heap[V] = {
    new DefaultHeap(branchingFactor, newBuffer, newIndexOfElement)(ordering)
  }

}

private object DefaultHeap {

  /** Create a [[DefaultHeap]] containing elements of the given iterable and
    * with the given branching factor
    *
    * @throws IllegalArgumentException
    *   if the branching factor is not greater than 1
    *
    * @note
    *   Time complexity: O(n)
    */
  def from[V: Ordering](
      branchingFactor: Int,
      iterable: IterableOnce[V]
  ): DefaultHeap[V] = {
    require(branchingFactor > 1, "Branching factor must be greater than 1.")
    val buf = iterable.iterator.to(HashSet).toVector
    val ioe = buf.zipWithIndex.to(HashMap)
    val (buffer, indexOfElement) = new Ops(branchingFactor).heapify(buf, ioe)
    new DefaultHeap(branchingFactor, buffer, indexOfElement)
  }

  private final class Ops(val branchingFactor: Int) extends AnyVal {

    @inline private def getParentIndex(index: Int): Int = {
      (index - 1) / branchingFactor
    }

    @inline private def getChildIndices(index: Int): Range = {
      (branchingFactor * index + 1) to (branchingFactor * index + branchingFactor)
    }

    @inline private def getFirstLeafIndex[V](buffer: Vector[V]): Int = {
      if (buffer.size == 1) 0 else getParentIndex(buffer.size - 1) + 1
    }

    @inline private def getHighestOrderChildIndex[V: Ordering](
        buffer: Vector[V],
        index: Int
    ): Int = {
      getChildIndices(index)
        .filter(_ < buffer.size)
        .maxBy(buffer(_))
    }

    @inline def bubbleUp[V: Ordering](
        buffer: Vector[V],
        indexOfElement: HashMap[V, Int],
        index: Int
    ): (Vector[V], HashMap[V, Int]) = {
      import Ordering.Implicits.*
      val targetValue = buffer(index)
      var newBuffer = buffer
      var newIndexOfElement = indexOfElement
      var currentIndex = index
      var continue = true
      while (currentIndex > 0 && continue) {
        val parentIndex = getParentIndex(currentIndex)
        if (newBuffer(parentIndex) < targetValue) {
          newBuffer = newBuffer.updated(currentIndex, newBuffer(parentIndex))
          newIndexOfElement += (newBuffer(currentIndex) -> currentIndex)
          currentIndex = parentIndex
        } else {
          continue = false
        }
      }
      newBuffer = newBuffer.updated(currentIndex, targetValue)
      newIndexOfElement += (newBuffer(currentIndex) -> currentIndex)
      (newBuffer, newIndexOfElement)
    }

    @inline def pushDown[V: Ordering](
        buffer: Vector[V],
        indexOfElement: HashMap[V, Int],
        index: Int
    ): (Vector[V], HashMap[V, Int]) = {
      import Ordering.Implicits.*
      val targetValue = buffer(index)
      val firstLeafIndex = getFirstLeafIndex(buffer)
      var newBuffer = buffer
      var newIndexOfElement = indexOfElement
      var currentIndex = index
      var continue = true
      while (currentIndex < firstLeafIndex && continue) {
        val highestOrderChildIndex =
          getHighestOrderChildIndex(newBuffer, currentIndex)
        if (newBuffer(highestOrderChildIndex) > targetValue) {
          newBuffer =
            newBuffer.updated(currentIndex, newBuffer(highestOrderChildIndex))
          newIndexOfElement += (newBuffer(currentIndex) -> currentIndex)
          currentIndex = highestOrderChildIndex
        } else {
          continue = false
        }
      }
      newBuffer = newBuffer.updated(currentIndex, targetValue)
      newIndexOfElement += (newBuffer(currentIndex) -> currentIndex)
      (newBuffer, newIndexOfElement)
    }

    @inline def remove[V: Ordering](
        buffer: Vector[V],
        indexOfElement: HashMap[V, Int],
        index: Int
    ): (Vector[V], HashMap[V, Int], V) = {
      val lastValue = buffer.last
      var newBuffer = buffer.dropRight(1)
      var newIndexOfElement = indexOfElement - lastValue
      if (index == newBuffer.size) {
        (newBuffer, newIndexOfElement, lastValue)
      } else {
        val value = newBuffer(index)
        newBuffer = newBuffer.updated(index, lastValue)
        newIndexOfElement -= value
        newIndexOfElement += (lastValue -> index)
        val (buf, ioe) = pushDown(newBuffer, newIndexOfElement, index)
        newBuffer = buf
        newIndexOfElement = ioe
        (newBuffer, newIndexOfElement, value)
      }
    }

    @inline def update[V: Ordering](
        buffer: Vector[V],
        indexOfElement: HashMap[V, Int],
        index: Int,
        value: V
    ): (Vector[V], HashMap[V, Int]) = {
      import Ordering.Implicits.*
      val existingValue = buffer(index)
      val newBuffer = buffer.updated(index, value)
      val newIndexOfElement = indexOfElement - existingValue + (value -> index)
      if (value > existingValue) {
        bubbleUp(newBuffer, newIndexOfElement, index)
      } else if (value < existingValue) {
        pushDown(newBuffer, newIndexOfElement, index)
      } else {
        (newBuffer, newIndexOfElement)
      }
    }

    @inline def heapify[V: Ordering](
        buffer: Vector[V],
        indexOfElement: HashMap[V, Int]
    ): (Vector[V], HashMap[V, Int]) = {
      if (buffer.isEmpty) {
        (buffer, indexOfElement)
      } else {
        var newBuffer = buffer
        var newIndexOfElement = indexOfElement
        ((buffer.size - 1) / branchingFactor to 0 by -1).foreach { index =>
          val (buf, ioe) = pushDown(newBuffer, newIndexOfElement, index)
          newBuffer = buf
          newIndexOfElement = ioe
        }
        (newBuffer, newIndexOfElement)
      }
    }

  }

}
