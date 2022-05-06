package algo.data.heap.mutable

import java.util
import scala.collection.immutable.{HashMap, HashSet}

private final class DefaultHeap[V: Ordering] private (
    val branchingFactor: Int,
    // buffer must not have the same element.
    // heapify must be called manually.
    private var buffer: Array[AnyRef],
    private var count: Int
) extends Heap[V] {
  require(branchingFactor > 1, "Branching factor must be greater than 1.")

  private var indexOfElement: HashMap[AnyRef, Int] =
    buffer.view.zipWithIndex.filter(_._1 != null).to(HashMap)

  override def size: Int = count

  override def isEmpty: Boolean = count == 0

  override def nonEmpty: Boolean = count != 0

  override def ordering: Ordering[V] = Ordering[V]

  override def top: V = {
    throwIfEmpty()
    buffer(0).asInstanceOf[V]
  }

  override def topOption: Option[V] = {
    if (count > 0) {
      Some(buffer(0).asInstanceOf[V])
    } else {
      None
    }
  }

  override def pop(): V = {
    throwIfEmpty()
    remove(0)
  }

  override def popAll(): Seq[V] = {
    val builder = Seq.newBuilder[V]
    while (count > 0) {
      builder += remove(0)
    }
    builder.result()
  }

  override def push(value: V): this.type = {
    val valueAnyRef = value.asInstanceOf[AnyRef]
    indexOfElement.get(valueAnyRef) match {
      case Some(index) =>
        update(index, value)
      case None =>
        growBufferIfFull()
        buffer(count) = valueAnyRef
        count += 1
        indexOfElement += (valueAnyRef -> (count - 1))
        bubbleUp(count - 1)
    }
    this
  }

  override def contains(value: V): Boolean = {
    indexOfElement.contains(value.asInstanceOf[AnyRef])
  }

  override def remove(value: V): this.type = {
    indexOfElement.get(value.asInstanceOf[AnyRef]) match {
      case Some(index) =>
        remove(index)
        this
      case None =>
        // Do nothing if the value does not exists.
        this
    }
  }

  override def update(oldValue: V, newValue: V): this.type = {
    val oldValueAnyRef = oldValue.asInstanceOf[AnyRef]
    val newValueAnyRef = newValue.asInstanceOf[AnyRef]
    if (oldValue.hashCode() == newValue.hashCode()) {
      // The old and new values have the same hash code.
      // Insert the new value again, which might update its priority.
      push(newValue)
    } else {
      indexOfElement.get(oldValueAnyRef) match {
        case Some(index) =>
          if (indexOfElement.contains(newValueAnyRef)) {
            // Both the old and new values exist.
            // Insert the new value again, which might update its priority.
            remove(index)
            push(newValue)
          } else {
            // The old value exists; The new value does not exist.
            update(index, newValue)
          }
          this
        case None =>
          // The old value does not exist; The new value might exist.
          // Insert the new value again, which might update its priority.
          push(newValue)
      }
    }
  }

  override def clear(): this.type = {
    util.Arrays.fill(buffer, null)
    count = 0
    indexOfElement = HashMap.empty
    this
  }

  override def reversed: Heap[V] = {
    val reverseOrdering = Ordering[V].reverse
    new DefaultHeap[V](branchingFactor, buffer.clone(), count)(
      reverseOrdering
    ).heapify()
  }

  override def clone(): Heap[V] = {
    // heapify is not needed since any of parameters will not change.
    new DefaultHeap[V](branchingFactor, buffer.clone(), count)
  }

  @inline private def getParentIndex(index: Int): Int = {
    (index - 1) / branchingFactor
  }

  @inline private def getChildIndices(index: Int): Range = {
    (branchingFactor * index + 1) to (branchingFactor * index + branchingFactor)
  }

  @inline private def getFirstLeafIndex: Int = {
    if (count == 1) 0 else getParentIndex(count - 1) + 1
  }

  @inline private def getHighestOrderChildIndex(index: Int): Int = {
    getChildIndices(index).filter(_ < count).maxBy(buffer(_).asInstanceOf[V])
  }

  @inline private def bubbleUp(index: Int): Unit = {
    import Ordering.Implicits.*
    val targetValueAnyRef = buffer(index)
    val targetValue = targetValueAnyRef.asInstanceOf[V]
    var currentIndex = index
    var continue = true
    while (currentIndex > 0 && continue) {
      val parentIndex = getParentIndex(currentIndex)
      if (buffer(parentIndex).asInstanceOf[V] < targetValue) {
        buffer(currentIndex) = buffer(parentIndex)
        indexOfElement += (buffer(currentIndex) -> currentIndex)
        currentIndex = parentIndex
      } else {
        continue = false
      }
    }
    buffer(currentIndex) = targetValueAnyRef
    indexOfElement += (buffer(currentIndex) -> currentIndex)
  }

  @inline private def pushDown(index: Int): Unit = {
    import Ordering.Implicits.*
    val targetValueAnyRef = buffer(index)
    val targetValue = targetValueAnyRef.asInstanceOf[V]
    val firstLeafIndex = getFirstLeafIndex
    var currentIndex = index
    var continue = true
    while (currentIndex < firstLeafIndex && continue) {
      val highestOrderChildIndex = getHighestOrderChildIndex(currentIndex)
      if (buffer(highestOrderChildIndex).asInstanceOf[V] > targetValue) {
        buffer(currentIndex) = buffer(highestOrderChildIndex)
        indexOfElement += (buffer(currentIndex) -> currentIndex)
        currentIndex = highestOrderChildIndex
      } else {
        continue = false
      }
    }
    buffer(currentIndex) = targetValueAnyRef
    indexOfElement += (buffer(currentIndex) -> currentIndex)
  }

  @inline private def remove(index: Int): V = {
    val lastValue = buffer(count - 1)
    buffer(count - 1) = null
    indexOfElement -= lastValue
    count -= 1
    shrinkBufferIfPossible()
    if (index == count) {
      lastValue.asInstanceOf[V]
    } else {
      val value = buffer(index)
      buffer(index) = lastValue
      indexOfElement -= value
      indexOfElement += (lastValue -> index)
      pushDown(index)
      value.asInstanceOf[V]
    }
  }

  @inline private def update(index: Int, value: V): Unit = {
    import Ordering.Implicits.*
    val valueAnyRef = value.asInstanceOf[AnyRef]
    val existingValueAnyRef = buffer(index)
    val existingValue = existingValueAnyRef.asInstanceOf[V]
    buffer(index) = valueAnyRef
    indexOfElement -= existingValueAnyRef
    indexOfElement += (valueAnyRef -> index)
    if (value > existingValue) {
      bubbleUp(index)
    } else if (value < existingValue) {
      pushDown(index)
    }
  }

  @inline private def heapify(): this.type = {
    if (count > 0) {
      ((count - 1) / branchingFactor to 0 by -1).foreach { index =>
        pushDown(index)
      }
    }
    this
  }

  @inline private def throwIfEmpty(): Unit = {
    if (count == 0) throw new NoSuchElementException("The heap is empty.")
  }

  @inline private def growBufferIfFull(): Unit = {
    if (count == buffer.length) {
      buffer = Array.copyOf(buffer, buffer.length * 2)
    }
  }

  @inline private def shrinkBufferIfPossible(): Unit = {
    if (
      buffer.length >= DefaultHeap.MinimumBufferSize * 2 &&
      count * 4 <= buffer.length
    ) {
      buffer = Array.copyOf(buffer, buffer.length / 2)
    }
  }

}

private object DefaultHeap {

  private final val MinimumBufferSize = 4

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
    val (buffer, size) = bufferFrom(iterable)
    new DefaultHeap[V](branchingFactor, buffer, size).heapify()
  }

  private def bufferFrom[V](
      iterable: IterableOnce[V]
  ): (Array[AnyRef], Int) = {
    val elements = iterable.iterator.to(HashSet)
    val size = elements.size
    val buffer = new Array[AnyRef](math.max(size * 2, MinimumBufferSize))
    elements.iterator.map(_.asInstanceOf[AnyRef]).copyToArray(buffer)
    (buffer, size)
  }

}
