package algo.data.queue.immutable

import scala.collection.{Factory, mutable}

/** MinMaxQueue
  *
  * @see
  *   [[algo.data.queue.mutable.MinMaxQueue]]
  */
trait MinMaxQueue[A] extends IterableOnce[A] {

  /** Returns the size of this queue
    *
    * @note
    *   Time Complexity: O(1)
    */
  def size: Int

  /** Enqueues the element to the last of this queue
    *
    * @note
    *   Time Complexity: O(1)
    */
  def enqueue(value: A): MinMaxQueue[A]

  /** Enqueues all elements to the last of this queue */
  def enqueue(value: A, values: A*): MinMaxQueue[A]

  /** Enqueues elements of the given iterable to the last of this queue */
  def enqueueAll(values: IterableOnce[A]): MinMaxQueue[A]

  /** Dequeues and returns the first element from this queue
    *
    * @throws java.util.NoSuchElementException
    *   if this queue is empty
    *
    * @note
    *   Time Complexity: amortized O(1)
    */
  def dequeue(): (A, MinMaxQueue[A])

  /** Dequeues and returns all elements from this queue */
  def dequeueAll(): (IndexedSeq[A], MinMaxQueue[A])

  /** Dequeues and returns elements from this queue that satisfy the given
    * predicate
    */
  def dequeueWhile(f: A => Boolean): (IndexedSeq[A], MinMaxQueue[A])

  /** Returns the first element of this queue
    *
    * @throws java.util.NoSuchElementException
    *   if this queue is empty
    *
    * @note
    *   Time Complexity: O(1)
    */
  def front: A

  /** Returns the first element of this queue
    *
    * @note
    *   Time Complexity: O(1)
    */
  def frontOption: Option[A]

  /** Returns the last element of this queue
    *
    * @throws java.util.NoSuchElementException
    *   if this queue is empty
    *
    * @note
    *   Time Complexity: O(1)
    */
  def back: A

  /** Returns the last element of this queue
    *
    * @note
    *   Time Complexity: O(1)
    */
  def backOption: Option[A]

  /** Returns the element at the given index
    *
    * @throws java.lang.IndexOutOfBoundsException
    *   if the index is out of bounds
    *
    * @note
    *   Time Complexity: O(1)
    */
  def apply(index: Int): A

  /** Removes all elements from this queue
    *
    * @note
    *   Time Complexity: O(1)
    */
  def clear(): MinMaxQueue[A]

  /** Returns the ordering of this queue
    *
    * @note
    *   Time Complexity: O(1)
    */
  def ordering: Ordering[A]

  /** Returns the minimum element of this queue
    *
    * @throws java.util.NoSuchElementException
    *   if this queue is empty
    *
    * @note
    *   Time Complexity: O(1)
    */
  def min: A

  /** Returns the minimum element of this queue
    *
    * @note
    *   Time Complexity: O(1)
    */
  def minOption: Option[A]

  /** Returns the maximum element of this queue
    *
    * @throws java.util.NoSuchElementException
    *   if this queue is empty
    *
    * @note
    *   Time Complexity: O(1)
    */
  def max: A

  /** Returns the maximum element of this queue
    *
    * @note
    *   Time Complexity: O(1)
    */
  def maxOption: Option[A]

  /** Returns the minimum and maximum elements of this queue
    *
    * @throws java.util.NoSuchElementException
    *   if this queue is empty
    *
    * @note
    *   Time Complexity: O(1)
    */
  def minmax: (A, A)

  /** Returns the minimum and maximum elements of this queue
    *
    * @note
    *   Time Complexity: O(1)
    */
  def minmaxOption: Option[(A, A)]

  /** Returns true if this queue is empty
    *
    * @note
    *   Time Complexity: O(1)
    */
  def isEmpty: Boolean

  /** Returns true if this queue is not empty
    *
    * @note
    *   Time Complexity: O(1)
    */
  def nonEmpty: Boolean

  /** Returns a reverse iterator of this queue
    *
    * @note
    *   Time Complexity: O(1)
    */
  def reverseIterator: Iterator[A]

  /** Builds a collection from this queue */
  def to[C](factory: Factory[A, C]): C

}

object MinMaxQueue {

  /** Returns an empty queue
    *
    * @note
    *   Time Complexity: O(1)
    */
  def empty[A: Ordering]: MinMaxQueue[A] =
    DefaultMinMaxQueue.empty

  /** Creates a queue from the given iterable
    *
    * @note
    *   Time Complexity: O(N)
    */
  def from[A: Ordering](iterable: IterableOnce[A]): MinMaxQueue[A] = {
    val builder = newBuilder
    builder ++= iterable
    builder.result()
  }

  /** Creates a queue from the given elements
    *
    * @note
    *   Time Complexity: O(N)
    */
  def apply[A: Ordering](elems: A*): MinMaxQueue[A] =
    from(elems)

  /** Creates a builder for [[MinMaxQueue]] */
  def newBuilder[A: Ordering]: mutable.Builder[A, MinMaxQueue[A]] =
    new mutable.Builder[A, MinMaxQueue[A]]() {
      val buffer: mutable.ArrayBuffer[A] = mutable.ArrayBuffer.empty
      override def clear(): Unit = buffer.clear()
      override def result(): MinMaxQueue[A] = {
        val queue = MinMaxQueue.empty
        queue.enqueueAll(buffer.iterator)
      }
      override def addOne(elem: A): this.type = {
        buffer.addOne(elem)
        this
      }
    }

  import scala.language.implicitConversions
  implicit def toFactory[A: Ordering](
      self: this.type
  ): Factory[A, MinMaxQueue[A]] =
    new Factory[A, MinMaxQueue[A]] {
      override def fromSpecific(it: IterableOnce[A]): MinMaxQueue[A] =
        self.from(it)
      override def newBuilder: mutable.Builder[A, MinMaxQueue[A]] =
        self.newBuilder
    }

}
