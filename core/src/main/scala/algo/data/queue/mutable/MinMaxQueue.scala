package algo.data.queue.mutable

import scala.collection.{Factory, mutable}

trait MinMaxQueue[A] {

  /** The size of this queue
    * @note Time Complexity: O(1)
    */
  def size: Int

  /** Add the element to the last of this queue
    * @note Time Complexity: O(1)
    */
  def enqueue(value: A): this.type

  /** Enqueue the elements to this queue */
  def enqueue(value: A, values: A*): this.type

  /** Enqueue all elements in the given iterable to this queue */
  def enqueueAll(values: IterableOnce[A]): this.type

  /** Remove the first element from this queue and return the element
    * @note Time Complexity: amortized O(1)
    */
  @throws(classOf[NoSuchElementException])
  def dequeue(): A

  /** Dequeue all elements from this queue and return the elements */
  def dequeueAll(): Seq[A]

  /** Dequeue and return all elements from this stack which satisfies the given predicate */
  def dequeueWhile(f: A => Boolean): Seq[A]

  /** Return the first element of this queue
    * @note Time Complexity: O(1)
    */
  @throws(classOf[NoSuchElementException])
  def front: A

  /** Return the first element of this queue
    * @note Time Complexity: O(1)
    */
  def frontOption: Option[A]

  /** Return the last element of this queue
    * @note Time Complexity: O(1)
    */
  @throws(classOf[NoSuchElementException])
  def back: A

  /** Return the last element of this queue
    * @note Time Complexity: O(1)
    */
  def backOption: Option[A]

  /** Get the element at the specified index
    * @note Time Complexity: O(1)
    */
  @throws(classOf[IndexOutOfBoundsException])
  def apply(index: Int): A

  /** Remove the all elements of this queue
    * @note Time Complexity: O(N)
    */
  def clear(): Unit

  /** Return the ordering of this queue
    * @note Time Complexity: O(1)
    */
  def ordering: Ordering[A]

  /** Return the minimum element of this queue
    * @note Time Complexity: O(1)
    */
  @throws(classOf[UnsupportedOperationException])
  def min: A

  /** Return the minimum element of this queue
    * @note Time Complexity: O(1)
    */
  def minOption: Option[A]

  /** Return the maximum element of this queue
    * @note Time Complexity: O(1)
    */
  @throws(classOf[UnsupportedOperationException])
  def max: A

  /** Return the maximum element of this queue
    * @note Time Complexity: O(1)
    */
  def maxOption: Option[A]

  /** Return the minimum and maximum elements of this queue
    * @note Time Complexity: O(1)
    */
  @throws(classOf[UnsupportedOperationException])
  def minmax: (A, A)

  /** Return the minimum and maximum elements of this queue
    * @note Time Complexity: O(1)
    */
  def minmaxOption: Option[(A, A)]

  /** Return true if this queue is empty
    * @note Time Complexity: O(1)
    */
  def isEmpty: Boolean

  /** Return true if this queue is not empty
    * @note Time Complexity: O(1)
    */
  def nonEmpty: Boolean

  /** Return an iterator of this queue
    * @note Time Complexity: O(1)
    */
  def iterator: Iterator[A]

  /** Return a reverse iterator of this queue
    * @note Time Complexity: O(1)
    */
  def reverseIterator: Iterator[A]

  /** Build a collection from this queue */
  def to[C](factory: Factory[A, C]): C

}

object MinMaxQueue {

  /** An empty queue
    * @note Time Complexity: O(1)
    */
  def empty[A: Ordering]: MinMaxQueue[A] =
    new DefaultMinMaxQueue[A]()

  /** Create a queue from the given collection
    * @note Time Complexity: O(N)
    */
  def from[A: Ordering](iterable: IterableOnce[A]): MinMaxQueue[A] = {
    val builder = newBuilder
    builder ++= iterable
    builder.result()
  }

  /** Create a queue from the given elements
    * @note Time Complexity: O(N)
    */
  def apply[A: Ordering](elems: A*): MinMaxQueue[A] =
    from(elems)

  /** Create a builder for [[MinMaxQueue]] */
  def newBuilder[A: Ordering]: mutable.Builder[A, MinMaxQueue[A]] =
    new mutable.Builder[A, MinMaxQueue[A]]() {
      val buffer: mutable.ArrayBuffer[A] = mutable.ArrayBuffer.empty
      override def clear(): Unit = buffer.clear()
      override def result(): MinMaxQueue[A] = {
        val queue = MinMaxQueue.empty
        buffer.iterator.foreach(queue.enqueue)
        queue
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
