package algo.data.stack.immutable

import scala.collection.{Factory, mutable}

/** MinMaxStack
  *
  * @see
  *   [[algo.data.stack.mutable.MinMaxStack]]
  */
trait MinMaxStack[A] extends IterableOnce[A] {

  /** Returns the size of this stack
    *
    * @note
    *   Time Complexity: O(1)
    */
  def size: Int

  /** Pushes the given element to the top of this stack
    *
    * @note
    *   Time Complexity: O(1)
    */
  def push(value: A): MinMaxStack[A]

  /** Pushes the given elements onto this stack */
  def push(value: A, values: A*): MinMaxStack[A]

  /** Pushes all elements of the given iterable onto this stack */
  def pushAll(iterable: IterableOnce[A]): MinMaxStack[A]

  /** Pops and returns the top element from this stack
    *
    * @throws java.util.NoSuchElementException
    *   if this stack is empty
    *
    * @note
    *   Time Complexity: O(1)
    */
  def pop(): (A, MinMaxStack[A])

  /** Pops and returns all elements from this stack
    *
    * @note
    *   Time Complexity: O(N)
    */
  def popAll(): (IndexedSeq[A], MinMaxStack[A])

  /** Pops and returns all elements from this stack that satisfy the given
    * predicate
    */
  def popWhile(f: A => Boolean): (IndexedSeq[A], MinMaxStack[A])

  /** Returns the top element of this stack
    *
    * @throws java.util.NoSuchElementException
    *   if this stack is empty
    *
    * @note
    *   Time Complexity: O(1)
    */
  def top: A

  /** Returns the top element of this stack
    *
    * @note
    *   Time Complexity: O(1)
    */
  def topOption: Option[A]

  /** Returns the bottom element of this stack
    *
    * @throws java.util.NoSuchElementException
    *   if this stack is empty
    *
    * @note
    *   Time Complexity: O(1)
    */
  def bottom: A

  /** Returns the bottom element of this stack
    *
    * @note
    *   Time Complexity: O(1)
    */
  def bottomOption: Option[A]

  /** Returns the element at the given index
    *
    * @throws java.lang.IndexOutOfBoundsException
    *   if the index is out of bounds
    *
    * @note
    *   Time Complexity: O(1)
    */
  def apply(index: Int): A

  /** Removes all elements of this stack
    *
    * @note
    *   Time Complexity: O(N)
    */
  def clear(): MinMaxStack[A]

  /** Returns the ordering of this stack
    *
    * @note
    *   Time Complexity: O(1)
    */
  def ordering: Ordering[A]

  /** Returns the minimum element of this stack
    *
    * @throws java.util.NoSuchElementException
    *   if this stack is empty
    *
    * @note
    *   Time Complexity: O(1)
    */
  def min: A

  /** Returns the minimum element of this stack
    *
    * @note
    *   Time Complexity: O(1)
    */
  def minOption: Option[A]

  /** Returns the maximum element of this stack
    *
    * @throws java.util.NoSuchElementException
    *   if this stack is empty
    *
    * @note
    *   Time Complexity: O(1)
    */
  def max: A

  /** Returns the maximum element of this stack
    *
    * @note
    *   Time Complexity: O(1)
    */
  def maxOption: Option[A]

  /** Returns the minimum and maximum elements of this stack
    *
    * @throws java.util.NoSuchElementException
    *   if this stack is empty
    *
    * @note
    *   Time Complexity: O(1)
    */
  def minmax: (A, A)

  /** Returns the minimum and maximum elements of this stack
    *
    * @note
    *   Time Complexity: O(1)
    */
  def minmaxOption: Option[(A, A)]

  /** Returns true if this stack is empty
    *
    * @note
    *   Time Complexity: O(1)
    */
  def isEmpty: Boolean

  /** Returns true if this stack not empty
    *
    * @note
    *   Time Complexity: O(1)
    */
  def nonEmpty: Boolean

  /** Returns a reverse iterator of this stack
    *
    * @note
    *   Time Complexity: O(1)
    */
  def reverseIterator: Iterator[A]

  /** Builds a collection from this stack */
  def to[C](factory: Factory[A, C]): C

}

object MinMaxStack {

  /** Returns an empty stack
    *
    * @note
    *   Time Complexity: O(1)
    */
  def empty[A: Ordering]: MinMaxStack[A] =
    DefaultMinMaxStack.empty

  /** Creates a stack from elements of the given iterable
    *
    * @note
    *   Time Complexity: O(N)
    */
  def from[A: Ordering](iterable: IterableOnce[A]): MinMaxStack[A] = {
    val builder = newBuilder
    builder ++= iterable
    builder.result()
  }

  /** Creates a stack from the given elements
    *
    * @note
    *   Time Complexity: O(N)
    */
  def apply[A: Ordering](elems: A*): MinMaxStack[A] =
    from(elems)

  /** Creates a builder of [[MinMaxStack]] */
  def newBuilder[A: Ordering]: mutable.Builder[A, MinMaxStack[A]] =
    new mutable.Builder[A, MinMaxStack[A]]() {
      private val buffer: mutable.ArrayBuffer[A] = mutable.ArrayBuffer.empty
      override def clear(): Unit = buffer.clear()
      override def result(): MinMaxStack[A] = {
        val stack = MinMaxStack.empty
        stack.pushAll(buffer.reverseIterator)
      }
      override def addOne(elem: A): this.type = {
        buffer.addOne(elem)
        this
      }
    }

  import scala.language.implicitConversions
  implicit def toFactory[A: Ordering](
      self: this.type
  ): Factory[A, MinMaxStack[A]] =
    new Factory[A, MinMaxStack[A]] {
      override def fromSpecific(it: IterableOnce[A]): MinMaxStack[A] =
        self.from(it)
      override def newBuilder: mutable.Builder[A, MinMaxStack[A]] =
        self.newBuilder
    }

}
