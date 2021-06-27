package algo.data.stack.mutable

import scala.collection.{Factory, mutable}

/** A data structure that allows to store and retrieve elements in a last-in-first-out (LIFO) fashion.
  * This data structure also allows to retrieval of the minimum and maximum value efficiently.
  */
trait MinMaxStack[A] {

  /** The size of this stack
    * @note Time Complexity: O(1)
    */
  def size: Int

  /** Add the element to the top of this stack
    * @note Time Complexity: O(1)
    */
  def push(value: A): this.type

  /** Push the elements onto this stack */
  def push(value: A, values: A*): this.type

  /** Push all elements in the given iterable onto this stack */
  def pushAll(iterable: IterableOnce[A]): this.type

  /** Remove the top element from this stack and return the element
    * @note Time Complexity: O(1)
    */
  @throws(classOf[NoSuchElementException])
  def pop(): A

  /** Pop all elements from this stack and return the elements
    * @note Time Complexity: O(N)
    */
  def popAll(): Seq[A]

  /** Return the top element of this stack
    * @note Time Complexity: O(1)
    */
  @throws(classOf[NoSuchElementException])
  def top: A

  /** Return the top element of this stack
    * @note Time Complexity: O(1)
    */
  def topOption: Option[A]

  /** Return the bottom element of this stack
    * @note Time Complexity: O(1)
    */
  @throws(classOf[NoSuchElementException])
  def bottom: A

  /** Return the bottom element of this stack
    * @note Time Complexity: O(1)
    */
  def bottomOption: Option[A]

  /** Get the element at the specified index
    * @note Time Complexity: O(1)
    */
  @throws(classOf[IndexOutOfBoundsException])
  def apply(index: Int): A

  /** Remove the all elements of this stack
    * @note Time Complexity: O(N)
    */
  def clear(): Unit

  /** Return the ordering of this stack
    * @note Time Complexity: O(1)
    */
  def ordering: Ordering[A]

  /** Return the minimum element of this stack
    * @note Time Complexity: O(1)
    */
  @throws(classOf[UnsupportedOperationException])
  def min: A

  /** Return the minimum element of this stack
    * @note Time Complexity: O(1)
    */
  def minOption: Option[A]

  /** Return the maximum element of this stack
    * @note Time Complexity: O(1)
    */
  @throws(classOf[UnsupportedOperationException])
  def max: A

  /** Return the maximum element of this stack
    * @note Time Complexity: O(1)
    */
  def maxOption: Option[A]

  /** Return the minimum and maximum elements of this stack
    * @note Time Complexity: O(1)
    */
  @throws(classOf[UnsupportedOperationException])
  def minmax: (A, A)

  /** Return the minimum and maximum elements of this stack
    * @note Time Complexity: O(1)
    */
  def minmaxOption: Option[(A, A)]

  /** Return true if this stack is empty
    * @note Time Complexity: O(1)
    */
  def isEmpty: Boolean

  /** Return true if this stack is not empty
    * @note Time Complexity: O(1)
    */
  def nonEmpty: Boolean

  /** Return an iterator of this stack
    * @note Time Complexity: O(1)
    */
  def iterator: Iterator[A]

  /** Return a reverse iterator of this stack
    * @note Time Complexity: O(1)
    */
  def reverseIterator: Iterator[A]

  /** Build a collection from this stack */
  def to[C](factory: Factory[A, C]): C

}

object MinMaxStack {

  /** An empty stack
    * @note Time Complexity: O(1)
    */
  def empty[A: Ordering]: MinMaxStack[A] =
    new DefaultMinMaxStack[A]()

  /** Create a stack from the given collection
    * @note Time Complexity: O(N)
    */
  def from[A: Ordering](iterable: IterableOnce[A]): MinMaxStack[A] = {
    val builder = newBuilder
    builder ++= iterable
    builder.result()
  }

  /** Create a stack from the given elements
    * @note Time Complexity: O(N)
    */
  def apply[A: Ordering](elems: A*): MinMaxStack[A] =
    from(elems)

  /** Create a builder for [[MinMaxStack]] */
  def newBuilder[A: Ordering]: mutable.Builder[A, MinMaxStack[A]] =
    new mutable.Builder[A, MinMaxStack[A]]() {
      val buffer: mutable.ArrayBuffer[A] = mutable.ArrayBuffer.empty
      override def clear(): Unit = buffer.clear()
      override def result(): MinMaxStack[A] = {
        val stack = MinMaxStack.empty
        buffer.reverseIterator.foreach(stack.push)
        stack
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
