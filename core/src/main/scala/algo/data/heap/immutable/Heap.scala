package algo.data.heap.immutable

import scala.collection.{Factory, SortedIterableFactory, mutable}

/** D-way Heap
  *
  * @see
  *   [[algo.data.heap.mutable.Heap]]
  */
trait Heap[A] {

  /** Returns the size of this heap
    *
    * @note
    *   Time Complexity: O(1)
    */
  def size: Int

  /** Returns true if this heap is empty
    *
    * @note
    *   Time Complexity: O(1)
    */
  def isEmpty: Boolean

  /** Returns true if this heap is not empty
    *
    * @note
    *   Time Complexity: O(1)
    */
  def nonEmpty: Boolean

  /** Returns the ordering of this heap
    *
    * @note
    *   Time Complexity: O(1)
    */
  def ordering: Ordering[A]

  /** Returns the branching factor of this heap
    *
    * @note
    *   Time Complexity: O(1)
    */
  def branchingFactor: Int

  /** Returns the element with the highest priority of this heap
    *
    * @throws java.util.NoSuchElementException
    *   if this heap is empty
    * @note
    *   Time Complexity: O(1)
    */
  def top: A

  /** Returns the element with the highest priority of this heap
    *
    * If this heap is empty, returns [[scala.None]].
    *
    * @note
    *   Time Complexity: O(1)
    */
  def topOption: Option[A]

  /** Removes and returns the element with the highest priority from this heap
    *
    * @throws java.util.NoSuchElementException
    *   if this heap is empty
    *
    * @note
    *   Time Complexity: O(log n) amortized
    */
  def pop(): (A, Heap[A])

  /** Removes and returns all elements in priority order from this heap
    *
    * @note
    *   Time Complexity: O(n log n)
    */
  def popAll(): (Seq[A], Heap[A])

  /** Inserts the given value into this heap
    *
    * If the given value already exists in this heap, the element will be
    * inserted again. The existing one will be removed.
    *
    * @note
    *   Time Complexity: O(log n) amortized
    */
  def push(value: A): Heap[A]

  /** Returns true if this heap contains the given value
    *
    * @note
    *   Time Complexity: O(1)
    */
  def contains(value: A): Boolean

  /** Removes the give value from this heap
    *
    * If the given value is not in this heap, this method does nothing.
    *
    * @note
    *   Time Complexity: O(log n) amortized
    */
  def remove(value: A): Heap[A]

  /** Removes the old value from this heap and inserts the new value into this
    * heap
    *
    * `update(oldValue,newValue)` is equivalent to the following calls but more
    * efficient:
    * {{{
    * remove(oldValue)
    * push(newValue)
    * }}}
    *
    * If the old value is not in this heap, this method skips the removal. If
    * the new value already exists in this heap, this method inserts the new
    * element again and removes the existing one.
    *
    * @note
    *   Time Complexity: O(log n) amortized
    */
  def update(oldValue: A, newValue: A): Heap[A]

  /** Removes all elements from this heap
    *
    * @note
    *   Time Complexity: O(1)
    */
  def clear(): Heap[A]

  /** Returns the reverse of this heap
    *
    * The reversed heap has the same elements of this heap, but the reverse
    * ordering.
    *
    * @note
    *   Time Complexity: O(n)
    */
  def reverse: Heap[A]

  /** Returns an iterator of this heap
    * @note
    *   Time Complexity: O(1)
    */
  def iterator: Iterator[A]

  /** Build a collection from this heap */
  def to[C](factory: Factory[A, C]): C

}

/** Building a [[Heap]] (D-way heap) */
object Heap extends SortedIterableFactory[Heap] {
  private val DefaultBranchingFactor: Int = 4
  private val DefaultHeapFactory: HeapFactory =
    new HeapFactory(DefaultBranchingFactor)

  override def from[A: Ordering](iterable: IterableOnce[A]): Heap[A] =
    DefaultHeapFactory.from(iterable)

  override def empty[A: Ordering]: Heap[A] =
    DefaultHeapFactory.empty

  override def newBuilder[A: Ordering]: mutable.Builder[A, Heap[A]] =
    DefaultHeapFactory.newBuilder

  override def apply[A: Ordering](xs: A*): Heap[A] =
    DefaultHeapFactory.apply(xs*)

  override def fill[A: Ordering](n: Int)(elem: => A): Heap[A] =
    DefaultHeapFactory.fill(n)(elem)

  override def tabulate[A: Ordering](n: Int)(f: Int => A): Heap[A] =
    DefaultHeapFactory.tabulate(n)(f)

  override def iterate[A: Ordering](start: A, len: Int)(f: A => A): Heap[A] =
    DefaultHeapFactory.iterate(start, len)(f)

  override def unfold[A: Ordering, S](init: S)(
      f: S => Option[(A, S)]
  ): Heap[A] =
    DefaultHeapFactory.unfold(init)(f)

  /** Creates a [[HeapFactory]] with the given branching factor
    *
    * @throws java.lang.IllegalArgumentException
    *   if the branching factor is not greater than 1
    */
  def withBranchingFactor(branchingFactor: Int): HeapFactory = {
    require(branchingFactor > 1, "branchingFactor must be greater than 1.")
    new HeapFactory(branchingFactor)
  }

}
