package algo.data.heap.mutable

import scala.collection.{Factory, SortedIterableFactory, View, mutable}

/** Builds a [[Heap]] with the given branching factor
  *
  * This class extends [[scala.AnyVal]] to avoid object allocation when using
  * [[Heap.withBranchingFactor]].
  */
final class HeapFactory(val branchingFactor: Int) extends AnyVal {

  /** Creates a [[Heap]] containing elements of the given iterable
    * @note
    *   Time Complexity: O(n)
    */
  def from[A: Ordering](iterable: IterableOnce[A]): Heap[A] = {
    DefaultHeap.from(branchingFactor, iterable)
  }

  /** Creates an empty [[Heap]]
    * @note
    *   Time Complexity: O(1)
    */
  def empty[A: Ordering]: Heap[A] = {
    DefaultHeap.from(branchingFactor, View.empty[A])
  }

  /** Creates a [[Heap]] containing the given elements
    * @note
    *   Time Complexity: O(n)
    */
  def apply[A: Ordering](xs: A*): Heap[A] =
    from(xs)

  /** @see
    *   [[scala.collection.SortedIterableFactory.fill]]
    * @note
    *   Time Complexity: O(n)
    */
  def fill[A: Ordering](n: Int)(elem: => A): Heap[A] =
    from(new View.Fill(n)(elem))

  /** @see
    *   [[scala.collection.SortedIterableFactory.tabulate]]
    * @note
    *   Time Complexity: O(n)
    */
  def tabulate[A: Ordering](n: Int)(f: Int => A): Heap[A] =
    from(new View.Tabulate(n)(f))

  /** @see
    *   [[scala.collection.SortedIterableFactory.iterate]]
    * @note
    *   Time Complexity: O(n)
    */
  def iterate[A: Ordering](start: A, len: Int)(f: A => A): Heap[A] =
    from(new View.Iterate(start, len)(f))

  /** @see
    *   [[scala.collection.SortedIterableFactory.unfold]]
    * @note
    *   Time Complexity: O(n)
    */
  def unfold[A: Ordering, S](init: S)(
      f: S => Option[(A, S)]
  ): Heap[A] =
    from(new View.Unfold(init)(f))

  /** Creates a [[Heap]] builder */
  def newBuilder[A: Ordering]: mutable.Builder[A, Heap[A]] = {
    new mutable.Builder[A, Heap[A]]() {
      val buffer: mutable.ArrayBuffer[A] = mutable.ArrayBuffer.empty
      override def clear(): Unit = buffer.clear()
      override def result(): Heap[A] = {
        DefaultHeap.from(branchingFactor, buffer)
      }
      override def addOne(elem: A): this.type = {
        buffer.addOne(elem)
        this
      }
    }
  }

}

object HeapFactory {

  import scala.language.implicitConversions

  /** Implicit Conversion from [[HeapFactory]] to [[scala.collection.Factory]]
    */
  implicit def toFactory[A: Ordering](
      factory: HeapFactory
  ): Factory[A, Heap[A]] = {
    new SortedIterableFactory[Heap] {
      override def from[B: Ordering](iterable: IterableOnce[B]): Heap[B] = {
        factory.from(iterable)
      }
      override def empty[B: Ordering]: Heap[B] =
        factory.empty
      override def newBuilder[B: Ordering]: mutable.Builder[B, Heap[B]] =
        factory.newBuilder
    }
  }

}
