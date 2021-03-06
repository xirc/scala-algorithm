package algo.data.queue.mutable

import algo.data.stack.mutable
import cats.Semigroupal
import cats.kernel.Semigroup
import cats.syntax.semigroup.*

import scala.collection.Factory

private final class DefaultMinMaxQueue[A: Ordering] extends MinMaxQueue[A] {

  private val inStack: mutable.MinMaxStack[A] = mutable.MinMaxStack.empty
  private val outStack: mutable.MinMaxStack[A] = mutable.MinMaxStack.empty
  private val minSemigroup: Semigroup[A] = Semigroup.instance(Ordering[A].min)
  private val maxSemigroup: Semigroup[A] = Semigroup.instance(Ordering[A].max)

  override def size: Int =
    inStack.size + outStack.size

  override def enqueue(value: A): this.type = {
    inStack.push(value)
    this
  }

  override def enqueue(value: A, values: A*): this.type = {
    inStack.push(value, values*)
    this
  }

  override def enqueueAll(values: IterableOnce[A]): this.type = {
    inStack.pushAll(values)
    this
  }

  override def dequeue(): A = {
    if (outStack.isEmpty) {
      outStack.pushAll(inStack.popAll())
    }
    if (outStack.isEmpty) {
      throw new NoSuchElementException("The queue is empty.")
    }
    outStack.pop()
  }

  override def dequeueAll(): IndexedSeq[A] = {
    val builder = IndexedSeq.newBuilder[A]
    builder.sizeHint(size)
    builder.addAll(outStack.iterator)
    builder.addAll(inStack.reverseIterator)
    outStack.clear()
    inStack.clear()
    builder.result()
  }

  override def dequeueWhile(f: A => Boolean): IndexedSeq[A] = {
    val builder = IndexedSeq.newBuilder[A]
    while (nonEmpty && f(front)) {
      builder += dequeue()
    }
    builder.result()
  }

  override def front: A =
    frontOption.getOrElse {
      throw new NoSuchElementException("The queue is empty.")
    }

  override def frontOption: Option[A] =
    if (outStack.isEmpty) inStack.bottomOption
    else outStack.topOption

  override def back: A =
    backOption.getOrElse {
      throw new NoSuchElementException("The queue is empty.")
    }

  override def backOption: Option[A] =
    if (inStack.isEmpty) outStack.bottomOption
    else inStack.topOption

  override def apply(index: Int): A = {
    if (index < 0 || index >= size)
      throw new IndexOutOfBoundsException(
        s"Index out of range [0, $size): $index"
      )
    if (index < outStack.size) outStack(index)
    else inStack(inStack.size - 1 - (index - outStack.size))
  }

  override def clear(): Unit = {
    inStack.clear()
    outStack.clear()
  }

  override def ordering: Ordering[A] = Ordering[A]

  override def min: A =
    minOption.getOrElse {
      throw new NoSuchElementException("The queue is empty.")
    }

  override def minOption: Option[A] = {
    implicit val m: Semigroup[A] = minSemigroup
    inStack.minOption |+| outStack.minOption
  }

  override def max: A =
    maxOption.getOrElse {
      throw new NoSuchElementException("The queue is empty.")
    }

  override def maxOption: Option[A] = {
    implicit val m: Semigroup[A] = maxSemigroup
    inStack.maxOption |+| outStack.maxOption
  }

  override def minmax: (A, A) =
    minmaxOption.getOrElse {
      throw new NoSuchElementException("The queue is empty.")
    }

  override def minmaxOption: Option[(A, A)] =
    Semigroupal.tuple2(minOption, maxOption)

  override def isEmpty: Boolean =
    inStack.isEmpty && outStack.isEmpty

  override def nonEmpty: Boolean =
    inStack.nonEmpty || outStack.nonEmpty

  override def iterator: Iterator[A] =
    outStack.iterator ++ inStack.reverseIterator

  override def knownSize: Int = size

  override def reverseIterator: Iterator[A] =
    inStack.iterator ++ outStack.reverseIterator

  override def to[C](factory: Factory[A, C]): C =
    factory.fromSpecific(iterator)

}
