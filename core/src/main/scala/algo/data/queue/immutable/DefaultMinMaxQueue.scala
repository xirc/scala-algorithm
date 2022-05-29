package algo.data.queue.immutable

import algo.data.stack.immutable
import cats.Semigroupal
import cats.kernel.Semigroup
import cats.syntax.semigroup.*

import scala.collection.Factory

private final class DefaultMinMaxQueue[A: Ordering](
    inStack: immutable.MinMaxStack[A],
    outStack: immutable.MinMaxStack[A]
) extends MinMaxQueue[A] {

  private val minSemigroup: Semigroup[A] = Semigroup.instance(Ordering[A].min)
  private val maxSemigroup: Semigroup[A] = Semigroup.instance(Ordering[A].max)

  override def size: Int =
    inStack.size + outStack.size

  override def enqueue(value: A): MinMaxQueue[A] = {
    val newInStack = inStack.push(value)
    new DefaultMinMaxQueue(newInStack, outStack)
  }

  override def enqueue(value: A, values: A*): MinMaxQueue[A] = {
    val newInStack = inStack.push(value, values*)
    new DefaultMinMaxQueue(newInStack, outStack)
  }

  override def enqueueAll(values: IterableOnce[A]): MinMaxQueue[A] = {
    val newInStack = inStack.pushAll(values)
    new DefaultMinMaxQueue(newInStack, outStack)
  }

  override def dequeue(): (A, MinMaxQueue[A]) = {
    if (outStack.isEmpty) {
      val (values, newInStack) = inStack.popAll()
      val newOutStackContainingValue = outStack.pushAll(values)
      if (newOutStackContainingValue.isEmpty) {
        throw new NoSuchElementException("The queue is empty.")
      }
      val (value, newOutStack) = newOutStackContainingValue.pop()
      (value, new DefaultMinMaxQueue(newInStack, newOutStack))
    } else {
      val (value, newOutStack) = outStack.pop()
      (value, new DefaultMinMaxQueue(inStack, newOutStack))
    }
  }

  override def dequeueAll(): (IndexedSeq[A], MinMaxQueue[A]) = {
    val builder = IndexedSeq.newBuilder[A]
    builder.sizeHint(size)
    builder.addAll(outStack.iterator)
    builder.addAll(inStack.reverseIterator)
    (builder.result(), DefaultMinMaxQueue.empty)
  }

  override def dequeueWhile(
      f: A => Boolean
  ): (IndexedSeq[A], MinMaxQueue[A]) = {
    val builder = IndexedSeq.newBuilder[A]
    var newOutStack = outStack
    var newInStack = inStack
    newOutStack = {
      val (values, stack) = newOutStack.popWhile(f)
      builder.addAll(values)
      stack
    }
    if (newOutStack.isEmpty) {
      newOutStack = newOutStack.pushAll(newInStack.iterator)
      newInStack = newInStack.clear()
    }
    newOutStack = {
      val (values, stack) = newOutStack.popWhile(f)
      builder.addAll(values)
      stack
    }
    (builder.result(), new DefaultMinMaxQueue(newInStack, newOutStack))
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

  override def clear(): MinMaxQueue[A] =
    DefaultMinMaxQueue.empty

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

private object DefaultMinMaxQueue {

  def empty[A: Ordering]: DefaultMinMaxQueue[A] =
    new DefaultMinMaxQueue(
      immutable.MinMaxStack.empty,
      immutable.MinMaxStack.empty
    )

}
