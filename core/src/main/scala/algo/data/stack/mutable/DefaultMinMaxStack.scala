package algo.data.stack.mutable

import scala.collection.{Factory, mutable}

private final class DefaultMinMaxStack[A: Ordering] private (
    private val stack: mutable.Stack[DefaultMinMaxStack.Entry[A]]
) extends MinMaxStack[A] {
  import DefaultMinMaxStack.Entry

  override def size: Int = stack.size

  override def push(value: A): this.type = {
    if (stack.isEmpty) {
      stack.push(new Entry(value, value, value))
    } else {
      val minimum = Ordering[A].min(value, stack.top.minimum)
      val maximum = Ordering[A].max(value, stack.top.maximum)
      stack.push(new Entry(value, minimum, maximum))
    }
    this
  }

  override def push(value: A, values: A*): this.type = {
    push(value)
    values.foreach(push)
    this
  }

  override def pushAll(iterable: IterableOnce[A]): this.type = {
    iterable.iterator.foreach(push)
    this
  }

  override def pop(): A = stack.pop().value

  override def popAll(): IndexedSeq[A] = {
    val builder = IndexedSeq.newBuilder[A]
    builder.sizeHint(size)
    while (nonEmpty) {
      builder += pop()
    }
    builder.result()
  }

  override def popWhile(f: A => Boolean): IndexedSeq[A] = {
    val builder = IndexedSeq.newBuilder[A]
    while (nonEmpty && f(top)) {
      builder += pop()
    }
    builder.result()
  }

  override def top: A =
    topOption.getOrElse {
      throw new NoSuchElementException("The heap is empty.")
    }

  override def topOption: Option[A] =
    if (stack.isEmpty) None
    else Some(stack.top.value)

  override def bottom: A =
    bottomOption.getOrElse {
      throw new NoSuchElementException("The heap is empty.")
    }

  override def bottomOption: Option[A] =
    if (stack.isEmpty) None
    else Some(stack.last.value)

  override def apply(index: Int): A = {
    if (index < 0 || index >= size)
      throw new IndexOutOfBoundsException(
        s"Index out of range [0, $size): $index"
      )
    stack(index).value
  }

  override def min: A =
    minOption.getOrElse {
      throw new NoSuchElementException("The heap is empty.")
    }

  override def minOption: Option[A] =
    if (stack.isEmpty) None
    else Some(stack.top.minimum)

  override def max: A =
    maxOption.getOrElse {
      throw new NoSuchElementException("The heap is empty.")
    }

  override def maxOption: Option[A] =
    if (stack.isEmpty) None
    else Some(stack.top.maximum)

  override def minmax: (A, A) =
    minmaxOption.getOrElse {
      throw new NoSuchElementException("The heap is empty.")
    }

  override def minmaxOption: Option[(A, A)] = {
    if (stack.isEmpty) None
    else Some((stack.top.minimum, stack.top.maximum))
  }

  override def clear(): Unit = stack.clear()

  override def isEmpty: Boolean = stack.isEmpty

  override def nonEmpty: Boolean = stack.nonEmpty

  override def iterator: Iterator[A] = stack.iterator.map(_.value)

  override def knownSize: Int = stack.size

  override def reverseIterator: Iterator[A] = stack.reverseIterator.map(_.value)

  override def ordering: Ordering[A] = Ordering[A]

  override def to[C](factory: Factory[A, C]): C = factory.fromSpecific(iterator)

  override def clone(): MinMaxStack[A] = {
    new DefaultMinMaxStack[A](stack.clone())
  }

}

private object DefaultMinMaxStack {

  private final class Entry[A](val value: A, val minimum: A, val maximum: A)

  def empty[A: Ordering]: DefaultMinMaxStack[A] =
    new DefaultMinMaxStack[A](mutable.Stack.empty)

}
