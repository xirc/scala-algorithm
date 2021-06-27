package algo.data.stack.mutable

import scala.collection.{Factory, mutable}

private final class DefaultMinMaxStack[A: Ordering]() extends MinMaxStack[A] {

  case class Entry(value: A, minimum: A, maximum: A)

  private val stack: mutable.Stack[Entry] =
    mutable.Stack.empty

  override def size: Int = stack.size

  override def push(value: A): this.type = {
    if (stack.isEmpty) {
      stack.push(Entry(value, value, value))
    } else {
      val minimum = Ordering[A].min(value, stack.top.minimum)
      val maximum = Ordering[A].max(value, stack.top.maximum)
      stack.push(Entry(value, minimum, maximum))
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

  override def popAll(): Seq[A] = {
    val builder = Seq.newBuilder[A]
    builder.sizeHint(size)
    while (nonEmpty) {
      builder += pop()
    }
    builder.result()
  }

  override def popWhile(f: A => Boolean): Seq[A] = {
    val builder = Seq.newBuilder[A]
    while (nonEmpty && f(top)) {
      builder += pop()
    }
    builder.result()
  }

  override def top: A =
    topOption.getOrElse {
      throw new NoSuchElementException("empty.top")
    }

  override def topOption: Option[A] =
    if (stack.isEmpty) None
    else Some(stack.top.value)

  override def bottom: A =
    bottomOption.getOrElse {
      throw new NoSuchElementException("empty.bottom")
    }

  override def bottomOption: Option[A] =
    if (stack.isEmpty) None
    else Some(stack.last.value)

  override def apply(index: Int): A = {
    if (index < 0 || index >= size)
      throw new IndexOutOfBoundsException(
        s"Index out of range [0, $size): $index"
      )
    stack.apply(index).value
  }

  override def min: A =
    minOption.getOrElse {
      throw new UnsupportedOperationException("empty.min")
    }

  override def minOption: Option[A] =
    if (stack.isEmpty) None
    else Some(stack.top.minimum)

  override def max: A =
    maxOption.getOrElse {
      throw new UnsupportedOperationException("empty.max")
    }

  override def maxOption: Option[A] =
    if (stack.isEmpty) None
    else Some(stack.top.maximum)

  override def minmax: (A, A) =
    minmaxOption.getOrElse {
      throw new UnsupportedOperationException("empty.minmax")
    }

  override def minmaxOption: Option[(A, A)] = {
    if (stack.isEmpty) None
    else Some((stack.top.minimum, stack.top.maximum))
  }

  override def clear(): Unit = stack.clear()

  override def isEmpty: Boolean = stack.isEmpty

  override def nonEmpty: Boolean = stack.nonEmpty

  override def iterator: Iterator[A] = stack.iterator.map(_.value)

  override def reverseIterator: Iterator[A] = stack.reverseIterator.map(_.value)

  override def ordering: Ordering[A] = Ordering[A]

  override def to[C](factory: Factory[A, C]): C = factory.fromSpecific(iterator)

}
