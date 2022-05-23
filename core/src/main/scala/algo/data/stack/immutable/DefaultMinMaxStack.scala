package algo.data.stack.immutable
import scala.collection.Factory

private final class DefaultMinMaxStack[A: Ordering] private (
    private val buffer: Vector[DefaultMinMaxStack.Entry[A]]
) extends MinMaxStack[A] {
  import DefaultMinMaxStack.Entry

  override def size: Int = buffer.size

  override def push(value: A): MinMaxStack[A] = {
    val newBuffer = push(buffer, value)
    new DefaultMinMaxStack(newBuffer)
  }

  override def push(value: A, values: A*): MinMaxStack[A] = {
    var newBuffer = push(buffer, value)
    for (value <- values) {
      newBuffer = push(newBuffer, value)
    }
    new DefaultMinMaxStack(newBuffer)
  }

  override def pushAll(iterable: IterableOnce[A]): MinMaxStack[A] = {
    val newBuffer = iterable.iterator.foldLeft(buffer) { case (buf, value) =>
      push(buf, value)
    }
    new DefaultMinMaxStack(newBuffer)
  }

  override def pop(): (A, MinMaxStack[A]) = {
    if (buffer.isEmpty) {
      throw new NoSuchElementException("The stack is empty.")
    }
    val value = buffer.last.value
    val newBuffer = buffer.init
    (value, new DefaultMinMaxStack(newBuffer))
  }

  override def popAll(): (IndexedSeq[A], MinMaxStack[A]) = {
    val values = buffer.reverseIterator.map(_.value).toIndexedSeq
    (values, new DefaultMinMaxStack(Vector.empty))
  }

  override def popWhile(f: A => Boolean): (IndexedSeq[A], MinMaxStack[A]) = {
    val builder = IndexedSeq.newBuilder[A]
    var newBuffer = buffer
    while (newBuffer.nonEmpty && f(newBuffer.last.value)) {
      builder += newBuffer.last.value
      newBuffer = newBuffer.init
    }
    val values = builder.result()
    (values, new DefaultMinMaxStack(newBuffer))
  }

  override def top: A =
    topOption.getOrElse {
      throw new NoSuchElementException("The stack is empty.")
    }

  override def topOption: Option[A] =
    buffer.lastOption.map(_.value)

  override def bottom: A =
    bottomOption.getOrElse {
      throw new NoSuchElementException("The stack is empty.")
    }

  override def bottomOption: Option[A] =
    buffer.headOption.map(_.value)

  override def apply(index: Int): A = {
    if (index < 0 || index >= buffer.size) {
      throw new IndexOutOfBoundsException(
        s"Index out of range [0, $size): $index"
      )
    }
    buffer(buffer.size - 1 - index).value
  }

  override def clear(): MinMaxStack[A] =
    new DefaultMinMaxStack(Vector.empty)

  override def ordering: Ordering[A] = Ordering[A]

  override def min: A =
    minOption.getOrElse {
      throw new NoSuchElementException("The stack is empty.")
    }

  override def minOption: Option[A] =
    buffer.lastOption.map(_.minimum)

  override def max: A =
    maxOption.getOrElse {
      throw new NoSuchElementException("The stack is empty.")
    }

  override def maxOption: Option[A] =
    buffer.lastOption.map(_.maximum)

  override def minmax: (A, A) =
    minmaxOption.getOrElse {
      throw new NoSuchElementException("The stack is empty.")
    }

  override def minmaxOption: Option[(A, A)] =
    buffer.lastOption.map { e => (e.minimum, e.maximum) }

  override def isEmpty: Boolean = buffer.isEmpty

  override def nonEmpty: Boolean = buffer.nonEmpty

  override def reverseIterator: Iterator[A] =
    buffer.iterator.map(_.value)

  override def to[C](factory: Factory[A, C]): C =
    factory.fromSpecific(iterator)

  override def iterator: Iterator[A] =
    buffer.reverseIterator.map(_.value)

  override def knownSize: Int = buffer.size

  @inline private def push(
      buffer: Vector[Entry[A]],
      value: A
  ): Vector[Entry[A]] = {
    val newEntry = if (buffer.isEmpty) {
      new Entry(value, value, value)
    } else {
      val minimum = Ordering[A].min(value, buffer.last.minimum)
      val maximum = Ordering[A].max(value, buffer.last.maximum)
      new Entry(value, minimum, maximum)
    }
    buffer :+ newEntry
  }

}

private object DefaultMinMaxStack {

  private final class Entry[A](val value: A, val minimum: A, val maximum: A)

  def empty[A: Ordering]: MinMaxStack[A] =
    new DefaultMinMaxStack(Vector.empty)

}
