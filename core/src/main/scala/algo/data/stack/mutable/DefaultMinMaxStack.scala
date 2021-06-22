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

  override def pop(): A = stack.pop().value

  override def top: A =
    topOption.getOrElse {
      throw new NoSuchElementException("empty.top")
    }

  override def topOption: Option[A] =
    if (stack.isEmpty) None
    else Some(stack.top.value)

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

  override def ordering: Ordering[A] = Ordering[A]

  override def to[C](factory: Factory[A, C]): C = factory.fromSpecific(iterator)

}
