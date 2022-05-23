package algo.data.stack.immutable

import cats.data.State

trait MinMaxStackSyntax {

  /** @see [[MinMaxStack.size]] */
  def size[V]: State[MinMaxStack[V], Int] =
    State.inspect { stack =>
      stack.size
    }

  /** @see [[MinMaxStack.push(value:A)*]] */
  def push[V](value: V): State[MinMaxStack[V], Unit] =
    State.modify { stack =>
      stack.push(value)
    }

  /** @see [[MinMaxStack.push(value:A,values:A*)*]] */
  def push[V](value: V, values: V*): State[MinMaxStack[V], Unit] =
    State.modify { stack =>
      stack.push(value, values*)
    }

  /** @see [[MinMaxStack.pushAll]] */
  def pushAll[V](iterable: IterableOnce[V]): State[MinMaxStack[V], Unit] =
    State.modify { stack =>
      stack.pushAll(iterable)
    }

  /** @see [[MinMaxStack.pop]] */
  def pop[V]: State[MinMaxStack[V], V] =
    State { stack =>
      stack.pop().swap
    }

  /** @see [[MinMaxStack.popAll]] */
  def popAll[V]: State[MinMaxStack[V], IndexedSeq[V]] =
    State { stack =>
      stack.popAll().swap
    }

  /** @see [[MinMaxStack.popWhile]] */
  def popWhile[V](f: V => Boolean): State[MinMaxStack[V], IndexedSeq[V]] =
    State { stack =>
      stack.popWhile(f).swap
    }

  /** @see [[MinMaxStack.top]] */
  def top[V]: State[MinMaxStack[V], V] =
    State.inspect { stack =>
      stack.top
    }

  /** @see [[MinMaxStack.topOption]] */
  def topOption[V]: State[MinMaxStack[V], Option[V]] =
    State.inspect { stack =>
      stack.topOption
    }

  /** @see [[MinMaxStack.bottom]] */
  def bottom[V]: State[MinMaxStack[V], V] =
    State.inspect { stack =>
      stack.bottom
    }

  /** @see [[MinMaxStack.bottomOption]] */
  def bottomOption[V]: State[MinMaxStack[V], Option[V]] =
    State.inspect { stack =>
      stack.bottomOption
    }

  /** @see [[MinMaxStack.clear]] */
  def clear[V]: State[MinMaxStack[V], Unit] =
    State.modify { stack =>
      stack.clear()
    }

  /** @see [[MinMaxStack.ordering]] */
  def ordering[V]: State[MinMaxStack[V], Ordering[V]] =
    State.inspect { stack =>
      stack.ordering
    }

  /** @see [[MinMaxStack.min]] */
  def min[V]: State[MinMaxStack[V], V] =
    State.inspect { stack =>
      stack.min
    }

  /** @see [[MinMaxStack.minOption]] */
  def minOption[V]: State[MinMaxStack[V], Option[V]] =
    State.inspect { stack =>
      stack.minOption
    }

  /** @see [[MinMaxStack.max]] */
  def max[V]: State[MinMaxStack[V], V] =
    State.inspect { stack =>
      stack.max
    }

  /** @see [[MinMaxStack.maxOption]] */
  def maxOption[V]: State[MinMaxStack[V], Option[V]] =
    State.inspect { stack =>
      stack.maxOption
    }

  /** @see [[MinMaxStack.minmax]] */
  def minmax[V]: State[MinMaxStack[V], (V, V)] =
    State.inspect { stack =>
      stack.minmax
    }

  /** @see [[MinMaxStack.minmaxOption]] */
  def minmaxOption[V]: State[MinMaxStack[V], Option[(V, V)]] =
    State.inspect { stack =>
      stack.minmaxOption
    }

  /** @see [[MinMaxStack.isEmpty]] */
  def isEmpty[V]: State[MinMaxStack[V], Boolean] =
    State.inspect { stack =>
      stack.isEmpty
    }

  /** @see [[MinMaxStack.nonEmpty]] */
  def nonEmpty[V]: State[MinMaxStack[V], Boolean] =
    State.inspect { stack =>
      stack.nonEmpty
    }

  /** @see [[MinMaxStack.reverseIterator]] */
  def reverseIterator[V]: State[MinMaxStack[V], Iterator[V]] =
    State.inspect { stack =>
      stack.reverseIterator
    }

  /** @see [[MinMaxStack.iterator]] */
  def iterator[V]: State[MinMaxStack[V], Iterator[V]] =
    State.inspect { stack =>
      stack.iterator
    }

  /** @see [[MinMaxStack.knownSize]] */
  def knownSize[V]: State[MinMaxStack[V], Int] =
    State.inspect { stack =>
      stack.knownSize
    }

}
