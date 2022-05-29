package algo.data.queue.immutable

import cats.data.State

trait MinMaxQueueSyntax {

  /** @see [[MinMaxQueue.size]] */
  def size[V]: State[MinMaxQueue[V], Int] =
    State.inspect { stack =>
      stack.size
    }

  /** @see [[MinMaxQueue.enqueue(value:A)*]] */
  def enqueue[V](value: V): State[MinMaxQueue[V], Unit] =
    State.modify { stack =>
      stack.enqueue(value)
    }

  /** @see [[MinMaxQueue.enqueue(value:A,values:A*)*]] */
  def enqueue[V](value: V, values: V*): State[MinMaxQueue[V], Unit] =
    State.modify { stack =>
      stack.enqueue(value, values*)
    }

  /** @see [[MinMaxQueue.enqueueAll]] */
  def enqueueAll[V](values: IterableOnce[V]): State[MinMaxQueue[V], Unit] =
    State.modify { stack =>
      stack.enqueueAll(values)
    }

  /** @see [[MinMaxQueue.dequeue]] */
  def dequeue[V]: State[MinMaxQueue[V], V] =
    State { stack =>
      stack.dequeue().swap
    }

  /** @see [[MinMaxQueue.dequeueAll]] */
  def dequeueAll[V]: State[MinMaxQueue[V], IndexedSeq[V]] =
    State { stack =>
      stack.dequeueAll().swap
    }

  /** @see [[MinMaxQueue.dequeueWhile]] */
  def dequeueWhile[V](f: V => Boolean): State[MinMaxQueue[V], IndexedSeq[V]] =
    State { stack =>
      stack.dequeueWhile(f).swap
    }

  /** @see [[MinMaxQueue.front]] */
  def front[V]: State[MinMaxQueue[V], V] =
    State.inspect { stack =>
      stack.front
    }

  /** @see [[MinMaxQueue.frontOption]] */
  def frontOption[V]: State[MinMaxQueue[V], Option[V]] =
    State.inspect { stack =>
      stack.frontOption
    }

  /** @see [[MinMaxQueue.back]] */
  def back[V]: State[MinMaxQueue[V], V] =
    State.inspect { stack =>
      stack.back
    }

  /** @see [[MinMaxQueue.backOption]] */
  def backOption[V]: State[MinMaxQueue[V], Option[V]] =
    State.inspect { stack =>
      stack.backOption
    }

  /** @see [[MinMaxQueue.clear]] */
  def clear[V]: State[MinMaxQueue[V], Unit] =
    State.modify { stack =>
      stack.clear()
    }

  /** @see [[MinMaxQueue.ordering]] */
  def ordering[V]: State[MinMaxQueue[V], Ordering[V]] =
    State.inspect { stack =>
      stack.ordering
    }

  /** @see [[MinMaxQueue.min]] */
  def min[V]: State[MinMaxQueue[V], V] =
    State.inspect { stack =>
      stack.min
    }

  /** @see [[MinMaxQueue.minOption]] */
  def minOption[V]: State[MinMaxQueue[V], Option[V]] =
    State.inspect { stack =>
      stack.minOption
    }

  /** @see [[MinMaxQueue.max]] */
  def max[V]: State[MinMaxQueue[V], V] =
    State.inspect { stack =>
      stack.max
    }

  /** @see [[MinMaxQueue.maxOption]] */
  def maxOption[V]: State[MinMaxQueue[V], Option[V]] =
    State.inspect { stack =>
      stack.maxOption
    }

  /** @see [[MinMaxQueue.minmax]] */
  def minmax[V]: State[MinMaxQueue[V], (V, V)] =
    State.inspect { stack =>
      stack.minmax
    }

  /** @see [[MinMaxQueue.minmaxOption]] */
  def minmaxOption[V]: State[MinMaxQueue[V], Option[(V, V)]] =
    State.inspect { stack =>
      stack.minmaxOption
    }

  /** @see [[MinMaxQueue.isEmpty]] */
  def isEmpty[V]: State[MinMaxQueue[V], Boolean] =
    State.inspect { stack =>
      stack.isEmpty
    }

  /** @see [[MinMaxQueue.nonEmpty]] */
  def nonEmpty[V]: State[MinMaxQueue[V], Boolean] =
    State.inspect { stack =>
      stack.nonEmpty
    }

  /** @see [[MinMaxQueue.iterator]] */
  def iterator[V]: State[MinMaxQueue[V], Iterator[V]] =
    State.inspect { stack =>
      stack.iterator
    }

  /** @see [[MinMaxQueue.knownSize]] */
  def knownSize[V]: State[MinMaxQueue[V], Int] =
    State.inspect { stack =>
      stack.knownSize
    }

  /** @see [[MinMaxQueue.reverseIterator]] */
  def reverseIterator[V]: State[MinMaxQueue[V], Iterator[V]] =
    State.inspect { stack =>
      stack.reverseIterator
    }

}
